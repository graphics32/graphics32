unit UnitMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  GR32,
  GR32_Image,
  UPSD_Storage;

type
  TFormMain = class(TForm)
    BSave: TButton;
    Panel1: TPanel;
    BOpen: TButton;
    ImgView: TImgView32;
    procedure BSaveClick(Sender: TObject);
    procedure BOpenClick(Sender: TObject);
  private

  public

  end;

var
  FormMain: TFormMain;

implementation
{$R *.dfm}
uses
  GR32.ImageFormats.JPG,
  GR32.ImageFormats.PNG;

type
  TPsdBitmap32Layer = class(TCustomPsdLayer)
  private
    FBitmap: TCustomBitmap32;
    procedure SetBitmap(const Value: TCustomBitmap32);
    function GetSourceRect: TRect;
    procedure SetSourceRect(const Value: TRect);
  protected
    FSourceTopLeft:TPoint;
    procedure GetChannelScanLine(AChannel, ALine: integer; var Bytes); override;
  public
    procedure BeginScan; override;
    property Bitmap: TCustomBitmap32 read FBitmap write SetBitmap;
    property SourceRect :TRect read GetSourceRect write SetSourceRect;
  end;

procedure TPsdBitmap32Layer.SetBitmap(const Value: TCustomBitmap32);
begin
  FBitmap := Value;
  if (FBitmap <> nil) then
  begin
    Width := FBitmap.Width;
    Height := FBitmap.Height;
  end;
end;

function TPsdBitmap32Layer.GetSourceRect: TRect;
begin
   Result.TopLeft :=  FSourceTopLeft;
   Result.Width := Width;
   Result.Height := Height;
end;

procedure TPsdBitmap32Layer.SetSourceRect(const Value: TRect);
begin
   FSourceTopLeft :=  Value.TopLeft;
   Width := Value.Width;
   Height := Value.Height;
end;

procedure TPsdBitmap32Layer.BeginScan;
begin
   if Bitmap = nil  then
      Exit;
   if Width > Bitmap.Width then
      Width := Bitmap.Width;

   if Height > Bitmap.Height then
      Height := Bitmap.Height;
end;

procedure TPsdBitmap32Layer.GetChannelScanLine(AChannel, ALine: integer; var Bytes);
var
  i: integer;
  pDest: PByte;
  pSource: PByte;
begin
  if (Bitmap = nil) then
  begin
    FillChar(Bytes, Width, $FF);
    Exit;
  end;

  if AChannel < 0 then
    AChannel := 3
  else
    AChannel := 2 - AChannel;

  ALine := ALine + FSourceTopLeft.Y;

  pSource := @(PColor32Entry(Bitmap.ScanLine[ALine]).Planes[AChannel]);
  pDest := @Bytes;

  Inc(pSource, FSourceTopLeft.X * SizeOf(TColor32));

  for i:= 0 to Width-1 do
  begin
    pDest^ := pSource^;

    Inc(pDest);
    Inc(pSource, SizeOf(TColor32));
  end;
end;

procedure ExportRectangles(Src :TBitmap32; const Rects:array of TRect);
var
  Filename: string;
  PsdBuilder: TPsdBuilder;
  PsdLayer: TCustomPsdLayer;
  i: integer;
  Location: TRect;
begin
  if not PromptForFilename(Filename, 'PhotoShop files (*.psd)|*.psd', 'psd', '', '', True) then
     Exit;
  PsdBuilder := TPsdBuilder.Create;
  try
      PsdBuilder.SetSize(Src.Width, Src.Height); //  don't need background layer, just fix size of document
      for i := 0 to Length(Rects) -1 do
      begin // All layers share the same source bitmap
          Location:=  Rects[i];
          PsdLayer := PsdBuilder.AddLayer(TPsdBitmap32Layer);
          PsdLayer.Opacity := $FF;
          PsdLayer.BoundsRect := Location;
          TPsdBitmap32Layer(PsdLayer).Bitmap :=  Src;
          TPsdBitmap32Layer(PsdLayer).SourceRect := Location;
          PsdLayer.Name := 'Layer ' + inttostr(i);
      end;
      PsdBuilder.Build();
      PsdBuilder.Stream.SaveToFile(Filename);
  finally
    PsdBuilder.Free;
  end;
end;

procedure TFormMain.BOpenClick(Sender: TObject);
var
  Filename: string;
begin
   if PromptForFilename(Filename, 'Supported files (*.jpg *.png *.bmp)|*.jpg;*.png;*.bmp') then
   begin
      ImgView.Bitmap.LoadFromFile(Filename);
      ImgView.Bitmap.DrawMode := dmBlend;
   end;
end;

procedure TFormMain.BSaveClick(Sender: TObject);
var
  I, J, Dx, Dy:integer;
  Regs:array of TRect;
  Comp:TPsdLayerCompression;
begin
   if ImgView.Bitmap.Empty then
      Exit;
   Dx := ImgView.Bitmap.Width div 3;
   Dy := ImgView.Bitmap.Height div 3;
   SetLength(Regs, 9);
   for I := 0 to 2 do
    for J := 0 to 2 do
    begin
        Regs[I * 3 + J] := Bounds(J * Dx, I * Dy, Dx, Dy);
    end;

   ExportRectangles(ImgView.Bitmap, Regs);
end;



end.
