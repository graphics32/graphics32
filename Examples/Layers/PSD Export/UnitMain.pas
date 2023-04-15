unit UnitMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  GR32, GR32_Image,
  GR32_Layers,
  GR32_PNG,
  GR32_Polygons,
  UPSD_Storage;

type
  TPsdBitmap32Layer = class(TPsdLayer)
  public
      Bitmap: TCustomBitmap32;
      procedure GetChannelScanLine(AChannel, ALine:integer;var Bytes);override;
   end;

  TFormMain = class(TForm)
    ImgView: TImgView32;
    BSave: TButton;
    Panel1: TPanel;
    CExportLayers: TCheckBox;
    BRandom: TButton;
    procedure FormCreate(Sender: TObject);
    procedure BSaveClick(Sender: TObject);
    procedure BRandomClick(Sender: TObject);
  private
    procedure Star(Opacity:integer);
  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}
procedure PsdSave(AImgView: TImgView32; ExportLayers:boolean);
var
 Psd:TPsdBuilder;
 BLayer :TPsdBitmap32Layer;
 Filename:string;
 I, W, H:integer;
 Ly:TCustomLayer;
 B : TBitmap32;
 L: TFloatRect;
 Bmp32:TBitmap32;
begin
  if AImgView.Bitmap.Empty then
      Exit;

  W := AImgView.Bitmap.Width;
  H := AImgView.Bitmap.Height;

  B := TBitmap32.Create;
  Psd := TPsdBuilder.Create;
  try
    B.SetSize(W, H);
    AImgView.PaintTo(B, System.Classes.Rect(0, 0, W, H));
	  BLayer := TPsdBitmap32Layer.Create;
	  BLayer.Bitmap := B;
    BLayer.LayerSetBounds(0, 0, B.Width, B.Height);
	  Psd.Background := BLayer;
    Psd.PsdCompression := psComRLE;
	  Psd.PsdLayerCompression := psComZip; // some editors don't support zip compression
    if ExportLayers then
      for I := 0 to AImgView.Layers.Count -1 do
      begin
          Ly := AImgView.Layers[I];
          if not (Ly is TBitmapLayer) then
            continue;
          Bmp32 := TBitmapLayer(Ly).Bitmap;
          L :=  TBitmapLayer(Ly).Location;

          BLayer := TPsdBitmap32Layer.Create;
          Psd.Add(BLayer);
          BLayer.LayerSetBounds(Round(L.Left),
                                Round(L.Top),
                                Bmp32.Width,
                                Bmp32.Height);
          BLayer.Opacity := Bmp32.MasterAlpha;
          BLayer.Bitmap :=  Bmp32;

          BLayer.Name := 'Layer ' + inttostr(I);
      end;
    Psd.Build;
    if PromptForFilename(Filename,'PS files (*.psd)|*.psd','psd','','',True) then
       Psd.Stream.SaveToFile(Filename);
  finally
    Psd.Free;
    B.Free;
  end;
end;


procedure TFormMain.BSaveClick(Sender: TObject);
begin
   PsdSave(ImgView, CExportLayers.Checked);
end;

procedure TPsdBitmap32Layer.GetChannelScanLine(AChannel, ALine: integer;var Bytes);
var
 I:integer;
 pData:PByteArray;
 P:PColor32Entry;
 C:TColor32;
begin
   if (Bitmap = nil) then
   begin
     FillChar(Bytes, Width, $FF);
     Exit;
   end;

   pData := @Bytes;
   if AChannel < 0 then
      AChannel := 3
   else
      AChannel := 2 - AChannel;
   for I:= 0 to Bitmap.Width-1 do
   begin
     C := Bitmap.Pixels[I, ALine];
     pData[I] := TColor32Entry(C).Planes[AChannel];
   end;
end;

function RandomColor():TColor32;
begin
  Result := Color32(64 + Random(192),
                    64 + Random(192),
                    64 + Random(192));
end;

procedure TFormMain.Star(Opacity: integer);
var
  BitmapLayer: TBitmapLayer;
  I, Steps, nCorners ,X, Y, Diam, t2:integer;
  r, Ang: Double;
  Poly:TArrayOfFloatPoint;
begin
  BitmapLayer := TBitmapLayer.Create(ImgView.Layers);
  X := Random(400);
  Y := Random(300);
  Diam := 50 + Random(100);
  t2 := Diam div 2;
  nCorners :=  4 + Random(6);
  BitmapLayer.Bitmap.SetSize(Diam, Diam);
  Steps := nCorners * 2;
  Setlength(Poly, Steps + 1);
  Ang := PI / nCorners;

  for I := 0 to Steps do
  begin
     r := t2;
     if Odd(I) then
       r := t2 * 0.6;
     Poly[I] := FloatPoint(t2 + Sin(I * Ang) * r, t2 + Cos(I * Ang) * r);
  end;

  GR32_Polygons.PolygonFS(BitmapLayer.Bitmap, Poly, RandomColor());
  GR32_Polygons.PolyLineFS(BitmapLayer.Bitmap, Poly, clBlack32,True, 2);


  with BitmapLayer do
  begin
      Bitmap.DrawMode := dmBlend;
      Bitmap.MasterAlpha := Opacity;
      Location := GR32.FloatRect(X, Y, X + Diam, Y + Diam);
      Scaled := True;
  end;
end;

procedure TFormMain.BRandomClick(Sender: TObject);
var
 I:Integer;
begin
  ImgView.Layers.Clear;
  ImgView.Bitmap.Clear(Color32(clWhite));

  for I := 0 to 3 do
     Star($FF);
  for I := 0 to 3 do // semi transparent forms
     Star($80);
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ImgView.Background.CheckersStyle := bcsMedium;
  ImgView.Bitmap.SetSize(600,400);

  BRandomClick(nil);
end;


end.
