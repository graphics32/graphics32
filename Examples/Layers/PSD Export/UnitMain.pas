unit UnitMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  GR32,
  GR32_Image,
  GR32_Layers,
  GR32_Polygons,
  UPSD_Storage;

type
  TFormMain = class(TForm)
    ImgView: TImgView32;
    ButtonSave: TButton;
    Panel1: TPanel;
    CheckBoxExportLayers: TCheckBox;
    ButtonRandom: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure ButtonRandomClick(Sender: TObject);
  private
    procedure Star(Opacity:integer);
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

type
  TPsdBitmap32Layer = class(TCustomPsdLayer)
  private
    FBitmap: TCustomBitmap32;
    procedure SetBitmap(const Value: TCustomBitmap32);
  protected
    procedure GetChannelScanLine(AChannel, ALine: integer; var Bytes); override;
  public
    property Bitmap: TCustomBitmap32 read FBitmap write SetBitmap;
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

  pSource := @(PColor32Entry(Bitmap.ScanLine[ALine]).Planes[AChannel]);
  pDest := @Bytes;

  for i:= 0 to Bitmap.Width-1 do
  begin
    pDest^ := pSource^;

    Inc(pDest);
    Inc(pSource, SizeOf(TColor32));
  end;
end;

procedure PsdSave(AImgView: TImgView32; ExportLayers: boolean);
var
  Filename: string;
  PsdBuilder: TPsdBuilder;
  PsdLayer: TCustomPsdLayer;
  i: integer;
  SourceLayer: TCustomLayer;
  BackgroundBitmap: TBitmap32;
  Location: TFloatRect;
  LayerBitmap: TBitmap32;
begin
  if AImgView.Bitmap.Empty then
    Exit;

  if not PromptForFilename(Filename, 'PhotoShop files (*.psd)|*.psd', 'psd', '', '', True) then
    Exit;

  PsdBuilder := TPsdBuilder.Create;
  try
    PsdBuilder.Compression := psComRLE;
    PsdBuilder.LayerCompression := psComZip; // some editors don't support zip compression

    BackgroundBitmap := TBitmap32.Create;
    try
      // Create flattened bitmap for use as background
      BackgroundBitmap.SetSizeFrom(AImgView.Bitmap);
      AImgView.PaintTo(BackgroundBitmap, BackgroundBitmap.BoundsRect);

      PsdLayer := TPsdBitmap32Layer.Create;
      TPsdBitmap32Layer(PsdLayer).Bitmap := BackgroundBitmap; // Layer just references the bitmap; It doesn't own it.

      PsdBuilder.Background := PsdLayer; // PsdBuilder now owns the layer, but not the bitmap

      if ExportLayers then
        for i := 0 to AImgView.Layers.Count -1 do
        begin
          SourceLayer := AImgView.Layers[i];
          if not (SourceLayer is TBitmapLayer) then
            continue;

          LayerBitmap := TBitmapLayer(SourceLayer).Bitmap;
          Location := TBitmapLayer(SourceLayer).Location;

          PsdLayer := PsdBuilder.AddLayer(TPsdBitmap32Layer);
          PsdLayer.Opacity := LayerBitmap.MasterAlpha;
          PsdLayer.Left := Round(Location.Left);
          PsdLayer.Top := Round(Location.Top);
          TPsdBitmap32Layer(PsdLayer).Bitmap :=  LayerBitmap;

          PsdLayer.Name := 'Layer ' + inttostr(i);
        end;

      PsdBuilder.Build;

      PsdBuilder.Stream.SaveToFile(Filename);

    finally
      BackgroundBitmap.Free;
    end;
  finally
    PsdBuilder.Free;
  end;
end;


procedure TFormMain.ButtonSaveClick(Sender: TObject);
begin
  PsdSave(ImgView, CheckBoxExportLayers.Checked);
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
  i, Steps, nCorners ,X, Y, Diam, t2:integer;
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

  for i := 0 to Steps do
  begin
    r := t2;
    if Odd(i) then
      r := t2 * 0.6;
    Poly[i] := FloatPoint(t2 + Sin(i * Ang) * r, t2 + Cos(i * Ang) * r);
  end;

  GR32_Polygons.PolygonFS(BitmapLayer.Bitmap, Poly, RandomColor());
  GR32_Polygons.PolyLineFS(BitmapLayer.Bitmap, Poly, clBlack32,True, 2);


  BitmapLayer.Bitmap.DrawMode := dmBlend;
  BitmapLayer.Bitmap.MasterAlpha := Opacity;
  BitmapLayer.Location := GR32.FloatRect(X, Y, X + Diam, Y + Diam);
  BitmapLayer.Scaled := True;
end;

procedure TFormMain.ButtonRandomClick(Sender: TObject);
var
  i:Integer;
begin
  ImgView.Layers.Clear;

  for i := 0 to 3 do
    Star($FF); // Solid shapes

  for i := 0 to 3 do
    Star($80); // Semi-transparent shapes
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ImgView.Background.CheckersStyle := bcsMedium;
  ImgView.Background.FillStyle := bfsCheckers;

  ImgView.Bitmap.SetSize(600,400);
  ImgView.Bitmap.DrawMode := dmBlend;

  ButtonRandom.Click;
end;

end.
