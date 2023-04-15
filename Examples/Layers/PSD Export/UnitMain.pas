unit UnitMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  GR32,
  GR32_Image,
  GR32_Layers,
  GR32_PNG,
  GR32_Polygons,
  UPSD_Storage;

type
  TPsdBitmap32Layer = class(TPsdLayer)
  public
    Bitmap: TCustomBitmap32;
    procedure GetChannelScanLine(AChannel, ALine: integer; var Bytes); override;
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

procedure PsdSave(AImgView: TImgView32; ExportLayers: boolean);
var
 PsdBuilder: TPsdBuilder;
 PsdLayer: TPsdBitmap32Layer;
 Filename: string;
 I, W, H: integer;
 SourceLayer: TCustomLayer;
 BackgroundBitmap: TBitmap32;
 Location: TFloatRect;
 LayerBitmap: TBitmap32;
begin
  if AImgView.Bitmap.Empty then
    Exit;

  if not PromptForFilename(Filename, 'PhotoShop files (*.psd)|*.psd', 'psd', '', '', True) then
    Exit;

  W := AImgView.Bitmap.Width;
  H := AImgView.Bitmap.Height;

  PsdBuilder := TPsdBuilder.Create;
  try
    PsdBuilder.PsdCompression := psComRLE;
    PsdBuilder.PsdLayerCompression := psComZip; // some editors don't support zip compression

    BackgroundBitmap := TBitmap32.Create;
    try
      // Create flattened bitmap
      BackgroundBitmap.SetSize(W, H);
      AImgView.PaintTo(BackgroundBitmap, BackgroundBitmap.BoundsRect);

      PsdLayer := TPsdBitmap32Layer.Create;
      PsdLayer.Bitmap := BackgroundBitmap;
      PsdLayer.LayerSetBounds(0, 0, BackgroundBitmap.Width, BackgroundBitmap.Height);

      PsdBuilder.Background := PsdLayer; // PsdBuilder now owns SourceLayer, but not the bitmap

      if ExportLayers then
        for I := 0 to AImgView.Layers.Count -1 do
        begin
          SourceLayer := AImgView.Layers[I];
          if not (SourceLayer is TBitmapLayer) then
            continue;

          LayerBitmap := TBitmapLayer(SourceLayer).Bitmap;
          Location := TBitmapLayer(SourceLayer).Location;

          PsdLayer := TPsdBitmap32Layer.Create;
          PsdBuilder.Add(PsdLayer);
          PsdLayer.LayerSetBounds(Round(Location.Left),
                                Round(Location.Top),
                                LayerBitmap.Width,
                                LayerBitmap.Height);
          PsdLayer.Opacity := LayerBitmap.MasterAlpha;
          PsdLayer.Bitmap :=  LayerBitmap;

          PsdLayer.Name := 'Layer ' + inttostr(I);
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


procedure TFormMain.BSaveClick(Sender: TObject);
begin
  PsdSave(ImgView, CExportLayers.Checked);
end;

procedure TPsdBitmap32Layer.GetChannelScanLine(AChannel, ALine: integer; var Bytes);
var
  I: integer;
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

  for I:= 0 to Bitmap.Width-1 do
  begin
    pDest^ := pSource^;

    Inc(pDest);
    Inc(pSource, SizeOf(TColor32));
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

  for I := 0 to 3 do
     Star($FF);
  for I := 0 to 3 do // semi transparent forms
     Star($80);
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ImgView.Background.CheckersStyle := bcsMedium;
  ImgView.Background.FillStyle := bfsCheckers;

  ImgView.Bitmap.SetSize(600,400);
  ImgView.Bitmap.DrawMode := dmBlend;

  BRandomClick(nil);
end;


end.
