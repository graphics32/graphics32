unit UnitMain;

interface

uses
  {$IFDEF FPC}LCLIntf, LResources, LCLType, {$ELSE} Winapi.Windows, {$ENDIF}
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls, StdCtrls, Buttons,
  GR32,
  GR32_Image,
  GR32_Layers,
  GR32_Polygons;

type
  TFormMain = class(TForm)
    ImgView: TImgView32;
    ButtonSave: TButton;
    Panel1: TPanel;
    ButtonRandom: TButton;
    ComboBoxCompression: TComboBox;
    LabelCompression: TLabel;
    ButtonCompressionWarning: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure ButtonRandomClick(Sender: TObject);
    procedure ComboBoxCompressionChange(Sender: TObject);
    procedure ButtonCompressionWarningClick(Sender: TObject);
  private
    procedure Star(Opacity:integer);
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
{$ifndef FPC}
  System.UITypes,
  System.Types,
{$endif}
  GR32.Examples,
  GR32.ImageFormats.PSD,
  GR32.ImageFormats.PSD.Writer,
  GR32.ImageFormats.JPG;

{$ifdef FPC}
function PromptForFilename(var AFilename: string; const AFilter: string;
  const ADefaultExt: string = ''; Dummy1: string = ''; Dummy2: string = '';
  Save: boolean = False): boolean;
var
  Dialog: TOpenDialog;
begin
  if (Save) then
    Dialog := TSaveDialog.Create(nil)
  else
    Dialog := TOpenDialog.Create(nil);
  try
    if (Save) then
      Dialog.Options := [ofPathMustExist, ofOverwritePrompt]
    else
      Dialog.Options := [ofFileMustExist];
    Dialog.Filter := AFilter;
    Dialog.Filename := AFilename;
    Dialog.DefaultExt := ADefaultExt;

    Result := Dialog.Execute;

    If Result then
      AFilename := Dialog.Filename;
  finally
    Dialog.Free;
  end;
end;
{$endif}

procedure SaveToPSD(AImgView: TImgView32; ACompression: TPsdLayerCompression);
var
  Filename: string;
  PhotoshopDocument: TPhotoshopDocument;
  Stream: TStream;
begin
  if not PromptForFilename(Filename, 'PhotoShop files (*.psd)|*.psd', 'psd', '', '', True) then
    Exit;

  Stream := TFileStream.Create(Filename, fmCreate);
  try
    PhotoshopDocument := TPhotoshopDocument.Create;
    try
      PhotoshopDocument.Compression := ACompression;

      // Construct a PSD based on the layers of the TImgView32
      PhotoshopDocument.Assign(AImgView);

      TPhotoshopDocumentWriter.SaveToStream(PhotoshopDocument, Stream);
    finally
      PhotoshopDocument.Free;
    end;

  finally
    Stream.Free;
  end;
end;


procedure TFormMain.ButtonSaveClick(Sender: TObject);
begin
  SaveToPSD(ImgView, TPsdLayerCompression(ComboBoxCompression.ItemIndex));
end;

procedure TFormMain.ComboBoxCompressionChange(Sender: TObject);
begin
  ButtonCompressionWarning.Visible := (TPsdLayerCompression(ComboBoxCompression.ItemIndex) >= lcZIP)
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

procedure TFormMain.ButtonCompressionWarningClick(Sender: TObject);
begin
  MessageDlg('Be aware that many applications only support reading RAW and RLE compressed PSD files', mtWarning, [mbOK], 0);
end;

procedure TFormMain.ButtonRandomClick(Sender: TObject);
var
  i: Integer;
begin
  ImgView.Layers.Clear;

  // Add a bunch of random shapes
  for i := 0 to 3 do
    Star($FF); // Solid shapes

  for i := 0 to 3 do
    Star($80); // Semi-transparent shapes
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ImgView.Background.CheckersStyle := bcsMedium;
  ImgView.Background.FillStyle := bfsCheckers;

  ImgView.Bitmap.DrawMode := dmBlend;
  ImgView.Bitmap.MasterAlpha := 192;
  ImgView.Bitmap.DrawMode := dmBlend;

  if Graphics32Examples.MediaFileExists('Monalisa.jpg') then
    // Background is a static bitmap
    ImgView.Bitmap.LoadFromFile(Graphics32Examples.MediaFolder+'\Monalisa.jpg');

  ButtonRandom.Click;
end;

end.
