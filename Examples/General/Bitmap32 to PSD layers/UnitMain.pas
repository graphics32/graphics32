unit UnitMain;

interface

uses
  {$IFDEF FPC}LCLIntf, LResources, LCLType, {$ELSE} Winapi.Windows, {$ENDIF}
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls, StdCtrls,
  GR32,
  GR32_Image,
  GR32_Layers;

type
  TRectangles = array of TRect;

  // A simple custom paint layer used to visualize the tile rectangles
  TTileLayer = class(TCustomLayer)
  private
    FRectangles: TRectangles;
  protected
    procedure Paint(Buffer: TBitmap32); override;
  public
    procedure SetRectangles(const ARectangles: TRectangles);
  end;

type
  TFormMain = class(TForm)
    ButtonSave: TButton;
    Panel1: TPanel;
    ButtonOpen: TButton;
    ImgView: TImgView32;
    CheckBoxViewTiles: TCheckBox;
    procedure ButtonSaveClick(Sender: TObject);
    procedure ButtonOpenClick(Sender: TObject);
    procedure CheckBoxViewTilesClick(Sender: TObject);
  private
    FRectangles: TRectangles;
    FTileLayer: TTileLayer;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  Types,
  GR32.ImageFormats,
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


procedure ExportTiles(ABitmap: TCustomBitmap32; const ARectangles: TRectangles);
var
  Writer: IImageFormatWriter;
  FileInfo: IImageFormatFileInfo;
  Filter: string;
  Filename: string;
  PSD: TPhotoshopDocument;
  PSDLayer: TCustomPhotoshopLayer;
  i: integer;
  Stream: TStream;
begin
  // Find the image format writer for PSD images
  Writer := ImageFormatManager.Writers.FindWriter('psd');

  // Get the FileInfo interface from it and construct a filter for the open dialog
  if (Writer <> nil) and (Supports(Writer, IImageFormatFileInfo, FileInfo)) then
    Filter := Format('%0:s (*.%1:s)|*.%1:s', [FileInfo.ImageFormatDescription, FileInfo.ImageFormatFileTypes[0]])
  else
    Filter := 'PhotoShop files (*.psd)|*.psd';

  if not PromptForFilename(Filename, Filter, 'psd', '', '', True) then
    Exit;

  PSD := TPhotoshopDocument.Create;
  try

    // This creates the PSD background image and set the size of the PSD image
    PSD.Assign(ABitmap);

    // Create a layer for each of the tiles
    for i := 0 to High(ARectangles) do
    begin
      PSDLayer := PSD.Layers.Add;
      PSDLayer.BoundsRect := ARectangles[i];
      PSDLayer.Name := Format('Layer %d', [i+1]);

      // All layers reference the same source ABitmap
      TPhotoshopLayer32(PSDLayer).Bitmap :=  ABitmap;

      // Specify the area of the bitmap the PSD layer image should be created from
      TPhotoshopLayer32(PSDLayer).SourceRect := ARectangles[i];
    end;

    Stream := TFileStream.Create(Filename, fmCreate);
    try

      TPhotoshopDocumentWriter.SaveToStream(PSD, Stream);

    finally
      Stream.Free;
    end;

  finally
    PSD.Free;
  end;
end;

{ TTileLayer }

procedure TTileLayer.SetRectangles(const ARectangles: TRectangles);
begin
  FRectangles := ARectangles;
  Changed;
end;

procedure TTileLayer.Paint(Buffer: TBitmap32);
var
  i: integer;
  r: TFloatRect;
begin
  for i := 0 to High(FRectangles) do
  begin
    r := FloatRect(FRectangles[i]);

    // Rectangle is in bitmap coordinates. Translate it to viewport coordinates
    r.TopLeft := LayerCollection.LocalToViewport(r.TopLeft, True);
    r.BottomRight := LayerCollection.LocalToViewport(r.BottomRight, True);

    // Outline the tile as a semitransparent red rectangle
    Buffer.FrameRectTS(MakeRect(r), clTrRed32);
  end;
end;

{ TFormMain }

procedure TFormMain.CheckBoxViewTilesClick(Sender: TObject);
begin
  FTileLayer.Visible := TCheckBox(Sender).Checked;
end;

constructor TFormMain.Create(AOwner: TComponent);
begin
  inherited;
  FTileLayer := TTileLayer.Create(ImgView.Layers);
  FTileLayer.Visible := False;
end;

procedure TFormMain.ButtonOpenClick(Sender: TObject);
var
  Filter: string;
  Filename: string;

  X, Y: integer;
  SizeX, SizeY: integer;
begin
  Filter := ImageFormatManager.BuildFileFilter(IImageFormatReader, True);

  if not PromptForFilename(Filename, Filter) then
    exit;

  ImgView.Bitmap.LoadFromFile(Filename);
  ImgView.Bitmap.DrawMode := dmBlend;

  // Divide the bitmap into 3*3=9 equally sized tiles
  SizeX := ImgView.Bitmap.Width div 3;
  SizeY := ImgView.Bitmap.Height div 3;

  SetLength(FRectangles, 9);

  // Note that due to rounding the tiles might not cover the whole bitmap.
  // For example a 100*100 bitmap will be divided into nine 33*33 tiles
  // thus not covering the last column and the last row.
  for Y := 0 to 2 do
    for X := 0 to 2 do
      FRectangles[Y * 3 + X] := Bounds(X * SizeX, Y * SizeY, SizeX, SizeY);

  // Pass the rectangles to the custom paint layer so it can draw them
  FTileLayer.SetRectangles(FRectangles);
end;

procedure TFormMain.ButtonSaveClick(Sender: TObject);
var
  Bitmap: TCustomBitmap32;
begin
  Bitmap := ImgView.Bitmap;

  if Bitmap.Empty then
    Exit;

  ExportTiles(Bitmap, FRectangles);
end;

end.
