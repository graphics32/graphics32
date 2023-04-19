unit UnitMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  GR32,
  GR32_Image;

type
  TFormMain = class(TForm)
    ButtonSave: TButton;
    Panel1: TPanel;
    ButtonOpen: TButton;
    ImgView: TImgView32;
    procedure ButtonSaveClick(Sender: TObject);
    procedure ButtonOpenClick(Sender: TObject);
  private

  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  GR32.ImageFormats,
  GR32.ImageFormats.PSD,
  GR32.ImageFormats.PSD.Writer,
  GR32.ImageFormats.JPG,
  GR32.ImageFormats.PNG;

procedure ExportRectangles(ABitmap: TCustomBitmap32; const ARectangles: array of TRect);
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

  // Get the FileInfo interface from it and construct a fileter for the open dialog
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

    // Create a layer for each of the rectangles
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

procedure TFormMain.ButtonOpenClick(Sender: TObject);
var
  Filter: string;
  Filename: string;
begin
  Filter := ImageFormatManager.BuildFileFilter(IImageFormatReader, True);

  if PromptForFilename(Filename, Filter) then
  begin
    ImgView.Bitmap.LoadFromFile(Filename);
    ImgView.Bitmap.DrawMode := dmBlend;
  end;
end;

procedure TFormMain.ButtonSaveClick(Sender: TObject);
var
  X, Y: integer;
  SizeX, SizeY: integer;
  Rectangles: array of TRect;
  Bitmap: TCustomBitmap32;
begin
  Bitmap := ImgView.Bitmap;

  if Bitmap.Empty then
    Exit;

  // Divide the bitmap into 3*3=9 equally sized rectangles
  SizeX := Bitmap.Width div 3;
  SizeY := Bitmap.Height div 3;

  SetLength(Rectangles, 9);

  for Y := 0 to 2 do
    for X := 0 to 2 do
      Rectangles[Y * 3 + X] := Bounds(X * SizeX, Y * SizeY, SizeX, SizeY);

  ExportRectangles(Bitmap, Rectangles);
end;



end.
