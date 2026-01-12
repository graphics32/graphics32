unit UnitMain;

interface

uses
  {$IFDEF FPC}LCLIntf, LResources, LCLType, {$ELSE} Winapi.Windows, {$ENDIF}
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls, StdCtrls, Buttons, ComCtrls,
  GR32,
  GR32_Image,
  GR32_Layers,
  GR32_Polygons,
  GR32.ImageFormats.PSD,
  GR32.ImageFormats.PSD.Model;

type
  TFormMain = class(TForm)
    ImgView: TImgView32;
    ButtonSave: TButton;
    Panel1: TPanel;
    ComboBoxCompression: TComboBox;
    LabelCompression: TLabel;
    ButtonCompressionWarning: TSpeedButton;
    ButtonLoad: TButton;
    PanelLayers: TPanel;
    ButtonAddLayer: TButton;
    Splitter1: TSplitter;
    ButtonClear: TButton;
    ListViewLayers: TListView;
    TrackBarAlpha: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure ButtonAddLayerClick(Sender: TObject);
    procedure ComboBoxCompressionChange(Sender: TObject);
    procedure ButtonCompressionWarningClick(Sender: TObject);
    procedure ButtonLoadClick(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
    procedure ListViewLayersItemChecked(Sender: TObject; Item: TListItem);
    procedure ListViewLayersSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure TrackBarAlphaChange(Sender: TObject);
    procedure ImgViewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
  private
    FSelection: TPositionedLayer;
    FRubberbandLayer: TRubberbandLayer;
    procedure SetSelection(Value: TPositionedLayer);
    procedure LayerOnClick(Sender: TObject);

  private
    FLocked: integer;

  private
    procedure AddLayer(Opacity:integer);
    procedure LoadLayerList(PhotoshopDocument: TPhotoshopDocument; ImgView: TImgView32); overload;
    procedure LoadLayerList(ImgView: TImgView32); overload;
    procedure LoadLayerListState;
    procedure InitLayers(Layer: TBitmapLayer = nil);
    procedure PixelCombineNone(F: TColor32; var B: TColor32; M: Cardinal);
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
  GR32.ImageFormats.PSD.Writer,
  GR32.ImageFormats.PSD.Reader,
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
  if (not PromptForFilename(Filename, 'PhotoShop files (*.psd)|*.psd', 'psd', '', '', True)) then
    Exit;

  Stream := TFileStream.Create(Filename, fmCreate);
  try
    PhotoshopDocument := TPhotoshopDocument.Create;
    try
      PhotoshopDocument.Compression := ACompression;

      // Construct a PSD based on the layers of the TImgView32
      PhotoshopDocument.Assign(AImgView);

      PhotoshopDocumentWriter.SaveToStream(PhotoshopDocument, Stream);
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

procedure TFormMain.SetSelection(Value: TPositionedLayer);
var
  i: integer;
begin
  if (Value = FSelection) then
    exit;

  if (FRubberbandLayer <> nil) then
  begin
    FRubberbandLayer.ChildLayer := nil;
    FRubberbandLayer.LayerOptions := LOB_NO_UPDATE;
    ImgView.Invalidate;
  end;

  FSelection := Value;

  if (FSelection <> nil) then
  begin
    if FRubberbandLayer = nil then
    begin
      FRubberbandLayer := TRubberBandLayer.Create(ImgView.Layers);
      FRubberbandLayer.Handles := [rhCenter, rhSides, rhCorners];
    end else
      FRubberbandLayer.BringToFront;

    FRubberbandLayer.ChildLayer := FSelection;
    FRubberbandLayer.LayerOptions := LOB_VISIBLE or LOB_MOUSE_EVENTS or LOB_NO_UPDATE;

    for i := 0 to ListViewLayers.Items.Count-1 do
      if (ListViewLayers.Items[i].Data = FSelection) then
      begin
        ListViewLayers.Items[i].Selected := True;
        break;
      end;
  end else
    ListViewLayers.Items[0].Selected := True;
end;

procedure TFormMain.AddLayer(Opacity: integer);
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
  GR32_Polygons.PolyLineFS(BitmapLayer.Bitmap, Poly, clBlack32, True, 2);

  BitmapLayer.Bitmap.DrawMode := dmBlend;
  BitmapLayer.Bitmap.MasterAlpha := Opacity;
  BitmapLayer.Location := GR32.FloatRect(X, Y, X + Diam, Y + Diam);
  BitmapLayer.Scaled := True;

  InitLayers(BitmapLayer);
end;

procedure TFormMain.TrackBarAlphaChange(Sender: TObject);
begin
  if (FLocked > 0) then
    exit;

  if (ListViewLayers.Selected.Data <> nil) then
    TBitmapLayer(ListViewLayers.Selected.Data).Bitmap.MasterAlpha := TrackBarAlpha.Position
  else
    ImgView.Bitmap.MasterAlpha := TrackBarAlpha.Position;
end;

procedure TFormMain.ButtonClearClick(Sender: TObject);
begin
  SetSelection(nil);
  FRubberbandLayer := nil;
  ImgView.Layers.Clear;
  LoadLayerList(ImgView);
end;

procedure TFormMain.ButtonCompressionWarningClick(Sender: TObject);
begin
  MessageDlg('Be aware that many applications only support reading RAW and RLE compressed PSD files', mtWarning, [mbOK], 0);
end;

procedure TFormMain.ButtonLoadClick(Sender: TObject);
var
  Filename: string;
  Stream: TStream;
  PhotoshopDocument: TPhotoshopDocument;
begin
  if (not PromptForFileName(Filename, 'PhotoShop files (*.psd)|*.psd' {$ifdef DEBUG}, '', '', Graphics32Examples.MediaFolder{$endif})) then
    exit;

  SetSelection(nil);
  FRubberbandLayer := nil;
  ImgView.Layers.Clear;

  Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try

    PhotoshopDocument := TPhotoshopDocument.Create;
    try

      PhotoshopDocumentReader.LoadFromStream(PhotoshopDocument, Stream);

      // Populate TImgView32 from the layers of the PSD
      ImgView.Assign(PhotoshopDocument);

      // Start with background hidden
      ImgView.Bitmap.DrawMode := dmCustom;

      LoadLayerList(PhotoshopDocument, ImgView);

      // Update compression selection with the first one used in the document
      ComboBoxCompression.ItemIndex := Ord(PhotoshopDocument.Compression);
      ComboBoxCompressionChange(ComboBoxCompression); // Update warning

    finally
      PhotoshopDocument.Free;
    end;

  finally
    Stream.Free;
  end;

  InitLayers;
end;

procedure TFormMain.ButtonAddLayerClick(Sender: TObject);
begin
  AddLayer(64+Random(256-64));
  LoadLayerList(ImgView);
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  i: integer;
begin
  ImgView.Background.CheckersStyle := bcsMedium;
  ImgView.Background.FillStyle := bfsCheckers;

  ImgView.Bitmap.DrawMode := dmBlend;
  ImgView.Bitmap.MasterAlpha := 192;
  ImgView.Bitmap.OnPixelCombine := PixelCombineNone;

  if Graphics32Examples.MediaFileExists('Monalisa.jpg') then
    // Background is a static bitmap
    ImgView.Bitmap.LoadFromFile(Graphics32Examples.MediaFolder+'\Monalisa.jpg');

  Inc(FLocked);
  try

    for i := 1 to 5 do
      ButtonAddLayer.Click;

  finally
    Dec(FLocked);
  end;
end;

procedure TFormMain.ImgViewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if (Layer = nil) then
    SetSelection(nil);
end;

procedure TFormMain.InitLayers(Layer: TBitmapLayer);

  procedure DoInitLayer(Layer: TBitmapLayer);
  begin
    Layer.OnClick := LayerOnClick;
    Layer.AlphaHit := True;
    Layer.MouseEvents := Layer.Visible;
  end;

var
  i: integer;
begin
  if (Layer <> nil) then
    DoInitLayer(Layer)
  else
    for i := 0 to ImgView.Layers.Count-1 do
      if (ImgView.Layers[i] is TBitmapLayer) then
        DoInitLayer(TBitmapLayer(ImgView.Layers[i]));
end;

procedure TFormMain.LayerOnClick(Sender: TObject);
begin
  SetSelection(Sender as TPositionedLayer);
end;

procedure TFormMain.ListViewLayersItemChecked(Sender: TObject; Item: TListItem);
var
  i: integer;
begin
  if (ListViewLayers.Items.Count = 0) then
    exit;

  if (FLocked > 0) then
    exit;

  if (ListViewLayers.Items[0].Checked) then
    ImgView.Bitmap.DrawMode := dmBlend
  else
    ImgView.Bitmap.DrawMode := dmCustom;

  for i := 1 to ListViewLayers.Items.Count-1 do
    if (ListViewLayers.Items[i].Data <> nil) then
      TCustomLayer(ListViewLayers.Items[i].Data).Visible := ListViewLayers.Items[i].Checked;
end;

procedure TFormMain.ListViewLayersSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if (FLocked > 0) then
    exit;

  TrackBarAlpha.Enabled := Selected;

  Inc(FLocked);
  try

    if (Selected) then
    begin
      if (Item.Data <> nil) then
        TrackBarAlpha.Position := TBitmapLayer(Item.Data).Bitmap.MasterAlpha
      else
        TrackBarAlpha.Position := ImgView.Bitmap.MasterAlpha;

      SetSelection(TBitmapLayer(Item.Data));
    end else
      TrackBarAlpha.Position := 0;

  finally
    Dec(FLocked);
  end;
end;

procedure TFormMain.PixelCombineNone(F: TColor32; var B: TColor32; M: Cardinal);
begin
  // Do nothing; Image becomes invisible
end;

procedure TFormMain.LoadLayerList(ImgView: TImgView32);
var
  i: integer;
  Item: TListItem;
begin
  ListViewLayers.Items.BeginUpdate;
  Inc(FLocked);
  try

    ListViewLayers.Items.Clear;

    Item := ListViewLayers.Items.Add;
    Item.Caption := '(background)';
    Item.Selected := (FSelection = nil);

    for i := 0 to ImgView.Layers.Count-1 do
    begin
      Item := ListViewLayers.Items.Add;
      Item.Caption := Format('Layer %d', [i+1]);
      Item.Data := ImgView.Layers[i];
      Item.Selected := (FSelection = ImgView.Layers[i]);
    end;

  finally
    Dec(FLocked);
    ListViewLayers.Items.EndUpdate;
  end;

  LoadLayerListState;
end;

procedure TFormMain.LoadLayerList(PhotoshopDocument: TPhotoshopDocument; ImgView: TImgView32);
var
  i: integer;
  LayerIndex: integer;
  Item: TListItem;
begin
  ListViewLayers.Items.BeginUpdate;
  Inc(FLocked);
  try

    ListViewLayers.Items.Clear;

    Item := ListViewLayers.Items.Add;
    Item.Caption := '(background)';
    Item.Selected := True;

    LayerIndex := 0;
    for i := 0 to PhotoshopDocument.Layers.Count-1 do
    begin
      if (not(PhotoshopDocument.Layers[i] is TCustomPhotoshopBitmapLayer32)) then
        continue;

      Item := ListViewLayers.Items.Add;
      Item.Caption := PhotoshopDocument.Layers[i].Name;
      Item.Data := ImgView.Layers[LayerIndex];

      Inc(LayerIndex);
    end;

  finally
    Dec(FLocked);
    ListViewLayers.Items.EndUpdate;
  end;

  LoadLayerListState;
end;

procedure TFormMain.LoadLayerListState;
var
  i: integer;
begin
  if (ListViewLayers.Items.Count = 0) then
    exit;

  Inc(FLocked);
  try

    ListViewLayers.Items[0].Checked := (ImgView.Bitmap.DrawMode <> dmCustom);

    for i := 1 to ListViewLayers.Items.Count-1 do
    begin
      ListViewLayers.Items[i].Checked := TCustomLayer(ListViewLayers.Items[i].Data).Visible;
      // Update layer clickability while we're at it
      TCustomLayer(ListViewLayers.Items[i].Data).MouseEvents := TCustomLayer(ListViewLayers.Items[i].Data).Visible;
    end;

  finally
    Dec(FLocked);
  end;
end;

end.
