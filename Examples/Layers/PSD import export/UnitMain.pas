unit UnitMain;

interface

uses
  {$IFDEF FPC}LCLIntf, LResources, LCLType, {$ELSE} Winapi.Windows, {$ENDIF}
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls, StdCtrls, Buttons, ComCtrls,
  Actions, ActnList, ImageList, ImgList,
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
    PanelTop: TPanel;
    ComboBoxCompression: TComboBox;
    LabelCompression: TLabel;
    ButtonCompressionWarning: TSpeedButton;
    ButtonLoad: TButton;
    PanelLayers: TPanel;
    Splitter1: TSplitter;
    ListViewLayers: TListView;
    TrackBarAlpha: TTrackBar;
    CheckBoxSaveBackground: TCheckBox;
    PanelLayerButtons: TPanel;
    ButtonLayerAdd: TSpeedButton;
    ImageList: TImageList;
    ButtonLayerDelete: TSpeedButton;
    ButtonLayerClear: TSpeedButton;
    ActionList: TActionList;
    ActionLayerAdd: TAction;
    ActionLayerDelete: TAction;
    ActionLayerClear: TAction;
    ActionCompressionWarning: TAction;
    ButtonLayerUp: TSpeedButton;
    ButtonLayerDown: TSpeedButton;
    ActionLayerDown: TAction;
    ActionLayerUp: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure ComboBoxCompressionChange(Sender: TObject);
    procedure ButtonLoadClick(Sender: TObject);
    procedure ListViewLayersItemChecked(Sender: TObject; Item: TListItem);
    procedure ListViewLayersSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure TrackBarAlphaChange(Sender: TObject);
    procedure ImgViewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ListViewLayersDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure ListViewLayersStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure ListViewLayersDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure CheckBoxSaveBackgroundClick(Sender: TObject);
    procedure Splitter1CanResize(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
    procedure ActionLayerAddExecute(Sender: TObject);
    procedure ActionLayerDeleteExecute(Sender: TObject);
    procedure ActionLayerClearExecute(Sender: TObject);
    procedure ActionLayerDeleteUpdate(Sender: TObject);
    procedure ActionLayerClearUpdate(Sender: TObject);
    procedure ActionCompressionWarningExecute(Sender: TObject);
    procedure ActionLayerUpExecute(Sender: TObject);
    procedure ActionLayerUpUpdate(Sender: TObject);
    procedure ActionLayerDownExecute(Sender: TObject);
    procedure ActionLayerDownUpdate(Sender: TObject);
  private
    FSelection: TPositionedLayer;
    FRubberbandLayer: TRubberbandLayer;
    procedure SetSelection(Value: TPositionedLayer);
    procedure MoveSelectedTo(Index: integer);
    procedure LayerOnClick(Sender: TObject);

  private
    FLocked: integer;
    FNames: TStringList;
    FNextLayerIndex: integer;

  private
    procedure Clear;
    function AddLayer(Opacity: integer): TBitmapLayer;
    procedure LoadLayerList(PhotoshopDocument: TPhotoshopDocument; ImgView: TImgView32); overload;
    procedure LoadLayerList(ImgView: TImgView32); overload;
    procedure LoadLayerListState;
    procedure InitLayer(Layer: TBitmapLayer; const Name: string = '');
    procedure InitLayers;
    procedure PixelCombineNone(F: TColor32; var B: TColor32; M: Cardinal);
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

  FNames := TStringList.Create;
  FNextLayerIndex := 1;

  Inc(FLocked);
  try

    for i := 1 to 5 do
      AddLayer(64+Random(256-64));

    LoadLayerList(ImgView);

  finally
    Dec(FLocked);
  end;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FNames.Free;
end;

procedure TFormMain.ButtonSaveClick(Sender: TObject);
begin
  SaveToPSD(ImgView, TPsdLayerCompression(ComboBoxCompression.ItemIndex));
end;

procedure TFormMain.CheckBoxSaveBackgroundClick(Sender: TObject);
begin
  if (TCheckBox(Sender).Checked) then
    DefaultPhotoshopExportOptions := DefaultPhotoshopExportOptions + [peBackgroundAsLayer]
  else
    DefaultPhotoshopExportOptions := DefaultPhotoshopExportOptions - [peBackgroundAsLayer];
end;

procedure TFormMain.Clear;
begin
  SetSelection(nil);
  FRubberbandLayer := nil;
  ImgView.Layers.Clear;
  FNames.Clear;
  FNextLayerIndex := 1;
  LoadLayerList(ImgView);
end;

procedure TFormMain.ComboBoxCompressionChange(Sender: TObject);
begin
  ActionCompressionWarning.Visible := (TPsdLayerCompression(ComboBoxCompression.ItemIndex) >= lcZIP)
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

  if (FRubberbandLayer <> nil) and (FRubberbandLayer.ChildLayer <> nil) then
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
        ListViewLayers.Items[i].Focused := True;
        break;
      end;
  end else
  begin
    ListViewLayers.Items[0].Selected := True;
    ListViewLayers.Items[0].Focused := True;
  end;
end;

procedure TFormMain.Splitter1CanResize(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
begin
  Accept := (NewSize < 250);
end;

procedure TFormMain.ActionCompressionWarningExecute(Sender: TObject);
begin
  MessageDlg('Be aware that many applications only support reading RAW and RLE compressed PSD files', mtWarning, [mbOK], 0);
end;

procedure TFormMain.ActionLayerAddExecute(Sender: TObject);
var
  Layer: TPositionedLayer;
begin
  Layer := AddLayer(64+Random(256-64));
  LoadLayerList(ImgView);
  SetSelection(Layer);
end;

procedure TFormMain.ActionLayerDeleteExecute(Sender: TObject);
var
  Layer: TCustomLayer;
  Index: integer;
begin
  Layer := FSelection;
  Index := FSelection.Index;

  SetSelection(nil);

  Layer.Free;
  LoadLayerList(ImgView);

  while (Index > 0) and ((Index >= ImgView.Layers.Count) or (ImgView.Layers[Index] = FRubberbandLayer)) do
    Dec(Index);

  if (Index < ImgView.Layers.Count) and (ImgView.Layers[Index] <> FRubberbandLayer) then
    SetSelection(TPositionedLayer(ImgView.Layers[Index]));
end;

procedure TFormMain.ActionLayerDeleteUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FSelection <> nil);
end;

procedure TFormMain.ActionLayerDownExecute(Sender: TObject);
begin
  if (FSelection.Index < ImgView.Layers.Count-1) then
    MoveSelectedTo(FSelection.Index + 1);
end;

procedure TFormMain.ActionLayerDownUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FSelection <> nil) and
    (((FRubberbandLayer = nil) and (FSelection.Index < ImgView.Layers.Count-1)) or
     ((FRubberbandLayer <> nil) and (FSelection.Index < ImgView.Layers.Count-2)));
end;

procedure TFormMain.ActionLayerUpExecute(Sender: TObject);
begin
  if (FSelection.Index > 0) then
    MoveSelectedTo(FSelection.Index - 1);
end;

procedure TFormMain.ActionLayerUpUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FSelection <> nil) and (FSelection.Index > 0);
end;

procedure TFormMain.ActionLayerClearExecute(Sender: TObject);
begin
  Clear;
end;

procedure TFormMain.ActionLayerClearUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := ((FRubberbandLayer = nil) and (ImgView.Layers.Count > 0)) or ((FRubberbandLayer <> nil) and (ImgView.Layers.Count > 1));
end;

function TFormMain.AddLayer(Opacity: integer): TBitmapLayer;
var
  i, Steps, nCorners ,X, Y, Diam, t2:integer;
  r, Ang: Double;
  Poly:TArrayOfFloatPoint;
begin
  Result := TBitmapLayer.Create(ImgView.Layers);

  X := ImgView.Bitmap.Width;
  if (X = 0) then
    X := 300;
  X := Random(X);
  Y := ImgView.Bitmap.Width;
  if (Y = 0) then
    Y := 400;
  Y := Random(Y);
  Diam := 50 + Random(100);
  t2 := Diam div 2;
  nCorners :=  4 + Random(6);
  Result.Bitmap.SetSize(Diam, Diam);
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

  GR32_Polygons.PolygonFS(Result.Bitmap, Poly, RandomColor());
  GR32_Polygons.PolyLineFS(Result.Bitmap, Poly, clBlack32, True, 2);

  Result.Bitmap.DrawMode := dmBlend;
  Result.Bitmap.MasterAlpha := Opacity;
  Result.Location := GR32.FloatRect(X, Y, X + Diam, Y + Diam);
  Result.Scaled := True;

  InitLayer(Result);
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

procedure TFormMain.ButtonLoadClick(Sender: TObject);
var
  Filename: string;
  Stream: TStream;
  PhotoshopDocument: TPhotoshopDocument;
begin
  if (not PromptForFileName(Filename, 'PhotoShop files (*.psd)|*.psd' {$ifdef DEBUG}, '', '', Graphics32Examples.MediaFolder{$endif})) then
    exit;

  Clear;

  ImgView.BeginUpdate;
  try
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

    ImgView.FitToViewport;

  finally
    ImgView.EndUpdate;
  end;

  InitLayers;
end;

procedure TFormMain.ImgViewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if (Button = mbMiddle) then
    ImgView.FitToViewport
  else
  if (Layer = nil) then
    SetSelection(nil);
end;

procedure TFormMain.InitLayer(Layer: TBitmapLayer; const Name: string);
begin
  Layer.OnClick := LayerOnClick;
  Layer.AlphaHit := True;
  Layer.MouseEvents := Layer.Visible;

  if (Layer.Tag = 0) then
  begin
    Layer.Tag := FNextLayerIndex;
    Inc(FNextLayerIndex);
  end;

  if (Layer.Tag-1 >= FNames.Count) then
  begin
    if (Name <> '') then
      FNames.Add(Name)
    else
      FNames.Add(Format('Layer %d', [Layer.Tag]));
  end;
end;

procedure TFormMain.InitLayers;
var
  i: integer;
begin
  for i := 0 to ImgView.Layers.Count-1 do
    if (ImgView.Layers[i] is TBitmapLayer) then
      InitLayer(TBitmapLayer(ImgView.Layers[i]));
end;

procedure TFormMain.LayerOnClick(Sender: TObject);
begin
  SetSelection(Sender as TPositionedLayer);
end;

procedure TFormMain.ListViewLayersDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  Item: TListItem;
  NewIndex: integer;
begin
  Item := ListViewLayers.GetItemAt(X, Y);
  NewIndex := TBitmapLayer(Item.Data).Index;
  MoveSelectedTo(NewIndex);
end;

procedure TFormMain.ListViewLayersDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  Item: TListItem;
begin
  if (Sender = ListViewLayers) then
  begin
    Item := ListViewLayers.GetItemAt(X, Y);
    Accept := (Item <> nil) and (not Item.Selected) and (Item.Data <> nil);
  end else
    Accept := False;
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

procedure TFormMain.ListViewLayersStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  if (ListViewLayers.Selected = nil) or (ListViewLayers.Selected.Data = nil) then
    CancelDrag;
end;

procedure TFormMain.PixelCombineNone(F: TColor32; var B: TColor32; M: Cardinal);
begin
  // Do nothing; Image becomes invisible
end;

procedure TFormMain.LoadLayerList(ImgView: TImgView32);
var
  i: integer;
  Item: TListItem;
  Layer: TBitmapLayer;
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
      if (not (ImgView.Layers[i] is TBitmapLayer)) then
        continue;

      Layer := TBitmapLayer(ImgView.Layers[i]);

      Item := ListViewLayers.Items.Add;
      Item.Data := Layer;

      Item.Selected := (FSelection = Layer);
      if (Item.Selected) then
        Item.Focused := True;

      InitLayer(Layer);
      Item.Caption := FNames[Layer.Tag-1];
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
  Layer: TBitmapLayer;
begin
  ListViewLayers.Items.BeginUpdate;
  Inc(FLocked);
  try

    ListViewLayers.Items.Clear;

    Item := ListViewLayers.Items.Add;
    Item.Caption := '(background)';
    Item.Selected := True;
    Item.Focused := True;

    LayerIndex := 0;
    for i := 0 to PhotoshopDocument.Layers.Count-1 do
    begin
      if (not(PhotoshopDocument.Layers[i] is TCustomPhotoshopBitmapLayer32)) then
        continue;

      Layer := ImgView.Layers[LayerIndex] as TBitmapLayer;

      Item := ListViewLayers.Items.Add;
      Item.Data := Layer;

      InitLayer(Layer, PhotoshopDocument.Layers[i].Name);
      Item.Caption := FNames[Layer.Tag-1];

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

procedure TFormMain.MoveSelectedTo(Index: integer);
var
  MoveLayer: TPositionedLayer;
begin
  MoveLayer := FSelection;

  SetSelection(nil);

  if (ImgView.Layers[Index] = FRubberbandLayer) then
    Inc(Index);

  MoveLayer.Index := Index;

  LoadLayerList(ImgView);
  SetSelection(MoveLayer);
end;

end.
