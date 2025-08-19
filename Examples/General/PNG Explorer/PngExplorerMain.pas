unit PNGExplorerMain;

interface

{$include GR32.inc}

uses
  Generics.Collections,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ComCtrls, StdCtrls, ExtCtrls, ToolWin, ActnList, StdActns,
  ImgList, System.Actions,

  HexDump,

{$if defined(NeedImageList)}
//  System.ImageList,
{$ifend}
  GR32,
  GR32_PortableNetworkGraphic,
  GR32_PortableNetworkGraphic.Chunks,
  GR32_PNG,
  GR32_Image, System.ImageList;

type
  TChunkRenderer = procedure(Chunk: TCustomChunk) of object;

type
  TMyPortableNetworkGraphic = class(TPortableNetworkGraphic32);

  TFmPngExplorer = class(TForm)
    ImgView32: TImgView32;
    ListView: TListView;
    PnMain: TPanel;
    PanelPreview: TPanel;
    SplitterHorizontal: TSplitter;
    SplitterVertical: TSplitter;
    TreeView: TTreeView;
    Panel1: TPanel;
    ButtonLoad: TButton;
    OpenDialog: TOpenDialog;
    procedure FormShow(Sender: TObject);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure ButtonLoadClick(Sender: TObject);
  private
    FPngFile : TMyPortableNetworkGraphic;
    FChunkRenderers: TDictionary<TCustomChunkClass, TChunkRenderer>;
    FHexDump: THexDump;
    FChunkData: TMemoryStream;
    procedure InitializeDefaultListView(AChunk: TCustomChunk = nil);
    procedure PNGChanged;
  protected
    procedure ListViewColumns(const Columns: array of string);
    procedure ListViewData(const Strings: array of string);

    procedure RegisterChunkRenderers;

    procedure DisplayUnknownChunk(AChunk: TCustomChunk);
    procedure DisplayHeaderChunk(AChunk: TCustomChunk);
    procedure DisplayPaletteChunk(AChunk: TCustomChunk);
    procedure DisplayChromaticitiesChunk(AChunk: TCustomChunk);
    procedure DisplayGammaChunk(AChunk: TCustomChunk);
    procedure DisplayPhysicalDimensionsChunk(AChunk: TCustomChunk);
    procedure DisplayTextChunk(AChunk: TCustomChunk);
    procedure DisplaySuggestedPaletteChunk(AChunk: TCustomChunk);
    procedure DisplaySignificantBitsChunk(AChunk: TCustomChunk);
    procedure DisplayStandardColorSpaceRGBChunk(AChunk: TCustomChunk);
    procedure DisplayBackgroundColorChunk(AChunk: TCustomChunk);
    procedure DisplayTransparencyChunk(AChunk: TCustomChunk);
    procedure DisplayHistogramChunk(AChunk: TCustomChunk);
    procedure DisplayTimeChunk(AChunk: TCustomChunk);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadFromFile(const Filename: TFileName);
  end;

var
  FmPngExplorer: TFmPngExplorer;

implementation

{$R *.dfm}

uses
  Inifiles, Math, Types,
  GR32_System,
  GR32_PortableNetworkGraphic.Types,
  GR32_PortableNetworkGraphic.Chunks.PLTE,
  GR32_PortableNetworkGraphic.Chunks.gAMA,
  GR32_PortableNetworkGraphic.Chunks.tRNS,
  GR32_PortableNetworkGraphic.Chunks.tEXt,
  GR32_PortableNetworkGraphic.Chunks.sRGB,
  GR32_PortableNetworkGraphic.Chunks.cHRM,
  GR32_PortableNetworkGraphic.Chunks.tIME,
  GR32_PortableNetworkGraphic.Chunks.sBIT,
  GR32_PortableNetworkGraphic.Chunks.bKGD,
  GR32_PortableNetworkGraphic.Chunks.hIST,
  GR32_PortableNetworkGraphic.Chunks.pHYs,
  GR32_PortableNetworkGraphic.Chunks.sPLT;

{ TFmPngExplorer }

constructor TFmPngExplorer.Create(AOwner: TComponent);
begin
  inherited;

  FChunkRenderers := TDictionary<TCustomChunkClass, TChunkRenderer>.Create;
  FPngFile := TMyPortableNetworkGraphic.Create;

  RegisterChunkRenderers;

  FHexDump := THexDump.Create(Self);
  FHexDump.Parent := PanelPreview;
  FHexDump.Align := alClient;
  FHexDump.Visible := False;
  FHexDump.ReadOnly := True;
end;

destructor TFmPngExplorer.Destroy;
begin
  FPngFile.Free;
  FChunkRenderers.Free;
  FChunkData.Free;

  inherited;
end;

procedure TFmPngExplorer.FormShow(Sender: TObject);
begin
  if (ParamCount > 0) and (FileExists(ParamStr(1))) then
    LoadFromFile(ParamStr(1));
end;

procedure TFmPngExplorer.ButtonLoadClick(Sender: TObject);
begin
  if (OpenDialog.Execute) then
    LoadFromFile(OpenDialog.Filename);
end;

procedure TFmPngExplorer.InitializeDefaultListView(AChunk: TCustomChunk);
begin
  // add columns
  ListViewColumns(['Name', 'Value']);

  if (AChunk <> nil) then
  begin
    ListViewData(['Chunk Name', string(AChunk.ChunkNameAsString)]);
    ListViewData(['Chunk Size', Format('%.0n', [AChunk.ChunkSize * 1.0])]);
  end;
end;

procedure TFmPngExplorer.ListViewColumns(const Columns: array of string);
var
  i : Integer;
  Column: TListColumn;
begin
  // clear list view
  ListView.Clear;

  // clear columns
  ListView.Columns.Clear;

  // add column
  for i := 0 to Length(Columns) - 1 do
  begin
    Column := ListView.Columns.Add;

    Column.Caption := Columns[i];
    Column.Width := Min(256, (ListView.Width - 16) div (Length(Columns)));
    Column.MinWidth := 64;
    Column.AutoSize := True;
  end;
end;

procedure TFmPngExplorer.ListViewData(const Strings: array of string);
var
  i : Integer;
  Item: TListItem;
begin
  // add data
  Item := ListView.Items.Add;
  Item.Caption := Strings[0];
  for i := 1 to Length(Strings) - 1 do
    Item.SubItems.Add(Strings[i]);
end;

procedure TFmPngExplorer.DisplayHeaderChunk(AChunk: TCustomChunk);
var
  Chunk: TPngChunkImageHeader;
begin
  Chunk := AChunk as TPngChunkImageHeader;
  InitializeDefaultListView(AChunk);

  ListViewData(['Width', IntToStr(Chunk.Width)]);
  ListViewData(['Height', IntToStr(Chunk.Height)]);
  ListViewData(['Bit Depth', IntToStr(Chunk.BitDepth)]);
  ListViewData(['Color Type', ColorTypeToString(Chunk.ColorType)]);
  ListViewData(['Compression Method', IntToStr(Chunk.CompressionMethod)]);
  ListViewData(['Filter Method', 'Adaptive']);
  ListViewData(['Interlace Method', InterlaceMethodToString(Chunk.InterlaceMethod)]);
  ListViewData(['HasPallette', BoolToStr(Chunk.HasPalette)]);
end;

procedure TFmPngExplorer.DisplayPaletteChunk(AChunk: TCustomChunk);
var
  Index : Integer;
  Color: TRGB24;
  Chunk: TPngChunkPalette;
begin
  Chunk := AChunk as TPngChunkPalette;

  InitializeDefaultListView(AChunk);

  ListViewColumns(['Index', 'Color']);

  for Index := 0 to Chunk.Count - 1 do
  begin
    Color := Chunk.PaletteEntry[Index];
    ListViewData([IntToStr(Index), '#' + IntToHex(Integer(Color.R shl 16 + Color.G shl 8 + Color.B), 6)]);
  end;
end;

procedure TFmPngExplorer.DisplayGammaChunk(AChunk: TCustomChunk);
var
  Chunk: TPngChunkGamma;
begin
  Chunk := AChunk as TPngChunkGamma;

  InitializeDefaultListView(AChunk);

  ListViewData(['Gamma', FloatToStr(Chunk.GammaAsSingle)]);
end;

procedure TFmPngExplorer.DisplayHistogramChunk(AChunk: TCustomChunk);
var
  Index : Integer;
  Chunk: TPngChunkImageHistogram;
begin
  Chunk := AChunk as TPngChunkImageHistogram;

  ListViewColumns(['Index', 'Frequency']);

  for Index := 0 to Chunk.Count - 1 do
    ListViewData([IntToStr(Index), IntToStr(Chunk.Frequency[Index])]);
end;

procedure TFmPngExplorer.DisplaySuggestedPaletteChunk(AChunk: TCustomChunk);
{$if defined(TPngChunkSuggestedPalette)}
var
  Chunk: TPngChunkSuggestedPalette;
{$ifend}
begin
// TPngChunkSuggestedPalette is incomplete and has been disabled
{$if defined(TPngChunkSuggestedPalette)}
  Chunk := AChunk as TPngChunkSuggestedPalette;

  InitializeDefaultListView(AChunk);

//    ListViewData(['Palette Entries', IntToStr(Chunk.Count)]);
{$ifend}
end;

procedure TFmPngExplorer.DisplaySignificantBitsChunk(AChunk: TCustomChunk);
var
  Chunk: TPngChunkSignificantBits;
begin
  Chunk := AChunk as TPngChunkSignificantBits;

  InitializeDefaultListView(AChunk);

  if Chunk.SignificantBits is TPngSignificantBitsFormat0 then
    ListViewData(['Greyscale Bits', IntToStr(TPngSignificantBitsFormat0(Chunk.SignificantBits).GrayBits)])
  else
  if Chunk.SignificantBits is TPngSignificantBitsFormat23 then
  begin
    ListViewData(['Red Bits', IntToStr(TPngSignificantBitsFormat23(Chunk.SignificantBits).RedBits)]);
    ListViewData(['Green Bits', IntToStr(TPngSignificantBitsFormat23(Chunk.SignificantBits).GreenBits)]);
    ListViewData(['Blue Bits', IntToStr(TPngSignificantBitsFormat23(Chunk.SignificantBits).BlueBits)]);
  end else
  if Chunk.SignificantBits is TPngSignificantBitsFormat4 then
  begin
    ListViewData(['Greyscale Bits', IntToStr(TPngSignificantBitsFormat4(Chunk.SignificantBits).GrayBits)]);
    ListViewData(['Alpha Bits', IntToStr(TPngSignificantBitsFormat4(Chunk.SignificantBits).AlphaBits)]);
  end else
  if Chunk.SignificantBits is TPngSignificantBitsFormat6 then
  begin
    ListViewData(['Red Bits', IntToStr(TPngSignificantBitsFormat6(Chunk.SignificantBits).RedBits)]);
    ListViewData(['Green Bits', IntToStr(TPngSignificantBitsFormat6(Chunk.SignificantBits).GreenBits)]);
    ListViewData(['Blue Bits', IntToStr(TPngSignificantBitsFormat6(Chunk.SignificantBits).BlueBits)]);
    ListViewData(['Alpha Bits', IntToStr(TPngSignificantBitsFormat6(Chunk.SignificantBits).AlphaBits)]);
  end;
end;

procedure TFmPngExplorer.DisplayStandardColorSpaceRGBChunk(AChunk: TCustomChunk);
var
  Chunk: TPngChunkStandardColorSpaceRGB;
begin
  Chunk := AChunk as TPngChunkStandardColorSpaceRGB;

  InitializeDefaultListView(AChunk);

  case Chunk.RenderingIntent of
    0 : ListViewData(['Rendering Intent', 'Perceptual']);
    1 : ListViewData(['Rendering Intent', 'Relative Colorimetric']);
    2 : ListViewData(['Rendering Intent', 'Saturation']);
    3 : ListViewData(['Rendering Intent', 'Absolute Colorimetric']);
  else
    ListViewData(['Rendering Intent', IntToStr(Chunk.RenderingIntent)]);
  end;
end;

procedure TFmPngExplorer.DisplayTextChunk(AChunk: TCustomChunk);
var
  Chunk: TCustomChunkPngText;
begin
  Chunk := AChunk as TCustomChunkPngText;

  InitializeDefaultListView(AChunk);

  ListViewData([string(Chunk.Keyword), string(Chunk.Text)]);
end;

procedure TFmPngExplorer.DisplayTimeChunk(AChunk: TCustomChunk);
var
  Chunk: TPngChunkTime;
begin
  Chunk := AChunk as TPngChunkTime;

  InitializeDefaultListView(AChunk);

  ListViewData(['Time', DateTimeToStr(Chunk.ModifiedDateTime)]);
end;

procedure TFmPngExplorer.DisplayTransparencyChunk(AChunk: TCustomChunk);
var
  Index : Integer;
  Chunk: TPngChunkTransparency;
begin
  Chunk := AChunk as TPngChunkTransparency;

  InitializeDefaultListView(AChunk);

  if Chunk.Transparency is TPngTransparencyFormat0 then
    ListViewData(['Grey Sample Value', IntToStr(TPngTransparencyFormat0(Chunk.Transparency).GraySampleValue)])
  else
  if Chunk.Transparency is TPngTransparencyFormat2 then
  begin
    ListViewData(['Red Sample Value', IntToStr(TPngTransparencyFormat2(Chunk.Transparency).RedSampleValue)]);
    ListViewData(['Blue Sample Value', IntToStr(TPngTransparencyFormat2(Chunk.Transparency).BlueSampleValue)]);
    ListViewData(['Green Sample Value', IntToStr(TPngTransparencyFormat2(Chunk.Transparency).GreenSampleValue)]);
  end else
  if Chunk.Transparency is TPngTransparencyFormat3 then
    for Index := 0 to TPngTransparencyFormat3(Chunk.Transparency).Count - 1 do
      ListViewData(['Index ' + IntToStr(Index), IntToStr(TPngTransparencyFormat3(Chunk.Transparency).Transparency[Index])]);
end;

procedure TFmPngExplorer.DisplayUnknownChunk(AChunk: TCustomChunk);
begin
  InitializeDefaultListView(AChunk);
end;

procedure TFmPngExplorer.DisplayPhysicalDimensionsChunk(AChunk: TCustomChunk);
var
  Chunk: TPngChunkPhysicalPixelDimensions;
begin
  Chunk := AChunk as TPngChunkPhysicalPixelDimensions;

  InitializeDefaultListView(AChunk);

  ListViewData(['Pixels per unit X', IntToStr(Chunk.PixelsPerUnitX)]);
  ListViewData(['Pixels per unit Y', IntToStr(Chunk.PixelsPerUnitY)]);
end;

procedure TFmPngExplorer.DisplayBackgroundColorChunk(AChunk: TCustomChunk);
var
  Chunk: TPngChunkBackgroundColor;
begin
  Chunk := AChunk as TPngChunkBackgroundColor;

  InitializeDefaultListView(AChunk);

  if Chunk.Background is TPngBackgroundColorFormat04 then
    ListViewData(['Grey', IntToStr(TPngBackgroundColorFormat04(Chunk.Background).GraySampleValue)])
  else
  if Chunk.Background is TPngBackgroundColorFormat26 then
  begin
    ListViewData(['Red', IntToStr(TPngBackgroundColorFormat26(Chunk.Background).RedSampleValue)]);
    ListViewData(['Blue', IntToStr(TPngBackgroundColorFormat26(Chunk.Background).BlueSampleValue)]);
    ListViewData(['Green', IntToStr(TPngBackgroundColorFormat26(Chunk.Background).GreenSampleValue)]);
  end else
  if Chunk.Background is TPngBackgroundColorFormat3 then
    ListViewData(['Palette Index', IntToStr(TPngBackgroundColorFormat3(Chunk.Background).PaletteIndex)]);
end;

procedure TFmPngExplorer.DisplayChromaticitiesChunk(AChunk: TCustomChunk);
var
  Chunk: TPngChunkPrimaryChromaticities;
begin
  Chunk := AChunk as TPngChunkPrimaryChromaticities;

  InitializeDefaultListView(AChunk);

  ListViewData(['White X', FloatToStr(Chunk.WhiteXAsSingle)]);
  ListViewData(['White Y', FloatToStr(Chunk.WhiteYAsSingle)]);
  ListViewData(['Red X', FloatToStr(Chunk.RedXAsSingle)]);
  ListViewData(['Red Y', FloatToStr(Chunk.RedYAsSingle)]);
  ListViewData(['Green X', FloatToStr(Chunk.GreenXAsSingle)]);
  ListViewData(['Green Y', FloatToStr(Chunk.GreenYAsSingle)]);
  ListViewData(['Blue X', FloatToStr(Chunk.BlueXAsSingle)]);
  ListViewData(['Blue Y', FloatToStr(Chunk.BlueYAsSingle)]);
end;

procedure TFmPngExplorer.TreeViewChange(Sender: TObject; Node: TTreeNode);
var
  Renderer: TChunkRenderer;
  Chunk: TCustomChunk;
begin
  Node.Expanded := True;

  if (Node.Data = nil) then
  begin
    FHexDump.Visible := False;
    ImgView32.Visible := True;
    DisplayHeaderChunk(FPngFile.ImageHeader);
    exit;
  end;

  if TObject(Node.Data) is TCustomChunk then
  begin
    Chunk := TCustomChunk(Node.Data);

    if (not FChunkRenderers.TryGetValue(TCustomChunkClass(Chunk.ClassType), Renderer)) then
      Renderer := DisplayUnknownChunk;
  end else
  begin
    Renderer := nil;
    Chunk := nil;
  end;

  if (Chunk <> nil) then
  begin
    if Assigned(Renderer) then
      Renderer(Chunk);

    if (Chunk.ChunkData <> nil) then
      FHexDump.Address := Chunk.ChunkData
    else
    begin
      if (FChunkData = nil) then
        FChunkData := TMemoryStream.Create
      else
        FChunkData.Position := 0;
      FChunkData.Size := Chunk.ChunkSize;
      Chunk.WriteToStream(FChunkData);

      FHexDump.Address := FChunkData.Memory;
    end;

    FHexDump.DataSize := Chunk.ChunkSize;
  end;

  ImgView32.Visible := False;
  FHexDump.Visible := (Chunk <> nil);
end;

procedure TFmPngExplorer.PNGChanged;
var
  Chunk: TCustomChunk;
begin
  TreeView.Items.BeginUpdate;
  try

    for Chunk in FPngFile.Chunks do
      TreeView.Items.AddChildObject(TreeView.Items[0], string(Chunk.ChunkNameAsString), Chunk);

  finally
    TreeView.Items.EndUpdate;
  end;

  ImgView32.Bitmap.Assign(FPngFile);
  ImgView32.Scale := 1.0;
  ImgView32.ScrollToCenter;

  TreeView.Items[0].Expanded := True;
end;

procedure TFmPngExplorer.RegisterChunkRenderers;
begin
  FChunkRenderers.Add(TPngChunkImageHeader, DisplayHeaderChunk);
  FChunkRenderers.Add(TPngChunkPalette, DisplayPaletteChunk);
  FChunkRenderers.Add(TPngChunkGamma, DisplayGammaChunk);
  FChunkRenderers.Add(TPngChunkTime, DisplayTimeChunk);
{$if defined(TPngChunkPhysicalScale)}
  FChunkRenderers.Add(TPngChunkPhysicalScale, DisplayPhysicalScaleChunk); // TODO
{$ifend}
  FChunkRenderers.Add(TCustomChunkPngText, DisplayTextChunk);
  FChunkRenderers.Add(TPngChunkStandardColorSpaceRGB, DisplayStandardColorSpaceRGBChunk);
  FChunkRenderers.Add(TPngChunkImageHistogram, DisplayHistogramChunk);
  FChunkRenderers.Add(TPngChunkBackgroundColor, DisplayBackgroundColorChunk);
{$if defined(TPngChunkSuggestedPalette)}
  FChunkRenderers.Add(TPngChunkSuggestedPalette, DisplaySuggestedPaletteChunk);
{$ifend}
  FChunkRenderers.Add(TPngChunkPrimaryChromaticities, DisplayChromaticitiesChunk);
  FChunkRenderers.Add(TPngChunkPhysicalPixelDimensions, DisplayPhysicalDimensionsChunk);
  FChunkRenderers.Add(TPngChunkSignificantBits, DisplaySignificantBitsChunk);
  FChunkRenderers.Add(TPngChunkTransparency, DisplayTransparencyChunk);
end;

procedure TFmPngExplorer.LoadFromFile(const Filename: TFileName);
var
  Stopwatch: TStopwatch;
  MemoryFileStream  : TMemoryStream;
begin
  if not FileExists(FileName) then
    raise Exception.Create('File does not exists');

  // initialize listview
  InitializeDefaultListView;

  // Load from a temporary memory stream so we can separate the load from the parse time
  MemoryFileStream := TMemoryStream.Create;
  try

    // load data from file
    Stopwatch := TStopwatch.StartNew;
    MemoryFileStream.LoadFromFile(Filename);
    Stopwatch.Stop;
    ListViewData(['loading time', Format('%.3f ms', [Stopwatch.ElapsedTicks / Stopwatch.TicksPerMillisecond])]);

    // load PNG file
    Stopwatch := TStopwatch.StartNew;
    FPngFile.LoadFromStream(MemoryFileStream);
    Stopwatch.Stop;
    ListViewData(['interpreting time', Format('%.3f ms', [Stopwatch.ElapsedTicks / Stopwatch.TicksPerMillisecond])]);

  finally
    FreeAndNil(MemoryFileStream);
  end;

  // clear existing items on treeview
  TreeView.Items.Clear;

  // add root item on treeview
  TreeView.Items.AddChild(nil, ExtractFileName(FileName));

  Stopwatch := TStopwatch.StartNew;
  PNGChanged;
  Stopwatch.Stop;
  ListViewData(['building tree time', Format('%.3f ms', [Stopwatch.ElapsedTicks / Stopwatch.TicksPerMillisecond])]);

  Caption := 'PNG Explorer [' + ExtractFileName(Filename) + ']';
end;

end.
