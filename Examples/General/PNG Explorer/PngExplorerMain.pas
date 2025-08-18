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
    AcEditCopy: TEditCopy;
    AcEditCut: TEditCut;
    AcEditPaste: TEditPaste;
    AcEditUndo: TEditUndo;
    AcFileExit: TFileExit;
    AcFileOpen: TFileOpen;
    AcFileSaveAs: TFileSaveAs;
    ActionList: TActionList;
    CoolBar: TCoolBar;
    ImgView32: TImgView32;
    ListView: TListView;
    MainMenu: TMainMenu;
    MIAbout: TMenuItem;
    MiCopy: TMenuItem;
    MICut: TMenuItem;
    MIEdit: TMenuItem;
    MIExit: TMenuItem;
    MIFile: TMenuItem;
    MIHelp: TMenuItem;
    MIOpen: TMenuItem;
    MIPaste: TMenuItem;
    MISave: TMenuItem;
    MISaveAs: TMenuItem;
    MIStatusBar: TMenuItem;
    MIToolbar: TMenuItem;
    MIUndo: TMenuItem;
    MIView: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    PnMain: TPanel;
    PanelPreview: TPanel;
    SplitterHorizontal: TSplitter;
    SplitterVertical: TSplitter;
    StatusBar: TStatusBar;
    TbCopy: TToolButton;
    TbCut: TToolButton;
    TbOpen: TToolButton;
    TbPaste: TToolButton;
    TbSplit1: TToolButton;
    TbSplit2: TToolButton;
    ToolBar: TToolBar;
    ToolbarImages: TImageList;
    TreeView: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure AcFileOpenAccept(Sender: TObject);
    procedure MIStatusBarClick(Sender: TObject);
    procedure MIToolbarClick(Sender: TObject);
    procedure ShowHint(Sender: TObject);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
  private
    FPngFile : TMyPortableNetworkGraphic;
    FChunkRenderers: TDictionary<TCustomChunkClass, TChunkRenderer>;
    FHexDump: THexDump;
    FChunkData: TMemoryStream;
    procedure InitializeDefaultListView;
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
{$if defined(TPngChunkSuggestedPalette)} // TPngChunkSuggestedPalette is incomplete and has been disabled
    procedure DisplaySuggestedPaletteChunk(AChunk: TCustomChunk);
{$ifend}
    procedure DisplaySignificantBitsChunk(AChunk: TCustomChunk);
    procedure DisplayStandardColorSpaceRGBChunk(AChunk: TCustomChunk);
    procedure DisplayBackgroundColorChunk(AChunk: TCustomChunk);
    procedure DisplayTransparencyChunk(AChunk: TCustomChunk);
    procedure DisplayHistogramChunk(AChunk: TCustomChunk);
    procedure DisplayTimeChunk(AChunk: TCustomChunk);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadFromFile(Filename: TFileName);
    procedure LoadFromStream(Stream: TStream);
  end;

var
  FmPngExplorer: TFmPngExplorer;

implementation

{$R *.dfm}

uses
  Inifiles, Math, Types,
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
  GR32_PortableNetworkGraphic.Chunks.pHYs;

{ TFmPngExplorer }

procedure TFmPngExplorer.FormCreate(Sender: TObject);
begin
  Application.OnHint := ShowHint;
end;

procedure TFmPngExplorer.FormShow(Sender: TObject);
begin
  if (ParamCount > 0) and (FileExists(ParamStr(1))) then
    LoadFromFile(ParamStr(1));
end;

procedure TFmPngExplorer.InitializeDefaultListView;
begin
  // add columns
  ListViewColumns(['Name', 'Value']);
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

procedure TFmPngExplorer.ShowHint(Sender: TObject);
begin
  if Length(Application.Hint) > 0 then
  begin
    StatusBar.SimplePanel := True;
    StatusBar.SimpleText := Application.Hint;
  end
  else
    StatusBar.SimplePanel := False;
end;

procedure TFmPngExplorer.AcFileOpenAccept(Sender: TObject);
begin
  LoadFromFile(AcFileOpen.Dialog.Filename);
end;

procedure TFmPngExplorer.MIStatusBarClick(Sender: TObject);
begin
  MIStatusBar.Checked := not MIStatusBar.Checked;
  StatusBar.Visible := MIStatusBar.Checked;
end;

procedure TFmPngExplorer.MIToolbarClick(Sender: TObject);
begin
  MIToolbar.Checked := not MIToolbar.Checked;
  CoolBar.Visible := MIToolbar.Checked;
end;

procedure TFmPngExplorer.DisplayHeaderChunk(AChunk: TCustomChunk);
var
  Chunk: TPngChunkImageHeader;
begin
  Chunk := AChunk as TPngChunkImageHeader;
  InitializeDefaultListView;

  ListViewData(['Width', IntToStr(Chunk.Width)]);
  ListViewData(['Height', IntToStr(Chunk.Height)]);
  ListViewData(['Bit Depth', IntToStr(Chunk.BitDepth)]);
  ListViewData(['Color Type', ColorTypeToString(Chunk.ColorType)]);
  ListViewData(['Compression Method', IntToStr(Chunk.CompressionMethod)]);
  ListViewData(['Filter Method', 'Adaptive']);
  ListViewData(['Interlace Method', InterlaceMethodToString(Chunk.InterlaceMethod)]);
  ListViewData(['HasPallette', BoolToStr(Chunk.HasPalette)]);

  ListView.BringToFront;
end;

procedure TFmPngExplorer.DisplayPaletteChunk(AChunk: TCustomChunk);
var
  Index : Integer;
  Color: TRGB24;
  Chunk: TPngChunkPalette;
begin
  Chunk := AChunk as TPngChunkPalette;

  InitializeDefaultListView;

  ListViewColumns(['Index', 'Color']);

  for Index := 0 to Chunk.Count - 1 do
  begin
    Color := Chunk.PaletteEntry[Index];
    ListViewData([IntToStr(Index), '#' + IntToHex(Integer(Color.R shl 16 + Color.G shl 8 + Color.B), 6)]);
  end;

  ListView.BringToFront;
end;

procedure TFmPngExplorer.DisplayGammaChunk(AChunk: TCustomChunk);
var
  Chunk: TPngChunkGamma;
begin
  Chunk := AChunk as TPngChunkGamma;

  InitializeDefaultListView;

  ListViewData(['Gamma', FloatToStr(Chunk.GammaAsSingle)]);

  ListView.BringToFront;
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

  ListView.BringToFront;
end;

{$if defined(TPngChunkSuggestedPalette)}
procedure TFmPngExplorer.DisplaySuggestedPaletteChunk(Chunk: TPngChunkSuggestedPalette);
begin
  InitializeDefaultListView;

//    ListViewData(['Palette Entries', IntToStr(Chunk.Count)]);

  ListView.BringToFront;
end;
{$ifend}

procedure TFmPngExplorer.DisplaySignificantBitsChunk(AChunk: TCustomChunk);
var
  Chunk: TPngChunkSignificantBits;
begin
  Chunk := AChunk as TPngChunkSignificantBits;

  InitializeDefaultListView;

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

  ListView.BringToFront;
end;


procedure TFmPngExplorer.DisplayStandardColorSpaceRGBChunk(AChunk: TCustomChunk);
var
  Chunk: TPngChunkStandardColorSpaceRGB;
begin
  Chunk := AChunk as TPngChunkStandardColorSpaceRGB;

  InitializeDefaultListView;

  case Chunk.RenderingIntent of
    0 : ListViewData(['Rendering Intent', 'Perceptual']);
    1 : ListViewData(['Rendering Intent', 'Relative Colorimetric']);
    2 : ListViewData(['Rendering Intent', 'Saturation']);
    3 : ListViewData(['Rendering Intent', 'Absolute Colorimetric']);
  else
    ListViewData(['Rendering Intent', IntToStr(Chunk.RenderingIntent)]);
  end;

  ListView.BringToFront;
end;

procedure TFmPngExplorer.DisplayTextChunk(AChunk: TCustomChunk);
var
  Chunk: TCustomChunkPngText;
begin
  Chunk := AChunk as TCustomChunkPngText;

  InitializeDefaultListView;

  ListViewData([string(Chunk.Keyword), string(Chunk.Text)]);

  ListView.BringToFront;
end;

procedure TFmPngExplorer.DisplayTimeChunk(AChunk: TCustomChunk);
var
  Chunk: TPngChunkTime;
begin
  Chunk := AChunk as TPngChunkTime;

  InitializeDefaultListView;

  ListViewData(['Time', DateTimeToStr(Chunk.ModifiedDateTime)]);

  ListView.BringToFront;
end;

procedure TFmPngExplorer.DisplayTransparencyChunk(AChunk: TCustomChunk);
var
  Index : Integer;
  Chunk: TPngChunkTransparency;
begin
  Chunk := AChunk as TPngChunkTransparency;

  InitializeDefaultListView;

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

  ListView.BringToFront;
end;

procedure TFmPngExplorer.DisplayUnknownChunk(AChunk: TCustomChunk);
begin
  InitializeDefaultListView;

  ListViewData(['Chunk Name', string(AChunk.ChunkNameAsString)]);
  ListViewData(['Chunk Size', IntToStr(AChunk.ChunkSize)]);

  ListView.BringToFront;
end;

procedure TFmPngExplorer.DisplayPhysicalDimensionsChunk(AChunk: TCustomChunk);
var
  Chunk: TPngChunkPhysicalPixelDimensions;
begin
  Chunk := AChunk as TPngChunkPhysicalPixelDimensions;

  InitializeDefaultListView;

  ListViewData(['Pixels per unit X', IntToStr(Chunk.PixelsPerUnitX)]);
  ListViewData(['Pixels per unit Y', IntToStr(Chunk.PixelsPerUnitY)]);

  ListView.BringToFront;
end;

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

procedure TFmPngExplorer.DisplayBackgroundColorChunk(AChunk: TCustomChunk);
var
  Chunk: TPngChunkBackgroundColor;
begin
  Chunk := AChunk as TPngChunkBackgroundColor;

  InitializeDefaultListView;

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

  ListView.BringToFront;
end;

procedure TFmPngExplorer.DisplayChromaticitiesChunk(AChunk: TCustomChunk);
var
  Chunk: TPngChunkPrimaryChromaticities;
begin
  Chunk := AChunk as TPngChunkPrimaryChromaticities;

  InitializeDefaultListView;

  ListViewData(['White X', FloatToStr(Chunk.WhiteXAsSingle)]);
  ListViewData(['White Y', FloatToStr(Chunk.WhiteYAsSingle)]);
  ListViewData(['Red X', FloatToStr(Chunk.RedXAsSingle)]);
  ListViewData(['Red Y', FloatToStr(Chunk.RedYAsSingle)]);
  ListViewData(['Green X', FloatToStr(Chunk.GreenXAsSingle)]);
  ListViewData(['Green Y', FloatToStr(Chunk.GreenYAsSingle)]);
  ListViewData(['Blue X', FloatToStr(Chunk.BlueXAsSingle)]);
  ListViewData(['Blue Y', FloatToStr(Chunk.BlueYAsSingle)]);

  ListView.BringToFront;
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
    exit;
  end;

  // display chunk size
  if TObject(Node.Data) is TCustomDefinedChunk then
    StatusBar.SimpleText := 'Chunk Size: ' + IntToStr(TCustomDefinedChunk(Node.Data).ChunkSize);

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
  i: integer;
  Chunk: TCustomChunk;
begin
  TreeView.Items.BeginUpdate;
  try

    for i := 0 to FPngFile.Chunks.Count-1 do
    begin
      Chunk := FPngFile.Chunks[i];

      TreeView.Items.AddChildObject(TreeView.Items[0], string(Chunk.ChunkNameAsString), Chunk);
    end;

  finally
    TreeView.Items.EndUpdate;
  end;

  ImgView32.Bitmap.Assign(FPngFile);

  TreeView.Items[0].Expanded := True;
end;

procedure TFmPngExplorer.RegisterChunkRenderers;
begin
  // PNG HeaderChunk chunk
  FChunkRenderers.Add(TPngChunkImageHeader, DisplayHeaderChunk);
  FChunkRenderers.Add(TPngChunkPalette, DisplayPaletteChunk);
  FChunkRenderers.Add(TPngChunkGamma, DisplayGammaChunk);
  FChunkRenderers.Add(TPngChunkTime, DisplayTimeChunk);
(*
  FChunkRenderers.Add(TPngChunkPhysicalScale, DisplayPhysicalScaleChunk);
*)
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

procedure TFmPngExplorer.LoadFromFile(Filename: TFileName);
var
  Start, Stop, Freq : Int64;
  MemoryFileStream  : TMemoryStream;
begin
  if FileExists(FileName) then
  begin
    // initialize listview
    InitializeDefaultListView;

    // create temporary memory strem
    MemoryFileStream := TMemoryStream.Create;
    try
      // query start
      QueryPerformanceCounter(Start);

      // load data from file
      MemoryFileStream.LoadFromFile(Filename);

      // query stop
      QueryPerformanceCounter(Stop);

      // query performance frequency
      QueryPerformanceFrequency(Freq);

      // add loading TimeChunk
      ListViewData(['loading time', FloatToStrF((Stop - Start) * 1000 / Freq, ffGeneral, 4, 4) + ' ms']);

      // query start
      QueryPerformanceCounter(Start);

      // load PNG file
      FPngFile.LoadFromStream(MemoryFileStream);

      // query stop
      QueryPerformanceCounter(Stop);

      // query performance frequency
      QueryPerformanceFrequency(Freq);

      // add loading TimeChunk
      ListViewData(['interpreting time', FloatToStrF((Stop - Start) * 1000 / Freq, ffGeneral, 4, 4) + ' ms']);

    finally
      FreeAndNil(MemoryFileStream);
    end;

    // clear existing items on treeview
    TreeView.Items.Clear;

    // add root item on treeview
    TreeView.Items.AddChild(nil, ExtractFileName(FileName));

    // query start
    QueryPerformanceCounter(Start);

    PNGChanged;

    // query stop
    QueryPerformanceCounter(Stop);

    // add building tree TimeChunk
    ListViewData(['building tree time', FloatToStrF((Stop - Start) * 1000 / Freq, ffGeneral, 4, 4) + ' ms']);

    ListView.BringToFront;

    Caption := 'PNG Explorer [' + ExtractFileName(Filename) + ']';
  end
  else
    raise Exception.Create('File does not exists');
end;

procedure TFmPngExplorer.LoadFromStream(Stream: TStream);
var
  Start, Stop, Freq : Int64;
begin
  // reset stream position
  Stream.Position := 0;

  // query start
  QueryPerformanceCounter(Start);

  // load PNG file
  FPNGFile.LoadFromStream(Stream);

  // query stop
  QueryPerformanceCounter(Stop);

  // query performance frequency
  QueryPerformanceFrequency(Freq);

  // initialize listview
  InitializeDefaultListView;

  // add loading TimeChunk
  ListViewData(['loading time', FloatToStrF((Stop - Start) * 1000 / Freq, ffGeneral, 4, 4) + ' ms']);

  // clear existing items on treeview
  TreeView.Items.Clear;

  // add root item on treeview
  TreeView.Items.AddChild(nil, '(internal PNG)');

  // query start
  QueryPerformanceCounter(Start);

  PNGChanged;

  // query stop
  QueryPerformanceCounter(Stop);

  // add building tree TimeChunk
  ListViewData(['building tree time', FloatToStrF((Stop - Start) * 1000 / Freq, ffGeneral, 4, 4) + ' ms']);

  ListView.BringToFront;

  // change caption
  Caption := 'PNG Explorer';
end;

end.
