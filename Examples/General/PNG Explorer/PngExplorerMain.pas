unit PNGExplorerMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ComCtrls, StdCtrls, ExtCtrls, ToolWin, ActnList, StdActns,
  ImgList, System.ImageList, System.Actions,
  GR32,
  GR32_PortableNetworkGraphic,
  GR32_PNG,
  GR32_Image;

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
    PnPaintBox: TPanel;
    SpHorizontal: TSplitter;
    SpVertical: TSplitter;
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
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure AcFileOpenAccept(Sender: TObject);
    procedure MIStatusBarClick(Sender: TObject);
    procedure MIToolbarClick(Sender: TObject);
    procedure ShowHint(Sender: TObject);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure TreeViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    FPngFile : TMyPortableNetworkGraphic;
    procedure InitializeDefaultListView;
    procedure PNGChanged;
  protected
    procedure ListViewColumns(Columns: Array of string);
    procedure ListViewData(Strings: Array of string);

    procedure DisplayHeaderChunk(HeaderChunk: TChunkPngImageHeader);
    procedure DisplayPaletteChunk(PaletteChunk: TChunkPngPalette);
    procedure DisplayChromaticitiesChunk(ChromaChunk: TChunkPngPrimaryChromaticities);
    procedure DisplayGammaChunk(GammaChunk: TChunkPngGamma);
    procedure DisplayPhysicalDimensionsChunk(PhysicalDimensionsChunk: TChunkPngPhysicalPixelDimensions);
    procedure DisplayTextChunk(TextChunk: TCustomChunkPngText);
    procedure DisplaySuggestedPaletteChunk(SuggestedPaletteChunk: TChunkPngSuggestedPalette);
    procedure DisplaySignificantBitsChunk(SignificantBitsChunk: TChunkPngSignificantBits);
    procedure DisplayStandardColorSpaceRGBChunk(StandardColorSpaceRGB: TChunkPngStandardColorSpaceRGB);
    procedure DisplayBackgroundColorChunk(BackgroundColor: TChunkPngBackgroundColor);
    procedure DisplayTransparencyChunk(TransparencyChunk: TChunkPngTransparency);
    procedure DisplayHistogramChunk(HistogramChunk: TChunkPngImageHistogram);
    procedure DisplayTimeChunk(TimeChunk: TChunkPngTime);
  public
    procedure LoadFromFile(Filename: TFileName);
    procedure LoadFromStream(Stream: TStream);
  end;

var
  FmPngExplorer: TFmPngExplorer;

implementation

{$R *.dfm}

uses
  Inifiles, Math, Types;

{ TFmPngExplorer }

procedure TFmPngExplorer.FormCreate(Sender: TObject);
begin
  Application.OnHint := ShowHint;
  FPngFile := TMyPortableNetworkGraphic.Create;
end;

procedure TFmPngExplorer.FormDestroy(Sender: TObject);
begin
  // free png file
  FreeAndNil(FPngFile);
end;

procedure TFmPngExplorer.FormShow(Sender: TObject);
begin
  if FileExists(ParamStr(1)) then
    LoadFromFile(ParamStr(1));
end;

procedure TFmPngExplorer.InitializeDefaultListView;
begin
  // add columns
  ListViewColumns(['Name', 'Value']);
end;

procedure TFmPngExplorer.ListViewColumns(Columns: array of string);
var
  ColumnIndex : Integer;
begin
  // clear list view
  ListView.Clear;

  // clear columns
  ListView.Columns.Clear;

  // add column
  for ColumnIndex := 0 to Length(Columns) - 1 do
    with ListView.Columns.Add do
    begin
      Caption := Columns[ColumnIndex];
      Width := Min(256, (ListView.Width - 16) div (Length(Columns)));
      MinWidth := 64;
      AutoSize := True;
    end;
end;

procedure TFmPngExplorer.ListViewData(Strings: array of string);
var
  ValueIndex : Integer;
begin
  // add data
  with ListView.Items.Add do
  begin
    Caption := Strings[0];
    for ValueIndex := 1 to Length(Strings) - 1 do
      SubItems.Add(Strings[ValueIndex]);
  end;
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

procedure TFmPngExplorer.DisplayHeaderChunk(HeaderChunk: TChunkPngImageHeader);
begin
  with HeaderChunk do
  begin
    InitializeDefaultListView;

    ListViewData(['Width', IntToStr(Width)]);
    ListViewData(['Height', IntToStr(Height)]);
    ListViewData(['Bit Depth', IntToStr(BitDepth)]);
    ListViewData(['Color Type', ColorTypeToString(ColorType)]);
    ListViewData(['Compression Method', IntToStr(CompressionMethod)]);
    ListViewData(['Filter Method', 'Adaptive']);
    ListViewData(['Interlace Method', InterlaceMethodToString(InterlaceMethod)]);
    ListViewData(['HasPallette', BoolToStr(HasPalette)]);

    ListView.BringToFront;
  end;
end;

procedure TFmPngExplorer.DisplayPaletteChunk(PaletteChunk: TChunkPngPalette);
var
  Index : Integer;
begin
  with PaletteChunk do
  begin
    InitializeDefaultListView;

    ListViewColumns(['Index', 'Color']);

    for Index := 0 to Count - 1 do
      with PaletteEntry[Index] do
        ListViewData([IntToStr(Index), '#' + IntToHex(
          Integer(R shl 16 + G shl 8 + B), 6)]);

    ListView.BringToFront;
  end;
end;

procedure TFmPngExplorer.DisplayGammaChunk(GammaChunk: TChunkPngGamma);
begin
  with GammaChunk do
  begin
    InitializeDefaultListView;

    ListViewData(['Gamma', FloatToStr(GammaAsSingle)]);

    ListView.BringToFront;
  end;
end;

procedure TFmPngExplorer.DisplayHistogramChunk(HistogramChunk: TChunkPngImageHistogram);
var
  Index : Integer;
begin
  with HistogramChunk do
  begin
    ListViewColumns(['Index', 'Frequency']);

    for Index := 0 to Count - 1 do
      ListViewData([IntToStr(Index), IntToStr(Frequency[Index])]);

    ListView.BringToFront;
  end;
end;

procedure TFmPngExplorer.DisplaySuggestedPaletteChunk(SuggestedPaletteChunk: TChunkPngSuggestedPalette);
begin
  with SuggestedPaletteChunk do
  begin
    InitializeDefaultListView;

//    ListViewData(['Palette Entries', IntToStr(Count)]);

    ListView.BringToFront;
  end;
end;

procedure TFmPngExplorer.DisplaySignificantBitsChunk(
  SignificantBitsChunk: TChunkPngSignificantBits);
begin
  with SignificantBitsChunk do
  begin
    InitializeDefaultListView;

    if SignificantBits is TPngSignificantBitsFormat0 then
      ListViewData(['Greyscale Bits',
        IntToStr(TPngSignificantBitsFormat0(SignificantBits).GrayBits)])
    else if SignificantBits is TPngSignificantBitsFormat23 then
    begin
      ListViewData(['Red Bits',
        IntToStr(TPngSignificantBitsFormat23(SignificantBits).RedBits)]);
      ListViewData(['Green Bits',
        IntToStr(TPngSignificantBitsFormat23(SignificantBits).GreenBits)]);
      ListViewData(['Blue Bits',
        IntToStr(TPngSignificantBitsFormat23(SignificantBits).BlueBits)]);
    end
    else if SignificantBits is TPngSignificantBitsFormat4 then
    begin
      ListViewData(['Greyscale Bits',
        IntToStr(TPngSignificantBitsFormat4(SignificantBits).GrayBits)]);
      ListViewData(['Alpha Bits',
        IntToStr(TPngSignificantBitsFormat4(SignificantBits).AlphaBits)]);
    end
    else if SignificantBits is TPngSignificantBitsFormat6 then
    begin
      ListViewData(['Red Bits',
        IntToStr(TPngSignificantBitsFormat6(SignificantBits).RedBits)]);
      ListViewData(['Green Bits',
        IntToStr(TPngSignificantBitsFormat6(SignificantBits).GreenBits)]);
      ListViewData(['Blue Bits',
        IntToStr(TPngSignificantBitsFormat6(SignificantBits).BlueBits)]);
      ListViewData(['Alpha Bits',
        IntToStr(TPngSignificantBitsFormat6(SignificantBits).AlphaBits)]);
    end;

    ListView.BringToFront;
  end;
end;


procedure TFmPngExplorer.DisplayStandardColorSpaceRGBChunk(
  StandardColorSpaceRGB: TChunkPngStandardColorSpaceRGB);
begin
  with StandardColorSpaceRGB do
  begin
    InitializeDefaultListView;

    case RenderingIntent of
      0 : ListViewData(['Rendering Indent', 'Perceptual']);
      1 : ListViewData(['Rendering Indent', 'Relative Colorimetric']);
      2 : ListViewData(['Rendering Indent', 'Saturation']);
      3 : ListViewData(['Rendering Indent', 'Absolute Colorimetric']);
      else
        ListViewData(['Rendering Indent', IntToStr(RenderingIntent)]);
    end;

    ListView.BringToFront;
  end;
end;

procedure TFmPngExplorer.DisplayTextChunk(TextChunk: TCustomChunkPngText);
begin
  with TextChunk do
  begin
    InitializeDefaultListView;

    ListViewData([string(Keyword), string(Text)]);

    ListView.BringToFront;
  end;
end;

procedure TFmPngExplorer.DisplayTimeChunk(TimeChunk: TChunkPngTime);
begin
  with TimeChunk do
  begin
    InitializeDefaultListView;

    ListViewData(['Time', DateTimeToStr(ModifiedDateTime)]);

    ListView.BringToFront;
  end;
end;

procedure TFmPngExplorer.DisplayTransparencyChunk(
  TransparencyChunk: TChunkPngTransparency);
var
  Index : Integer;
begin
  with TransparencyChunk do
  begin
    InitializeDefaultListView;

    if Transparency is TPngTransparencyFormat0 then
      ListViewData(['Grey Sample Value',
        IntToStr(TPngTransparencyFormat0(Transparency).GraySampleValue)])
    else if Transparency is TPngTransparencyFormat2 then
    begin
      ListViewData(['Red Sample Value',
        IntToStr(TPngTransparencyFormat2(Transparency).RedSampleValue)]);
      ListViewData(['Blue Sample Value',
        IntToStr(TPngTransparencyFormat2(Transparency).BlueSampleValue)]);
      ListViewData(['Green Sample Value',
        IntToStr(TPngTransparencyFormat2(Transparency).GreenSampleValue)]);
    end
    else if Transparency is TPngTransparencyFormat3 then
      for Index := 0 to TPngTransparencyFormat3(Transparency).Count - 1 do
        ListViewData(['Index ' + IntToStr(Index),
          IntToStr(TPngTransparencyFormat3(Transparency).Transparency[Index])]);

    ListView.BringToFront;
  end;
end;

procedure TFmPngExplorer.DisplayPhysicalDimensionsChunk(PhysicalDimensionsChunk: TChunkPngPhysicalPixelDimensions);
begin
  with PhysicalDimensionsChunk do
  begin
    InitializeDefaultListView;

    ListViewData(['Pixels per unit X', IntToStr(PixelsPerUnitX)]);
    ListViewData(['Pixels per unit Y', IntToStr(PixelsPerUnitY)]);

    ListView.BringToFront;
  end;
end;

procedure TFmPngExplorer.DisplayBackgroundColorChunk(
  BackgroundColor: TChunkPngBackgroundColor);
begin
  with BackgroundColor do
  begin
    InitializeDefaultListView;

    if Background is TPngBackgroundColorFormat04 then
      ListViewData(['Grey',
        IntToStr(TPngBackgroundColorFormat04(Background).GraySampleValue)])
    else if Background is TPngBackgroundColorFormat26 then
    begin
      ListViewData(['Red',
        IntToStr(TPngBackgroundColorFormat26(Background).RedSampleValue)]);
      ListViewData(['Blue',
        IntToStr(TPngBackgroundColorFormat26(Background).BlueSampleValue)]);
      ListViewData(['Green',
        IntToStr(TPngBackgroundColorFormat26(Background).GreenSampleValue)]);
    end
    else if Background is TPngBackgroundColorFormat3 then
      ListViewData(['Palette Index',
        IntToStr(TPngBackgroundColorFormat3(Background).PaletteIndex)]);

    ListView.BringToFront;
  end;
end;

procedure TFmPngExplorer.DisplayChromaticitiesChunk(ChromaChunk: TChunkPngPrimaryChromaticities);
begin
  with ChromaChunk do
  begin
    InitializeDefaultListView;

    ListViewData(['White X', FloatToStr(WhiteXAsSingle)]);
    ListViewData(['White Y', FloatToStr(WhiteYAsSingle)]);
    ListViewData(['Red X', FloatToStr(RedXAsSingle)]);
    ListViewData(['Red Y', FloatToStr(RedYAsSingle)]);
    ListViewData(['Green X', FloatToStr(GreenXAsSingle)]);
    ListViewData(['Green Y', FloatToStr(GreenYAsSingle)]);
    ListViewData(['Blue X', FloatToStr(BlueXAsSingle)]);
    ListViewData(['Blue Y', FloatToStr(BlueYAsSingle)]);

    ListView.BringToFront;
  end;
end;

procedure TFmPngExplorer.TreeViewMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  MouseOverNode : TTreeNode;
  ParentNode    : TTreeNode;
begin
 MouseOverNode := TreeView.GetNodeAt(X, Y);

 // status bar
 if Assigned(MouseOverNode) and Assigned(MouseOverNode.Data) then
  begin
   ParentNode := MouseOverNode;

   while not (TObject(ParentNode.Data) is TCustomDefinedChunk) do
    begin
     ParentNode := ParentNode.Parent;
     if not Assigned(ParentNode.Data)
      then Exit;
    end;

   if TObject(ParentNode.Data) is TCustomDefinedChunkWithHeader then
    with TCustomDefinedChunkWithHeader(ParentNode.Data) do
     begin
//      StatusBar.SimpleText := 'Table ID: ' + TableType;
     end;
  end;
end;

procedure TFmPngExplorer.TreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  Node.Expanded := True;

  if Assigned(Node.Data) then
  begin
    // display chunk size
    if TObject(Node.Data) is TCustomDefinedChunk then
      StatusBar.SimpleText := 'Chunk Size: ' + IntToStr(
        TCustomDefinedChunk(Node.Data).ChunkSize);

    // PNG HeaderChunk chunk
    if TObject(Node.Data) is TChunkPngImageHeader then
      DisplayHeaderChunk(TChunkPngImageHeader(Node.Data))
    else if TObject(Node.Data) is TChunkPngPalette then
      DisplayPaletteChunk(TChunkPngPalette(Node.Data))
    else if TObject(Node.Data) is TChunkPngGamma then
      DisplayGammaChunk(TChunkPngGamma(Node.Data))
    else if TObject(Node.Data) is TChunkPngTime then
      DisplayTimeChunk(TChunkPngTime(Node.Data))
(*
    else if TObject(Node.Data) is TChunkPngPhysicalScale then
      DisplayPhysicalScaleChunk(TChunkPngPhysicalScale(Node.Data))
*)
    else if TObject(Node.Data) is TCustomChunkPngText then
      DisplayTextChunk(TCustomChunkPngText(Node.Data))
    else if TObject(Node.Data) is TChunkPngStandardColorSpaceRGB then
      DisplayStandardColorSpaceRGBChunk(TChunkPngStandardColorSpaceRGB(Node.Data))
    else if TObject(Node.Data) is TChunkPngImageHistogram then
      DisplayHistogramChunk(TChunkPngImageHistogram(Node.Data))
    else if TObject(Node.Data) is TChunkPngBackgroundColor then
      DisplayBackgroundColorChunk(TChunkPngBackgroundColor(Node.Data))
    else if TObject(Node.Data) is TChunkPngSuggestedPalette then
      DisplaySuggestedPaletteChunk(TChunkPngSuggestedPalette(Node.Data))
    else if TObject(Node.Data) is TChunkPngPrimaryChromaticities then
      DisplayChromaticitiesChunk(TChunkPngPrimaryChromaticities(Node.Data))
    else if TObject(Node.Data) is TChunkPngPhysicalPixelDimensions then
      DisplayPhysicalDimensionsChunk(TChunkPngPhysicalPixelDimensions(Node.Data))
    else if TObject(Node.Data) is TChunkPngSignificantBits then
      DisplaySignificantBitsChunk(TChunkPngSignificantBits(Node.Data))
    else if TObject(Node.Data) is TChunkPngTransparency then
      DisplayTransparencyChunk(TChunkPngTransparency(Node.Data))
    else

    // other unregistered chunks
    if TObject(Node.Data) is TCustomChunk then
      with TCustomChunk(Node.Data) do
      begin
        InitializeDefaultListView;

        ListViewData(['Chunk Name', string(ChunkNameAsString)]);
        ListViewData(['Chunk Size', IntToStr(ChunkSize)]);

        ListView.BringToFront;
      end;

    PnPaintBox.Visible := False;
    SpVertical.Visible := False;
  end
  else
  begin
    PnPaintBox.Visible := True;
    SpVertical.Visible := True;
  end;
end;

procedure TFmPngExplorer.PNGChanged;
var
  Index : Integer;
begin
  with FPngFile, TreeView do
  begin
    // begin update
    Items.BeginUpdate;

    // add PNG Header chunk
    Items.AddChildObject(Items[0], 'IHDR', FImageHeader);

    // eventually add Palette chunk
    if Assigned(FPaletteChunk) then
      Items.AddChildObject(Items[0], 'PLTE', FPaletteChunk);

    // eventually add PNG Gamma chunk
    if Assigned(FGammaChunk) then
      Items.AddChildObject(Items[0], 'gAMA', FGammaChunk);

    // eventually add PNG Time chunk
    if Assigned(FTimeChunk) then
      Items.AddChildObject(Items[0], 'tIME', FTimeChunk);

    // eventually add PNG Background chunk
    if Assigned(FBackgroundChunk) then
      Items.AddChildObject(Items[0], 'bKGD', FBackgroundChunk);

    // eventually add PNG Significant Bits chunk
    if Assigned(FSignificantBits) then
      Items.AddChildObject(Items[0], 'sBIT', FSignificantBits);

    // eventually add PNG Transparency chunk
    if Assigned(FTransparencyChunk) then
      Items.AddChildObject(Items[0], 'tRNS', FTransparencyChunk);

    // eventually add PNG Chroma chunk
    if Assigned(FChromaChunk) then
      Items.AddChildObject(Items[0], 'cHRM', FChromaChunk);

    // eventually add PNG Physical Pixel Dimensions chunk
    if Assigned(FPhysicalDimensions) then
      Items.AddChildObject(Items[0], 'pHYs', FPhysicalDimensions);

    // eventually add additional chunks
    for Index := 0 to FAdditionalChunkList.Count - 1 do
    begin
      if FAdditionalChunkList[Index] is TCustomChunk then
        Items.AddChildObject(Items[0],
          string(TCustomChunk(FAdditionalChunkList[Index]).ChunkNameAsString),
          FAdditionalChunkList[Index])
    end;

    // end update
    Items.EndUpdate;

    // assign png file
    ImgView32.Bitmap.Assign(FPngFile);

    // expand tree
    Items[0].Expanded := True;
  end;
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
