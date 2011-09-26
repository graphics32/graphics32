unit PNGExplorerMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ComCtrls, StdCtrls, ExtCtrls, ToolWin, ActnList, StdActns, AppEvnts,
  ImgList, GR32, GR32_PortableNetworkGraphic, GR32_PNG, GR32_Image;

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
    SpVertical: TSplitter;
    ImgView32: TImgView32;
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
    procedure DisplaySignificantBitsChunk(SignificantBitsChunk: TChunkPngSignificantBits);
    procedure DisplayTextChunk(TextChunk: TChunkPngText);
    procedure DisplaySuggestedPaletteChunk(SuggestedPaletteChunk: TChunkPngSuggestedPalette);
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
var
  Index : Integer;
begin
  with SuggestedPaletteChunk do
  begin
    InitializeDefaultListView;

//    ListViewData(['Palette Entries', IntToStr(Count)]);

    ListView.BringToFront;
  end;
end;

procedure TFmPngExplorer.DisplayTextChunk(TextChunk: TChunkPngText);
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

procedure TFmPngExplorer.DisplaySignificantBitsChunk(SignificantBitsChunk: TChunkPngSignificantBits);
begin
  with SignificantBitsChunk do
  begin
    InitializeDefaultListView;

    ListViewData(['Width', IntToStr(Width)]);

    ListView.BringToFront;
  end;
end;

procedure TFmPngExplorer.DisplayPhysicalDimensionsChunk(PhysicalDimensionsChunk: TChunkPngPhysicalPixelDimensions);
begin
  with PhysicalDimensionsChunk do
  begin
    InitializeDefaultListView;

    ListViewData(['Width', IntToStr(Width)]);

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
    else if TObject(Node.Data) is TChunkPngText then
      DisplayTextChunk(TChunkPngText(Node.Data))
    else if TObject(Node.Data) is TChunkPngImageHistogram then
      DisplayHistogramChunk(TChunkPngImageHistogram(Node.Data))
    else if TObject(Node.Data) is TChunkPngSuggestedPalette then
      DisplaySuggestedPaletteChunk(TChunkPngSuggestedPalette(Node.Data))
    else if TObject(Node.Data) is TChunkPngPrimaryChromaticities then
      DisplayChromaticitiesChunk(TChunkPngPrimaryChromaticities(Node.Data))
    else if TObject(Node.Data) is TChunkPngPhysicalPixelDimensions then
      DisplayPhysicalDimensionsChunk(TChunkPngPhysicalPixelDimensions(Node.Data))
    else if TObject(Node.Data) is TChunkPngSignificantBits then
      DisplaySignificantBitsChunk(TChunkPngSignificantBits(Node.Data))
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

    // add PNG HeaderChunk chunk
    Items.AddChildObject(Items[0], 'IHDR', FImageHeader);

    // eventually add PaletteChunk chunk
    if Assigned(FPaletteChunk) then
      Items.AddChildObject(Items[0], 'PLTE', FPaletteChunk);

    // eventually add PNG GammaChunk chunk
    if Assigned(FGammaChunk) then
      Items.AddChildObject(Items[0], 'gAMA', FGammaChunk);

    // eventually add PNG TimeChunk chunk
    if Assigned(FTimeChunk) then
      Items.AddChildObject(Items[0], 'tIME', FTimeChunk);

    // eventually add PNG chroma chunk
    if Assigned(FChromaChunk) then
      Items.AddChildObject(Items[0], 'cHRM', FChromaChunk);

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
 else raise Exception.Create('File does not exists');
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
