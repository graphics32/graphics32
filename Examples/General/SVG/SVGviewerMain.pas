unit SVGviewerMain;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1 or LGPL 2.1 with linking exception
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * Alternatively, the contents of this file may be used under the terms of the
 * Free Pascal modified version of the GNU Lesser General Public License
 * Version 2.1 (the "FPC modified LGPL License"), in which case the provisions
 * of this license are applicable instead of those above.
 * Please see the file LICENSE.txt for additional information concerning this
 * license.
 *
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Anders Melander <anders@melander.dk>
 *
 * Portions created by the Initial Developer are Copyright (C) 2026
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

{-$define IMAGE32}

uses
{$IFDEF FPC}
  LCLIntf, LCLType, LResources,
{$ELSE}
  Windows,
  ImageList,
  ImgList,
{$ENDIF}
  Messages,
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  Buttons,
  ShellCtrls,
{$IFNDEF FPC}
  FileCtrl, // Delphi will add it since the controls are on the form.
{$ENDIF}

{$if defined(IMAGE32)}
  Img32.Panels,
{$ifend}

  GR32,
  GR32_System,
  GR32.SVG.Tree,
  GR32.SVG.Renderer,
  GR32.Layers.SVG,
  GR32_Layers,
  GR32_Image;

type
  TSvgColorTheme = (ctNone, ctLight, ctDark);

const
  MSG_AFTER_SHOW = WM_USER;

// Interposer for TImageList.ColorDepth
{$IFDEF FPC}
type
  TImageList = class(Controls.TImageList)
  private type
    TColorDepth = (cdDefault, cdDeviceDependent, cd4Bit, cd8Bit, cd16Bit, cd24Bit, cd32Bit);
  private
    procedure SetDummy(Value: TColorDepth);
  published
    property ColorDepth: TColorDepth write SetDummy;
  end;
{$ENDIF}

// Interposers to replace the horrible old Win3.x shell controls with
// "modern" (meaning this century) shell controls
type
  TDirectoryListBox = class;
  TFileListBox = class;

  TDirectoryListBox = class(TShellTreeView)
  published
    property Align;
  end;

  TFileListBox = class(TShellListView)
  private
    procedure SetDummyInt(const Value: integer);
  published
    property ItemHeight: integer write SetDummyInt;
  end;

type
  TFormSVGviewer = class(TForm)
    PnlLeft: TPanel;
    DirectoryListBox: TDirectoryListBox;
    FileListBox: TFileListBox;
    SplitterMain: TSplitter;
    PnlRight: TPanel;
    Image32: TImage32;
    PageControlSVG: TPageControl;
    TabSheetPreview: TTabSheet;
    TabSheetSource: TTabSheet;
    MemoSource: TMemo;
    ButtonSave: TButton;
    StatusBar: TStatusBar;
    TabSheetDump: TTabSheet;
    MemoDump: TMemo;
    MemoSource2: TMemo;
    SplitterMemo: TSplitter;
    TabSheetImage32: TTabSheet;
    Splitter1: TSplitter;
    Panel1: TPanel;
    SpeedButtonTheme: TSpeedButton;
    ImageList: TImageList;
    SpeedButtonBackground: TSpeedButton;
    SpeedButtonRepaint: TSpeedButton;
    SpeedButtonExternal: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MemoSourceChange(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure SplitterMemoCanResize(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
    procedure MemoSourceKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure SpeedButtonThemeClick(Sender: TObject);
    procedure SpeedButtonBackgroundClick(Sender: TObject);
    procedure SpeedButtonRepaintClick(Sender: TObject);
    procedure SpeedButtonExternalClick(Sender: TObject);
    procedure Image32PaintStage(Sender: TObject; Buffer: TBitmap32; StageNum: Cardinal);
    procedure Image32MouseEnter(Sender: TObject);
    procedure Image32MouseLeave(Sender: TObject);
  private
    FDocNode: TSvgDocumentNode;
    FLockUpdate: integer;
    FIgnoreClick: boolean;
    FColorTheme: TSvgColorTheme;
    FFilename: string;
    FLayer: TSvgLayer;
    FRubberbandLayer: TRubberbandLayer;
    FStopWatchLayer: TStopWatch;

{$if defined(IMAGE32)}
    FImage32Panel: TImage32Panel;
{$ifend}
    procedure RenderSvg(const ASource: string);
    procedure LoadAndRenderSvg(const AFileName: string);
    procedure SplitterClicked(Sender: TObject);

    procedure MsgAfterShow(var Msg: TMessage); message MSG_AFTER_SHOW;
    procedure SetColorTheme(const Value: TSvgColorTheme);
    function GetBackground: TBackgroundCheckerStyle;
    procedure SetBackground(Value: TBackgroundCheckerStyle);
    procedure ApplyColorTheme(Renderer: TSvgRenderer);

    procedure SplitterMemoBeforeResize(Sender: TObject);
{$IFNDEF FPC}
    procedure ShellListViewAddFolder(Sender: TObject; AFolder: TShellFolder; var CanAdd: Boolean);
{$ELSE}
    procedure ShellListViewAddItem(Sender: TObject; const ABasePath: String; const AFileInfo: TSearchRec; var CanAdd: Boolean);
{$ENDIF}
    procedure ShellListViewOnSelected(Sender: TObject; Item: TListItem; Selected: Boolean);

    procedure ResolveExternalFileHandler(Sender: TObject; const Filename: string; Stream: TStream; var Allow: boolean);
  public
    property ColorTheme: TSvgColorTheme read FColorTheme write SetColorTheme;
    property Background: TBackgroundCheckerStyle read GetBackground write SetBackground;
  end;

var
  FormSVGviewer: TFormSVGviewer;

implementation

uses
  IniFiles,
  System.Types,
  System.Math,
{$IFNDEF FPC}
  IOUtils,
{$ENDIF}
{$if defined(IMAGE32)}
  Img32.SVG.Reader,
  Img32.SVG.Path,
  Img32.SVG.Core,
{$ifend}
  GR32.Platform,
  GR32.SVG.Types;

{$R *.dfm}

const
  sColorThemes: array[TSvgColorTheme] of string = ('Default', 'Light', 'Dark');
  sIniFilename = 'GR32_svg_viewer.ini';

type
  TControlCracker = class(TCustomControl);

procedure TFormSVGviewer.FormCreate(Sender: TObject);
var
  i: integer;
  Stage: PPaintStage;
begin
  FDocNode := nil;

  Image32.MouseZoom.ConstrainPivot := False;
  // Insert a custom paint stage just before and right after the layer paint
  // stage so we can time the rendering of the svg layer.
  // The presence of a PST_CUSTOM stages causes the OnPaintStage event to execute.
  for i := 0 to Image32.PaintStages.Count-1 do
    if (Image32.PaintStages[i].Stage = PST_DRAW_LAYERS) then
    begin
      // Add after
      Stage := Image32.PaintStages.Insert(i+1);
      Stage.Stage := PST_CUSTOM;
      Stage.Parameter := 1; // Indicate "after"
      // Add before
      Stage := Image32.PaintStages.Insert(i);
      Stage.Stage := PST_CUSTOM;
      Stage.Parameter := 0; // Indicate "before"
      break;
    end;

  FLayer := TSvgLayer.Create(Image32.Layers);
  FLayer.Scaled := True;
  FLayer.MouseEvents := True;
  FLayer.Renderer.OnResolveExternalFile := ResolveExternalFileHandler;

  FRubberbandLayer := TRubberbandLayer.Create(Image32.Layers);
  FRubberbandLayer.ChildLayer := FLayer;
  FRubberbandLayer.Visible := False;
  FRubberbandLayer.HandleStyle := hsCircle;
  FRubberbandLayer.HandleSize := 5;
  FRubberbandLayer.HandleFrameSize := 2.0;
  FRubberbandLayer.HandleFrame := clCornFlowerBlue32;
  FRubberbandLayer.HandleFill := clTrWhite32;

  TControlCracker(SplitterMemo).OnClick := SplitterClicked;

{$if defined(IMAGE32)}
  FImage32Panel := TImage32Panel.Create(Self);
  FImage32Panel.BkgType := pbtChessBoard;
  FImage32Panel.Parent := TabSheetImage32;
  FImage32Panel.Align := alClient;
  TabSheetImage32.TabVisible := True;
{$ifend}

  DirectoryListBox.ShellListView := FileListBox;
  DirectoryListBox.ObjectTypes := [otFolders];
  DirectoryListBox.ReadOnly := True;
  DirectoryListBox.HideSelection := False;
  DirectoryListBox.RowSelect := True;
  DirectoryListBox.Indent := 10;
  DirectoryListBox.ShowLines := False;
  DirectoryListBox.ShowRoot := False;

  FileListBox.ShellTreeView := DirectoryListBox;
  // Prevents AV when we set the other properties. These controls are really half-baked
  FileListBox.CreateRoot;
  FileListBox.ObjectTypes := [otNonFolders];
  FileListBox.ViewStyle := vsReport;
  FileListBox.ReadOnly := True;
  FileListBox.HideSelection := False;
  FileListBox.RowSelect := True;
{$IFNDEF FPC}
  FileListBox.OnAddFolder := ShellListViewAddFolder;
{$ELSE}
  FileListBox.OnAddItem := ShellListViewAddItem;
{$ENDIF}
  FileListBox.OnSelectItem := ShellListViewOnSelected;

{$IFNDEF FPC}
  SplitterMemo.OnBeforeResize := SplitterMemoBeforeResize;
{$ENDIF}

  PageControlSVG.TabIndex := 0;
  ColorTheme := ctNone;
end;

procedure TFormSVGviewer.FormDestroy(Sender: TObject);
var
  IniFile: TIniFile;
begin
  FLayer.SvgDocument := nil;
  FDocNode.Free;

  IniFile := TIniFile.Create(TPath.Combine(TPath.GetDirectoryName(Application.ExeName), sIniFilename));
  try
    if (FFilename <> '') then
      IniFile.WriteString('settings', 'LastFile', FFilename);
  finally
    IniFile.Free;
  end;
end;

procedure TFormSVGviewer.FormShow(Sender: TObject);
begin
  PostMessage(Handle, MSG_AFTER_SHOW, 0, 0);
end;

function TFormSVGviewer.GetBackground: TBackgroundCheckerStyle;
begin
  if (FColorTheme = ctNone) then
    Result := Image32.Background.CheckersStyle
  else
    Result := bcsNone;
end;

procedure TFormSVGviewer.Image32MouseEnter(Sender: TObject);
begin
  FRubberbandLayer.Visible := FLayer.Visible;
end;

procedure TFormSVGviewer.Image32MouseLeave(Sender: TObject);
begin
  FRubberbandLayer.Visible := False;
end;

procedure TFormSVGviewer.Image32PaintStage(Sender: TObject; Buffer: TBitmap32; StageNum: Cardinal);
begin
  case Image32.PaintStages[StageNum].Parameter of
    0: // Before layer
      FStopWatchLayer := FStopWatchLayer.StartNew;

    1: // After layer
      begin
        if (not FStopWatchLayer.IsRunning) then
          exit;
        FStopWatchLayer.Stop;
        if (FLayer <> nil) and (FLayer.Visible) then
        begin
          StatusBar.Panels[1].Text := Format('Render: %d mS', [FStopWatchLayer.ElapsedMilliseconds]); StatusBar.Update;
 {$if declared(SvgVertexCount)} // Declared in GR32.SVG.Tree if BENCHMARK_FLATTEN is defined
          StatusBar.Panels[3].Text := Format('Vertex count: %.0n', [SvgVertexCount * 1.0]); StatusBar.Update;
{$ifend}
        end;
    end;
  end;
end;

procedure TFormSVGviewer.ButtonSaveClick(Sender: TObject);
var
  Filename: string;
  Bitmap: TBitmap32;
  Renderer: TSvgRenderer;
  NaturalWidth, NaturalHeight: Single;
begin
  if (FFilename = '') then
    exit;
  Filename := TPath.ChangeExtension(FFilename, '.png');

  Bitmap := TBitmap32.Create;
  try
    // Convert width and height presentation attributes to pixels
    NaturalWidth := FDocNode.Width.ToPixels(600, Self.Monitor.PixelsPerInch);
    NaturalHeight := FDocNode.Height.ToPixels(800, Self.Monitor.PixelsPerInch);

    // Fall back to ViewBox dimensions if width or height are unspecified (0 or %)
    if (NaturalWidth <= 0) then
    begin
      if FDocNode.ViewBox.IsValid then
        NaturalWidth := FDocNode.ViewBox.Width
      else
        NaturalWidth := 600;
    end;

    if (NaturalHeight <= 0) then
    begin
      if FDocNode.ViewBox.IsValid then
        NaturalHeight := FDocNode.ViewBox.Height
      else
        NaturalHeight := 800;
    end;

    Bitmap.SetSize(Round(NaturalWidth), Round(NaturalHeight));

    Renderer := TSvgRenderer.Create;
    try
      Renderer.OnResolveExternalFile := ResolveExternalFileHandler;
      ApplyColorTheme(Renderer);
      Renderer.AllowExternalImages := FLayer.Renderer.AllowExternalImages;

      Renderer.RenderDocument(Bitmap, FDocNode);
    finally
      Renderer.Free;
    end;

    Bitmap.SaveToFile(Filename);
  finally
    Bitmap.Free;
  end;
  StatusBar.Panels[4].Text := 'Saved to: '+Filename;
end;

procedure TFormSVGviewer.LoadAndRenderSvg(const AFileName: string);
var
  Stream: TStringStream;
begin
  StatusBar.Panels[4].Text := 'Loading'; StatusBar.Update;
  Stream := TStringStream.Create('', TEncoding.UTF8);
  try
    Stream.LoadFromFile(AFileName);
    FFilename := AFilename;

    RenderSvg(Stream.DataString);

  finally
    Stream.Free;
  end;
end;

procedure TFormSVGviewer.MemoSourceChange(Sender: TObject);
begin
  if (FLockUpdate > 0) then
    exit;

  Inc(FLockUpdate);
  try
    if (Sender = MemoSource) then
    begin
      RenderSvg(MemoSource.Text);
      MemoSource2.Text := MemoSource.Text;
    end else
    if (Sender = MemoSource2) then
    begin
      RenderSvg(MemoSource2.Text);
      MemoSource.Text := MemoSource2.Text;
    end;
  finally
    Dec(FLockUpdate);
  end;
end;

procedure TFormSVGviewer.MemoSourceKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = ^A) then
  begin
    TMemo(Sender).SelStart := Length(TMemo(Sender).Text);
    TMemo(Sender).Perform(EM_SCROLLCARET, 0, 0);
    TMemo(Sender).SelectAll;
    Key := #0;
  end;
end;

procedure TFormSVGviewer.MsgAfterShow(var Msg: TMessage);
var
  IniFile: TIniFile;
  LastFile, LastFolder: string;
  i: integer;

{$ifndef FPC}
var
  Filename, Param: string;
  Count: integer;
  Benchmark: boolean;
{$endif}
begin
  Update;

{$ifndef FPC}
  Benchmark := False;

  if (FindCmdLineSwitch('file', Filename, True, [clstValueAppended])) then
  begin
    if (FindCmdLineSwitch('maximize')) then
      WindowState := wsMaximized;

    Count := 1;
    if (FindCmdLineSwitch('benchmark', Param, True, [clstValueAppended])) then
    begin
      Count := StrToIntDef(Param, Count);
      Benchmark := True;
      Inc(FLockUpdate);
    end;

    while (Count > 0) do
    begin
      LoadAndRenderSvg(Filename);

      if (Benchmark) then
        Caption := IntToStr(Count);

      Update;
      Dec(Count);
    end;
  end;

  if (Benchmark) then
  begin
    Application.Terminate;
    exit;
  end;
{$endif}

  // Reload previous file/folder
  IniFile := TIniFile.Create(TPath.Combine(TPath.GetDirectoryName(Application.ExeName), sIniFilename));
  try
    LastFile := IniFile.ReadString('settings', 'LastFile', '');
  finally
    IniFile.Free;
  end;

  if (LastFile <> '') and (TFile.Exists(LastFile)) then
    LastFolder := TPath.GetDirectoryName(LastFile)
  else
  begin
    LastFile := '';
    LastFolder := GetCurrentDir;
  end;

  DirectoryListBox.Path := LastFolder;

  if (LastFile <> '') then
  begin
    for i := 0 to FileListBox.Items.Count-1 do
      if SameText(LastFile, FileListBox.Folders[i].PathName) then
      begin
        FileListBox.ItemIndex := i;
        FileListBox.Items[i].MakeVisible(False);
        FileListBox.SetFocus;
        break;
      end;
  end else
    DirectoryListBox.SetFocus;
end;

procedure TFormSVGviewer.RenderSvg(const ASource: string);
var
  xmlText: UTF8String;
  ErrorMessage: string;
  StopWatch: TStopWatch;
  LayerRect, ViewportRect: TFloatRect;
  NaturalWidth, NaturalHeight: Single;
  Scale: Single;
const
  Margin = 8;
begin
  Screen.Cursor := crAppStart;
  try
    FLayer.SvgDocument := nil;
    FLayer.Visible := False;
    FRubberbandLayer.Visible := False;
    FreeAndNil(FDocNode);
    MemoDump.Lines.Clear;
    if (FLockUpdate = 0) then
    begin
      Inc(FLockUpdate);
      try
        MemoSource.Lines.Clear;
        MemoSource2.Lines.Clear;
      finally
        Dec(FLockUpdate);
      end;
    end;

    xmlText := UTF8String(ASource);

    Image32.Scale := 1.0;
    // Layer scaling via TImage32.Scale (and by extension mouse zoom) doesn't work
    // unless we have a bitmap, so create a transparent one
    Image32.SetupBitmap(True, 0);
    Image32.ScrollToCenter;

    ViewportRect := Image32.GetViewportRect;

    StatusBar.Panels[0].Text := '';
    StatusBar.Panels[1].Text := '';
    StatusBar.Panels[2].Text := '';
    StatusBar.Panels[3].Text := '';
    StatusBar.Panels[4].Text := '';

    try
      (*
      ** Parse svg
      *)
      StatusBar.Panels[4].Text := 'Parsing'; StatusBar.Update;
      StopWatch := TStopWatch.StartNew;
      begin

        FDocNode := ParseSvgXml(xmlText, ErrorMessage);

      end;
      StopWatch.Stop;
      StatusBar.Panels[0].Text := Format('Parse: %d mS', [StopWatch.ElapsedMilliseconds]); StatusBar.Update;


      (*
      ** Display svg info
      *)
      // Display source
      if (FLockUpdate = 0) then
      begin
        Inc(FLockUpdate);
        try
          // We're using Memo.Lines.Text instead of Memo.Text in order to handle Unix linebreaks
          MemoSource.Lines.Text := string(xmlText);
          MemoSource2.Lines.Text := string(xmlText);
        finally
          Dec(FLockUpdate);
        end;
      end;

      if FDocNode = nil then
      begin
        StatusBar.Panels[4].Text := ErrorMessage;;
        exit;
      end;

      // Convert width and height presentation attributes to pixels
      NaturalWidth := FDocNode.Width.ToPixels(ViewportRect.Width, Self.Monitor.PixelsPerInch);
      NaturalHeight := FDocNode.Height.ToPixels(ViewportRect.Height, Self.Monitor.PixelsPerInch);

      // Fall back to ViewBox dimensions if width or height are unspecified (0 or %)
      if (NaturalWidth <= 0) and FDocNode.ViewBox.IsValid then
        NaturalWidth := FDocNode.ViewBox.Width;

      if (NaturalHeight <= 0) and FDocNode.ViewBox.IsValid then
        NaturalHeight := FDocNode.ViewBox.Height;

      StatusBar.Panels[2].Text := Format('Size: %.1n x %.1n', [NaturalWidth, NaturalHeight]); StatusBar.Update;

      // Dump svg AST info
      MemoDump.Text := FDocNode.Dump;


      (*
      ** Setup svg layer
      *)
      // Scale to TImage32 viewport...
      Scale := Min((ViewportRect.Width - Margin) / NaturalWidth, (ViewportRect.Height - Margin) / NaturalHeight);

      // ...and center svg viewport in TImage32 viewport
      LayerRect := FloatRect(0, 0, NaturalWidth * Scale, NaturalHeight * Scale);
      GR32.OffsetRect(LayerRect, (ViewportRect.Width - LayerRect.Width) / 2, (ViewportRect.Height - LayerRect.Height) / 2);
      FRubberbandLayer.Location := LayerRect;
      FRubberbandLayer.Visible := True;

      (*
      ** Render svg
      *)
      StatusBar.Panels[4].Text := 'Rendering'; StatusBar.Update;
      FLayer.SvgDocument := FDocNode;

      // Repaint TImage32 -> render svg layer
      FLayer.Visible := True;
      Image32.Update;

      // The render stopwatch is started when the first custom do-nothing
      // paint stage is executed and stopped when the second executes.
      // If the stopwatch is still running now, it means the layer render,
      // which happens in-between, failed.
      if (FStopWatchLayer.IsRunning) then
        StatusBar.Panels[1].Text := 'Render: Failed';
      FStopWatchLayer.Stop;

      StatusBar.Panels[4].Text := 'Done';
    except
      on E: Exception do
        StatusBar.Panels[4].Text := ErrorMessage;
    end;

{$if defined(IMAGE32)}
    try
      var Stream := TStringStream.Create(xmlText);
      try

        // Adapted from Img32.Fmt.SVG
        with TSvgReader.Create do
        try
          StopWatch := TStopWatch.StartNew;
          if (LoadFromStream(stream)) then
          begin
            var TimeParse := StopWatch.ElapsedMilliseconds;
            var TimeRender: Int64;

            var r := RootElement.viewboxWH;
            FImage32Panel.Image.BeginUpdate;
            try
              var sx := GetScaleForBestFit(r.Width, r.Height, FImage32Panel.ClientWidth, FImage32Panel.ClientHeight);
              // This is cheating... but whatever
              FImage32Panel.Image.SetSize(Round(r.Width * sx), Round(r.Height * sx));

              StopWatch := TStopWatch.StartNew;
              DrawImage(FImage32Panel.Image, True);
              TimeRender := StopWatch.ElapsedMilliseconds;
            finally
              FImage32Panel.Image.EndUpdate;
            end;
            StatusBar.Panels[4].Text := Format('Image32: Parsed in %d mS, Rendered in %d mS', [TimeParse, TimeRender]);
          end else
            StatusBar.Panels[4].Text := 'Image32: Parse failed';
        finally
          Free;
        end;
      finally
        Stream.Free;
      end;
    except
      on E: Exception do
        StatusBar.Panels[4].Text := Format('Image32: %s', [E.Message]);
    end;
{$ifend}

  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormSVGviewer.ResolveExternalFileHandler(Sender: TObject; const Filename: string; Stream: TStream; var Allow: boolean);
var
  DocumentFolder: string;
  ExternalFilename: string;
  FileStream: TFileStream;
begin
  DocumentFolder := TPath.GetDirectoryName(FFilename);
  ExternalFilename := TPath.Combine(DocumentFolder, Filename, False);

  if (not TFile.Exists(ExternalFilename)) then
    exit;

  if (Stream is TMemoryStream) then
    TMemoryStream(Stream).LoadFromFile(ExternalFilename)
  else
  begin
    FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      Stream.CopyFrom(FileStream);
    finally
      FileStream.Free;
    end;
  end;
  Allow := True;
end;

procedure TFormSVGviewer.SetBackground(Value: TBackgroundCheckerStyle);
begin
  if (Value = bcsCustom) then
    Value := bcsNone;

  FColorTheme := ctNone;

  Image32.Background.FillStyle := bfsCheckers;
  Image32.Background.CheckersStyle := Value;
  Image32.ParentColor := True;
end;

procedure TFormSVGviewer.ApplyColorTheme(Renderer: TSvgRenderer);
begin
  // Modern/Well-authored SVGs use "currentColor".
  // Legacy/Hard-coded single-color SVGs will have to use Theme colors.
  case FColorTheme of
    ctLight:
      begin
        Renderer.CurrentColor32 := $FF111827; // Remember to also set the Alpha
        (*
        FRenderer.ThemeFillColor32 := clLightGray32;
        FRenderer.ThemeStrokeColor32 := clBlack32;
        *)
      end;

    ctDark:
      begin
        Renderer.CurrentColor32 := $FFFBFAF9; // Remember to also set the Alpha
        (*
        FRenderer.ThemeFillColor32 := clDarkGray32;
        FRenderer.ThemeStrokeColor32 := clWhite32;
        *)
      end;
  else
    Renderer.ClearThemeColors;
  end;
end;

procedure TFormSVGviewer.SetColorTheme(const Value: TSvgColorTheme);
begin
  FColorTheme := Value;

  SpeedButtonTheme.Hint := Format('Color theme: %s', [sColorThemes[FColorTheme]]);

  ApplyColorTheme(FLayer.Renderer);

  case FColorTheme of
    ctLight:
      begin
        // Dark features...
        // ...on light background
        Image32.Color := $E1FFD7;
        Image32.Background.CheckersStyle := bcsNone;
        Image32.Background.FillStyle := bfsColor;
        SpeedButtonBackground.Enabled := False;
      end;

    ctDark:
      begin
        // Light features...
        // ...on dark background
        Image32.Color := $6A2B00;
        Image32.Background.CheckersStyle := bcsNone;
        Image32.Background.FillStyle := bfsColor;
        SpeedButtonBackground.Enabled := False;
      end;
  else
    SetBackground(bcsLight);
    SpeedButtonBackground.Enabled := True;
  end;

  FLayer.Update;
end;

procedure TFormSVGviewer.SpeedButtonBackgroundClick(Sender: TObject);
const
  Styles: array[0..3] of TBackgroundCheckerStyle = (bcsNone, bcsLight, bcsMedium, bcsDark);
begin
  TSpeedButton(Sender).Tag := (TSpeedButton(Sender).Tag + 1) mod 4;
  Background := Styles[TSpeedButton(Sender).Tag];
end;

procedure TFormSVGviewer.SpeedButtonExternalClick(Sender: TObject);
begin
  FLayer.Renderer.AllowExternalImages := TSpeedButton(Sender).Down;
end;

procedure TFormSVGviewer.SpeedButtonRepaintClick(Sender: TObject);
begin
  MemoSourceChange(MemoSource);
end;

procedure TFormSVGviewer.SpeedButtonThemeClick(Sender: TObject);
begin
  TSpeedButton(Sender).ImageIndex := (TSpeedButton(Sender).ImageIndex + 1) mod (Ord(High(FColorTheme)) + 1);
  ColorTheme := TSvgColorTheme(TSpeedButton(Sender).ImageIndex);
end;

procedure TFormSVGviewer.SplitterClicked(Sender: TObject);
begin
  if (FIgnoreClick) then
    exit;
  MemoSource2.Visible := not MemoSource2.Visible;
end;

procedure TFormSVGviewer.SplitterMemoBeforeResize(Sender: TObject);
begin
  FIgnoreClick := False;
end;

procedure TFormSVGviewer.SplitterMemoCanResize(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
begin
  FIgnoreClick := True;
end;

{$IFNDEF FPC}
procedure TFormSVGviewer.ShellListViewAddFolder(Sender: TObject; AFolder: TShellFolder; var CanAdd: Boolean);
var
  FileExt: string;
begin
  CanAdd := not AFolder.IsFolder;
  if CanAdd then
  begin
    FileExt := ExtractFileExt(AFolder.PathName);
    CanAdd := (FileExt = '.svg');
  end;
end;
{$ELSE}
procedure TFormSVGviewer.ShellListViewAddItem(Sender: TObject; const ABasePath: String; const AFileInfo: TSearchRec;
  var CanAdd: Boolean);
var
  FileExt: string;
begin
  CanAdd := (AFileInfo.Attr and faAnyFile <> 0);
  if CanAdd then
  begin
    FileExt := ExtractFileExt(AFileInfo.Name);
    CanAdd := (FileExt = '.svg');
  end;
end;
{$ENDIF}

procedure TFormSVGviewer.ShellListViewOnSelected(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if (Selected) then
    LoadAndRenderSvg(FileListBox.SelectedFolder.PathName);
end;

{ TFileListBox }

procedure TFileListBox.SetDummyInt(const Value: integer);
begin
end;

{$IFDEF FPC}
procedure TImageList.SetDummy(Value: TColorDepth);
begin
end;
{$ENDIF}
end.
