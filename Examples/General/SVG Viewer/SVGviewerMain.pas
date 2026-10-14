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

{$IFNDEF FPC}
{$define MODERN_SHELL_CONTROLS}
{$ENDIF}

uses
  {$IFNDEF FPC} Windows, FileCtrl, {$ELSE} LCLIntf, LCLType, LResources, FileCtrl, {$ENDIF}
  Messages,
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Vcl.ComCtrls,
  System.ImageList, Vcl.ImgList, Vcl.Buttons,

{$IFDEF MODERN_SHELL_CONTROLS}
  ShellCtrls,
{$ENDIF}

{$if defined(IMAGE32)}
  Img32.Panels,
{$ifend}

  GR32,
  GR32.SVG.Tree,
  GR32.SVG.Renderer,
  GR32_Image;

type
  TSvgColorTheme = (ctNone, ctLight, ctDark);

const
  MSG_AFTER_SHOW = WM_USER;

// Interposers to replace the horrible old Win3.x shell controls with
// "modern" (meaning this century) shell controls
{$IFDEF MODERN_SHELL_CONTROLS}
type
  TDirectoryListBox = class;
  TFileListBox = class;

  TDriveComboBox = class(TShellComboBox)
  private
    procedure SetDirList(const Value: TDirectoryListBox);
  published
    property Align;
    property DirList: TDirectoryListBox write SetDirList;
  end;

  TDirectoryListBox = class(TShellTreeView)
  private
    procedure SetFileList(const Value: TFileListBox);
  published
    property Align;
    property FileList: TFileListBox write SetFileList;
  end;

  TFileListBox = class(TShellListView)
  private
    function GetFilename: string;
    procedure SetDummyInt(const Value: integer);
    procedure SetDummyString(const Value: string);
  public
    property Filename: string read GetFilename;
  published
    property ItemHeight: integer write SetDummyInt;
    property Mask: string write SetDummyString;
  end;
{$ENDIF}

type
  TFormSVGviewer = class(TForm)
    PnlLeft: TPanel;
    DriveComboBox: TDriveComboBox;
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
    procedure FileListBoxChange(Sender: TObject);
    procedure MemoSourceChange(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure SplitterMemoCanResize(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
    procedure SplitterMemoBeforeResize(Sender: TObject);
    procedure MemoSourceKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure SpeedButtonThemeClick(Sender: TObject);
    procedure SpeedButtonBackgroundClick(Sender: TObject);
    procedure SpeedButtonRepaintClick(Sender: TObject);
    procedure SpeedButtonExternalClick(Sender: TObject);
  private
    FDocNode: TSvgDocumentNode;
    FRenderer: TSvgRenderer;
    FLockUpdate: integer;
    FIgnoreClick: boolean;
    FColorTheme: TSvgColorTheme;
{$if defined(IMAGE32)}
    FImage32Panel: TImage32Panel;
{$ifend}
    procedure DoRender;
    procedure RenderSvg(const ASource: string);
    procedure LoadAndRenderSvg(const AFileName: string);
    procedure SplitterClicked(Sender: TObject);

    procedure MsgAfterShow(var Msg: TMessage); message MSG_AFTER_SHOW;
    procedure SetColorTheme(const Value: TSvgColorTheme);
    function GetBackground: TBackgroundCheckerStyle;
    procedure SetBackground(Value: TBackgroundCheckerStyle);

{$IFDEF MODERN_SHELL_CONTROLS}
    procedure ShellListViewAddFolder(Sender: TObject; AFolder: TShellFolder; var CanAdd: Boolean);
    procedure ShellListViewOnChange(Sender: TObject; Item: TListItem; Change: TItemChange);
{$ENDIF}
  public
    property ColorTheme: TSvgColorTheme read FColorTheme write SetColorTheme;
    property Background: TBackgroundCheckerStyle read GetBackground write SetBackground;
  end;

var
  FormSVGviewer: TFormSVGviewer;

implementation

uses
  IOUtils,
  IniFiles,
{$if defined(IMAGE32)}
  Img32.SVG.Reader,
  Img32.SVG.Path,
  Img32.SVG.Core,
{$ifend}
  GR32_System,
  GR32.SVG.Types;

{$R *.dfm}

const
  sColorThemes: array[TSvgColorTheme] of string = ('Default', 'Light', 'Dark');
  sIniFilename = 'GR32_svg_viewer.ini';

type
  TControlCracker = class(TControl);

procedure TFormSVGviewer.FormCreate(Sender: TObject);
var
  IniFile: TIniFile;
begin
  FDocNode := nil;
  Image32.Bitmap.SetSize(600, 600, False);
  Image32.Bitmap.Clear(clTrWhite32); // Just so we have something to look at
  Image32.ScrollToCenter;
  FRenderer := TSvgRenderer.Create(Image32.Bitmap);

  TControlCracker(SplitterMemo).OnClick := SplitterClicked;

{$if defined(IMAGE32)}
  FImage32Panel := TImage32Panel.Create(Self);
  FImage32Panel.BkgType := pbtChessBoard;
  FImage32Panel.Parent := TabSheetImage32;
  FImage32Panel.Align := alClient;
  TabSheetImage32.TabVisible := True;
{$ifend}

  IniFile := TIniFile.Create(TPath.Combine(TPath.GetDirectoryName(Application.ExeName), sIniFilename));
  try
{$IFDEF MODERN_SHELL_CONTROLS}
    FreeAndNil(DriveComboBox);

    DirectoryListBox.ShellListView := FileListBox;
    DirectoryListBox.ObjectTypes := [otFolders];
    DirectoryListBox.ReadOnly := True;
    DirectoryListBox.HideSelection := False;
    DirectoryListBox.RowSelect := True;
    DirectoryListBox.Indent := 10;
    DirectoryListBox.ShowLines := False;
    DirectoryListBox.ShowRoot := False;

    FileListBox.ShellTreeView := DirectoryListBox;
    FileListBox.ObjectTypes := [otNonFolders];

    FileListBox.ViewStyle := vsReport;
    FileListBox.ReadOnly := True;
    FileListBox.HideSelection := False;
    FileListBox.RowSelect := True;
    FileListBox.OnAddFolder := ShellListViewAddFolder;
    FileListBox.OnChange := ShellListViewOnChange;

    DirectoryListBox.Path := IniFile.ReadString('settings', 'LastFolder', GetCurrentDir);
{$ELSE}
    DirectoryListBox.Directory := IniFile.ReadString('settings', 'LastFolder', GetCurrentDir);
{$ENDIF}
  finally
    IniFile.Free;
  end;

  PageControlSVG.TabIndex := 0;
  ColorTheme := ctNone;
end;

procedure TFormSVGviewer.FormDestroy(Sender: TObject);
var
  IniFile: TIniFile;
begin
  FDocNode.Free;
  FRenderer.Free;

  IniFile := TIniFile.Create(TPath.Combine(TPath.GetDirectoryName(Application.ExeName), sIniFilename));
  try
{$IFDEF MODERN_SHELL_CONTROLS}
    IniFile.WriteString('settings', 'LastFolder', DirectoryListBox.Path);
{$ELSE}
    IniFile.WriteString('settings', 'LastFolder', DirectoryListBox.Directory);
{$ENDIF}
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

procedure TFormSVGviewer.ButtonSaveClick(Sender: TObject);
var
  Filename: string;
begin
  Filename := TPath.ChangeExtension(FileListBox.FileName, '.png');
  Image32.Bitmap.SaveToFile(Filename);
  StatusBar.SimpleText := 'Saved to: '+Filename;
end;

procedure TFormSVGviewer.DoRender;
begin
  Image32.Bitmap.Clear;
  FRenderer.RenderDocument(FDocNode, Image32.GetBitmapRect);
end;

procedure TFormSVGviewer.FileListBoxChange(Sender: TObject);
begin
  if (FileListBox.FileName <> '') and FileExists(FileListBox.FileName) then
    LoadAndRenderSvg(FileListBox.FileName);
end;

procedure TFormSVGviewer.LoadAndRenderSvg(const AFileName: string);
var
  xmlList: TStringList;
begin
  xmlList := TStringList.Create;
  try
    xmlList.LoadFromFile(AFileName, TEncoding.UTF8);

    RenderSvg(xmlList.Text);

  finally
    xmlList.Free;
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
  Filename, Param: string;
  Count: integer;
  Benchmark: boolean;
begin
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
    Application.Terminate;
end;

procedure TFormSVGviewer.RenderSvg(const ASource: string);
var
  xmlText: UTF8String;
  ErrorMessage: string;
  StopWatch: TStopWatch;
  TimeParse, TimeRender: Int64;
  NaturalWidth, NaturalHeight: Single;
  s: string;
begin
  Screen.Cursor := crAppStart;
  try
    if FDocNode <> nil then
      FreeAndNil(FDocNode);

    xmlText := UTF8String(ASource);

    Image32.Scale := 1.0;
    Image32.SetupBitmap(True, 0);

    Image32.ScrollToCenter;

    TimeParse := 0;
    TimeRender := 0;
    try
      StopWatch := TStopWatch.StartNew;

      FDocNode := ParseSvgXml(xmlText, ErrorMessage);

      if FDocNode = nil then
      begin
        StatusBar.SimpleText := ErrorMessage;

        Inc(FLockUpdate);
        try
          MemoSource.Lines.Clear;
          MemoSource2.Lines.Clear;
        finally
          Dec(FLockUpdate);
        end;
        MemoDump.Lines.Clear;
        Exit;
      end;

      TimeParse := StopWatch.ElapsedMilliseconds;

      if (FLockUpdate = 0) then
      begin
        Inc(FLockUpdate);
        try
          MemoSource.Text := string(xmlText);
          MemoSource2.Text := string(xmlText);
        finally
          Dec(FLockUpdate);
        end;
      end;

      StopWatch := TStopWatch.StartNew;
      DoRender;
      TimeRender := StopWatch.ElapsedMilliseconds;

    except
      on E: Exception do
        ErrorMessage := E.Message;
    end;

    MemoDump.Text := FDocNode.Dump;

    // Convert width and height presentation attributes to pixels
    NaturalWidth := FDocNode.Width.ToPixels(Image32.Bitmap.Width, Self.Monitor.PixelsPerInch);
    NaturalHeight := FDocNode.Height.ToPixels(Image32.Bitmap.Height, Self.Monitor.PixelsPerInch);

    // Fall back to ViewBox dimensions if width or height are unspecified (0 or %)
    if (NaturalWidth <= 0) and FDocNode.ViewBox.IsValid then
      NaturalWidth := FDocNode.ViewBox.Width;

    if (NaturalHeight <= 0) and FDocNode.ViewBox.IsValid then
      NaturalHeight := FDocNode.ViewBox.Height;

    if (ErrorMessage <> '') then
      s := ErrorMessage + ': '
    else
      s := '';
    s := s + Format('Parsed in %d mS, Rendered in %d mS. Size: %.1n x %.1n', [TimeParse, TimeRender, NaturalWidth, NaturalHeight]);

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
            TimeParse := StopWatch.ElapsedMilliseconds;

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
            s := s + Format(' - Image32: Parsed in %d mS, Rendered in %d mS', [TimeParse, TimeRender]);
          end else
            s := s + ' - Image32: Parse failed';
        finally
          Free;
        end;
      finally
        Stream.Free;
      end;
    except
      on E: Exception do
        s := s + Format(' - Image32: %s', [E.Message]);
    end;
{$ifend}

    StatusBar.SimpleText := s;

  finally
    Screen.Cursor := crDefault;
  end;
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

procedure TFormSVGviewer.SetColorTheme(const Value: TSvgColorTheme);
begin
  FColorTheme := Value;

  SpeedButtonTheme.Hint := Format('Color theme: %s', [sColorThemes[FColorTheme]]);

  // Modern/Well-authored SVGs use "currentColor".
  // Legacy/Hard-coded single-color SVGs will have to use Theme colors.
  case FColorTheme of
    ctLight:
      begin
        // Dark features...
        FRenderer.CurrentColor32 := $FF111827; // Remember to also set the Alpha
        (*
        FRenderer.ThemeFillColor32 := clLightGray32;
        FRenderer.ThemeStrokeColor32 := clBlack32;
        *)
        // ...on light background
        Image32.Color := $E1FFD7;
        Image32.Background.CheckersStyle := bcsNone;
        Image32.Background.FillStyle := bfsColor;
        SpeedButtonBackground.Enabled := False;
      end;

    ctDark:
      begin
        // Light features...
        FRenderer.CurrentColor32 := $FFFBFAF9; // Remember to also set the Alpha
        (*
        FRenderer.ThemeFillColor32 := clDarkGray32;
        FRenderer.ThemeStrokeColor32 := clWhite32;
        *)
        // ...on dark background
        Image32.Color := $6A2B00;
        Image32.Background.CheckersStyle := bcsNone;
        Image32.Background.FillStyle := bfsColor;
        SpeedButtonBackground.Enabled := False;
      end;
  else
    FRenderer.ClearThemeColors;
    SetBackground(bcsLight);
    SpeedButtonBackground.Enabled := True;
  end;

  DoRender;
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
  FRenderer.AllowExternalImages := TSpeedButton(Sender).Down;
end;

procedure TFormSVGviewer.SpeedButtonRepaintClick(Sender: TObject);
begin
  MemoSourceChange(MemoSource);
end;

procedure TFormSVGviewer.SpeedButtonThemeClick(Sender: TObject);
begin
  TSpeedButton(Sender).ImageIndex := (TSpeedButton(Sender).ImageIndex + 1) mod (Ord(High(FColorTheme)) + 1);
  ColorTheme := TSvgColorTheme(TSpeedButton(Sender).ImageIndex);
  DoRender;
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

{$IFDEF MODERN_SHELL_CONTROLS}
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

procedure TFormSVGviewer.ShellListViewOnChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  FileListBoxChange(Sender);
end;

{ TFileListBox }

function TFileListBox.GetFilename: string;
begin
  if (SelectedFolder <> nil) then
    Result := SelectedFolder.PathName
  else
    Result := '';
end;

procedure TFileListBox.SetDummyInt(const Value: integer);
begin
end;

procedure TFileListBox.SetDummyString(const Value: string);
begin
end;

{ TDriveComboBox }

procedure TDriveComboBox.SetDirList(const Value: TDirectoryListBox);
begin
  ShellTreeView := Value;
end;

{ TDirectoryListBox }

procedure TDirectoryListBox.SetFileList(const Value: TFileListBox);
begin
  ShellListView := Value;
end;

{$ENDIF}

end.
