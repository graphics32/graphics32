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

uses
  {$IFNDEF FPC} Windows, FileCtrl, {$ELSE} LCLIntf, LCLType, LResources, FileCtrl, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Vcl.ComCtrls,

  GR32,
  GR32.SVG.Tree,
  GR32_Image;

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
    Button1: TButton;
    StatusBar: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FileListBoxChange(Sender: TObject);
    procedure MemoSourceChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FDocNode: TSvgDocumentNode;
    FLockUpdate: boolean;
    procedure RenderSvg(const ASource: string);
    procedure LoadAndRenderSvg(const AFileName: string);
  end;

var
  FormSVGviewer: TFormSVGviewer;

implementation

uses
  IOUtils,
  GR32_System,
  GR32.SVG.Types,
  GR32.SVG.Renderer;

{$R *.dfm}

procedure TFormSVGviewer.FormCreate(Sender: TObject);
begin
  FDocNode := nil;
  Image32.Bitmap.SetSize(600, 600, False);
  Image32.Bitmap.Clear(clTrWhite32); // Just so we have something to look at
  Image32.ScrollToCenter;
end;

procedure TFormSVGviewer.FormDestroy(Sender: TObject);
begin
  if FDocNode <> nil then
    FreeAndNil(FDocNode);
end;

procedure TFormSVGviewer.Button1Click(Sender: TObject);
begin
  Image32.Bitmap.SaveToFile(TPath.ChangeExtension(FileListBox.FileName, '.png'));
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

    FLockUpdate := True;
    try
      MemoSource.Text := xmlList.Text;
    finally
      FLockUpdate := False;
    end;
    RenderSvg(xmlList.Text);
  finally
    xmlList.Free;
  end;
end;

procedure TFormSVGviewer.MemoSourceChange(Sender: TObject);
begin
  if (FLockUpdate) then
    exit;
  RenderSvg(MemoSource.Text);
end;

procedure TFormSVGviewer.RenderSvg(const ASource: string);
var
  xmlText: UTF8String;
  renderer: TSvgRenderer;
  drawWidth, drawHeight: Integer;
  ErrorMessage: string;
  StopWatch: TStopWatch;
  TimeParse, TimeRender: Int64;
begin
  if FDocNode <> nil then
    FreeAndNil(FDocNode);

  xmlText := UTF8String(ASource);

  drawWidth := Image32.ClientWidth;
  drawHeight := Image32.ClientHeight;
  if drawWidth <= 0 then
    drawWidth := 600;
  if drawHeight <= 0 then
    drawHeight := 600;

  Image32.Bitmap.SetSize(drawWidth, drawHeight);
  Image32.ScrollToCenter;

  StopWatch := TStopWatch.StartNew;

  FDocNode := ParseSvgXml(xmlText, ErrorMessage);
  if FDocNode = nil then
  begin
    StatusBar.SimpleText := ErrorMessage;
    Exit;
  end;

  TimeParse := StopWatch.ElapsedMilliseconds;

  renderer := TSvgRenderer.Create(Image32.Bitmap);
  try
    StopWatch := TStopWatch.StartNew;

    renderer.RenderDocument(FDocNode, FloatRect(0, 0, drawWidth, drawHeight));

    TimeRender := StopWatch.ElapsedMilliseconds;
  finally
    renderer.Free;
  end;

  StatusBar.SimpleText := Format('Parsed in %d mS, Rendered in %d mS', [TimeParse, TimeRender]);
end;

end.
