unit MainUnit;

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
 * The Original Code is RenderText Example
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2012
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Christian-W. Budde
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

{$define DEBUG_VERTICES}

uses
  {$IFDEF FPC} LCLType, LResources, LMessages, {$ELSE} Windows, Messages, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  Buttons,
  GR32, GR32_Image;

{$if defined(FPC)}
type
  TMessage = TLMessage;
{$ifend}

type
  TFormRenderText = class(TForm)
    PanelTop: TPanel;
    LabelText: TLabel;
    EditText: TEdit;
    Image: TImage32;
    CheckBoxAntiAlias: TCheckBox;
    CheckBoxCanvas32: TCheckBox;
    CheckBoxBold: TCheckBox;
    CheckBoxItalic: TCheckBox;
    LabelFont: TLabel;
    ComboBoxFont: TComboBox;
    ButtonBenchmark: TButton;
    Bevel1: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure ButtonBenchmarkClick(Sender: TObject);
    procedure ImageResize(Sender: TObject);
    procedure Changed(Sender: TObject);
    procedure CheckBoxCanvas32Click(Sender: TObject);
    procedure ImageClick(Sender: TObject);
  private
{$if defined(DEBUG_VERTICES)}
    FDisplayVertices: boolean;
{$ifend}
    function GetFontStyle: TFontStyles;
  protected
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
  public
    procedure Draw;
  end;

var
  FormRenderText: TFormRenderText;

implementation

{$R *.dfm}

uses
  Types,
{$if defined(DEBUG_VERTICES)}
  Math,
{$ifend}
  GR32_Paths,
  GR32_Brushes,
{$if defined(DEBUG_VERTICES)}
  GR32_Transforms,
{$ifend}
  GR32_Polygons,
  GR32_System,
  GR32.Text.Types;

{$if defined(DEBUG_VERTICES)}
type
  TVertexBrush = class(TCustomBrush)
  private
    FBitmap: TBitmap32;
  protected
    procedure RenderPolyPolygon(Renderer: TCustomPolygonRenderer; const Points: TArrayOfArrayOfFloatPoint;
      const ClipRect: TFloatRect; Transformation: TTransformation); override;
    public
    constructor Create(ABrushCollection: TBrushCollection; ABitmap: TBitmap32); reintroduce;
  end;

constructor TVertexBrush.Create(ABrushCollection: TBrushCollection; ABitmap: TBitmap32);
begin
  inherited Create(ABrushCollection);
  FBitmap := ABitmap;
end;

procedure TVertexBrush.RenderPolyPolygon(Renderer: TCustomPolygonRenderer; const Points: TArrayOfArrayOfFloatPoint;
  const ClipRect: TFloatRect; Transformation: TTransformation);
var
  i, j: integer;
const
  Size = 1;
begin
  for i := 0 to High(Points) do
    for j := 0 to High(Points[i]) do
      FBitmap.FrameRectTS(GR32.MakeRect(Points[i, j].X-Size, Points[i, j].Y-Size, Points[i, j].X+Size, Points[i, j].Y+Size), clTrRed32);
end;
{$ifend}


function TFormRenderText.GetFontStyle: TFontStyles;
begin
  Result := [];
  if CheckBoxBold.Checked then
    Include(Result, fsBold);

  if CheckBoxItalic.Checked then
    Include(Result, fsItalic);
end;

procedure TFormRenderText.FormCreate(Sender: TObject);
var
  i: integer;
begin
  Image.SetupBitmap;

  ComboBoxFont.Items.BeginUpdate;
  try
    ComboBoxFont.Items.Assign(Screen.Fonts);
    for i := ComboBoxFont.Items.Count-1 downto 0 do
      if (Copy(ComboBoxFont.Items[i], 1, 1) = '@') then
        ComboBoxFont.Items.Delete(i);
  finally
    ComboBoxFont.Items.EndUpdate;
  end;

  if Screen.Fonts.IndexOf('Segoe UI') <> -1 then
    Image.Bitmap.Font.Name := 'Segoe UI'
  else
    Image.Bitmap.Font.Name := 'Tahoma';

  ComboBoxFont.Text := Image.Bitmap.Font.Name;

  // Use classic metrics since we're comparing against ExtTextOut
  // (via TBitmap32.RenderText->TBitmap32.Textout->Backend.Textout)
  DefaultTextLayout.VerticalMetrics := vmWindows;
end;

procedure TFormRenderText.Draw;
var
  Y: integer;
  Height: integer;
  Size: integer;
  Canvas: TCanvas32;
  Brush: TSolidBrush;
begin
  Image.Bitmap.Clear;

  Image.Bitmap.Font.Style := GetFontStyle;
  Image.Bitmap.Font.Name := ComboBoxFont.Text;

  Canvas := nil;
  try
    if CheckboxCanvas32.Checked then
    begin
      Canvas := TCanvas32.Create(Image.Bitmap);

      Brush := TSolidBrush.Create(Canvas.Brushes);
      Brush.FillColor := clWhite32;
      Brush.FillMode := pfNonZero;

{$if defined(DEBUG_VERTICES)}
      if FDisplayVertices then
        TVertexBrush.Create(Canvas.Brushes, Image.Bitmap);
{$ifend}
    end;

    Y := 3;
    Size := 6;

    while (Y < Image.Bitmap.Height) do
    begin

      Image.Bitmap.Font.Size := Size;

      // Draw reference line
      Image.Bitmap.LineTS(0, Y, Image.Bitmap.Width, Y, $80204060);

      if (Canvas <> nil) then
        Canvas.RenderText(10, Y, Format('%d: %s', [Size, EditText.Text]), DT_SINGLELINE)
      else
        Image.Bitmap.RenderText(10, Y, Format('%d: %s', [Size, EditText.Text]), clWhite32, CheckBoxAntiAlias.Checked);

      Height := Image.Bitmap.TextHeight(EditText.Text);
      Y := Y + MulDiv(Height, 4, 5);

      Size := Round(Size * 1.2);

    end;
  finally
    Canvas.Free;
  end;
end;

procedure TFormRenderText.ImageClick(Sender: TObject);
begin
{$if defined(DEBUG_VERTICES)}
  FDisplayVertices := not FDisplayVertices;
  Draw;
{$ifend}
end;

procedure TFormRenderText.ImageResize(Sender: TObject);
begin
  Image.SetupBitmap;
  Draw;
end;

procedure TFormRenderText.ButtonBenchmarkClick(Sender: TObject);
var
  SaveQuality: TFontQuality;
  i: Integer;
  Str: string;
  StopWatch: TStopWatch;
  r: TRect;
  Size: TSize;
  Canvas: TCanvas32;
  Brush: TSolidBrush;
  SaveFont: string;
  Color: TColor32;
  Pos: TPoint;
const
  MinSize = 10;
  MaxSize = 30;
  MaxCount = 10000;
begin
  Screen.Cursor := crHourGlass;

  SaveQuality := Image.Bitmap.Font.Quality;
  SaveFont := Image.Bitmap.Font.Name;

  Image.Bitmap.Font.Style := GetFontStyle;

  if (CheckBoxAntiAlias.Checked) then
    Image.Bitmap.Font.Quality := TFontQuality.fqAntialiased
  else
    Image.Bitmap.Font.Quality := TFontQuality.fqNonAntialiased;

  Canvas := nil;
  Brush := nil;
  try
    if CheckboxCanvas32.Checked then
    begin
      Canvas := TCanvas32.Create(Image.Bitmap);
      Brush := TSolidBrush(Canvas.Brushes.Add(TSolidBrush));
      Brush.FillMode := pfNonZero;
    end;

    StopWatch := TStopWatch.StartNew;
    Image.Bitmap.BeginUpdate;

    for i := 1 to MaxCount do
    begin
      Image.Bitmap.Font.Size := MinSize + MulDiv(MaxSize-MinSize, i, MaxCount);
      Color := Color32(Random(255), Random(255), Random(255), 64+Random(191));
      Pos.X := Random(Image.Bitmap.Width + 10) - 10;
      Pos.Y := Random(Image.Bitmap.Height + 10) - 10;

      if (Canvas <> nil) then
      begin
        Brush.FillColor := Color;
        Canvas.RenderText(Pos.X, Pos.Y, IntToStr(i), DT_SINGLELINE);
      end else
        Image.Bitmap.RenderText(
          Pos.X,
          Pos.Y,
          IntToStr(i),
          Color);
    end;

    Image.Bitmap.EndUpdate;
    StopWatch.Stop;

  finally
    Canvas.Free;
  end;

  Image.Bitmap.Font.Name := 'Verdana';
  Image.Bitmap.Font.Style := [];
  Image.Bitmap.Font.Size := 8;
  Image.Bitmap.Font.Quality := SaveQuality;
  Image.Bitmap.Font.Color := clWhite;

  str := Format('  %.0n mS ', [StopWatch.ElapsedMilliseconds * 1.0]);

  Size := Image.Bitmap.TextExtent(str);

  r := Image.Bitmap.BoundsRect;
  r.Left := r.Right - Size.cx;
  r.Top := r.Bottom - Size.cy;

  Image.Bitmap.FillRectS(r, clBlack32);
  Image.Bitmap.Textout(r.Left, r.Top, str);

  Image.Bitmap.Font.Name := SaveFont;

  Screen.Cursor := crDefault;
  Image.Invalidate;
end;

procedure TFormRenderText.Changed(Sender: TObject);
begin
  Draw;
end;

procedure TFormRenderText.CheckBoxCanvas32Click(Sender: TObject);
begin
  CheckBoxAntiAlias.Enabled := not CheckBoxCanvas32.Checked;
  Update;
  Draw;
end;

procedure TFormRenderText.CMShowingChanged(var Message: TMessage);
var
  i: integer;
begin
  inherited;

  if Visible and FindCmdLineSwitch('benchmark') then
  begin
    CheckBoxCanvas32.Checked := True;
    Update;

    for i := 20 downto 1 do
    begin
      Caption := IntToStr(i);
      Update;

      ButtonBenchmark.Click;
      Update;
    end;

    Application.Terminate
  end;
end;

end.
