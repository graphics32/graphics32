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

uses
  {$IFDEF FPC} LCLType, LResources, {$ELSE} Windows, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  Buttons,
  GR32, GR32_Image;

type
  TFormRenderText = class(TForm)
    BtnRunBenchmark: TButton;
    EditText: TEdit;
    Image: TImage32;
    LblEnterText: TLabel;
    PnlControl: TPanel;
    CheckBoxAntiAlias: TCheckBox;
    CheckBoxCanvas32: TCheckBox;
    ComboBoxFont: TComboBox;
    Label1: TLabel;
    CheckBoxBold: TCheckBox;
    CheckBoxItalic: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure BtnRunBenchmarkClick(Sender: TObject);
    procedure EditTextChange(Sender: TObject);
    procedure ImageResize(Sender: TObject);
    procedure CheckBoxAntiAliasClick(Sender: TObject);
    procedure CheckBoxCanvas32Click(Sender: TObject);
    procedure ComboBoxFontChange(Sender: TObject);
    procedure CheckBoxBoldClick(Sender: TObject);
    procedure CheckBoxItalicClick(Sender: TObject);
  public
    procedure Draw;
  end;

var
  FormRenderText: TFormRenderText;

implementation

{$R *.dfm}

uses
  Types,
  GR32_Paths,
  GR32_Brushes,
  GR32_Polygons,
  GR32_System;

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
end;

procedure TFormRenderText.Draw;
var
  y: integer;
  Height: integer;
  Size: integer;
  Style: TFontStyles;
  Canvas: TCanvas32;
  Brush: TSolidBrush;
begin
  Image.Bitmap.Clear;

  y := 3;
  Size := 6;

  Image.Bitmap.Font.Size := 20;

  Style := [];

  if CheckBoxBold.Checked then
    Include(Style, fsBold);

  if CheckBoxItalic.Checked then
    Include(Style, fsItalic);

  Image.Bitmap.Font.Style := Style;

  Canvas := nil;
  try
    if CheckboxCanvas32.Checked then
    begin
      Canvas := TCanvas32.Create(Image.Bitmap);
      Brush := TSolidBrush(Canvas.Brushes.Add(TSolidBrush));
      Brush.FillColor := clWhite32;
      Brush.FillMode := pfNonZero;
    end;

    while (y < Image.Bitmap.Height) do
    begin
      Image.Bitmap.Font.Size := Size;

      if (Canvas <> nil) then
        Canvas.RenderText(10, y, Format('%d: %s', [Size, EditText.Text]))
      else
        Image.Bitmap.RenderText(10, y, Format('%d: %s', [Size, EditText.Text]), clWhite32, CheckBoxAntiAlias.Checked);

      Size := Trunc(Size * 1.2);

      Height := Image.Bitmap.TextHeight(EditText.Text);
      y := y + MulDiv(Height, 8, 10);
    end;
  finally
    Canvas.Free;
  end;
end;

procedure TFormRenderText.EditTextChange(Sender: TObject);
begin
  Draw;
end;

procedure TFormRenderText.ImageResize(Sender: TObject);
begin
  Image.SetupBitmap;
  Draw;
end;

procedure TFormRenderText.BtnRunBenchmarkClick(Sender: TObject);
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
begin
  Screen.Cursor := crHourGlass;

  SaveQuality := Image.Bitmap.Font.Quality;
  SaveFont := Image.Bitmap.Font.Name;

  Image.Bitmap.Font.Style := [fsBold, fsItalic];
  Image.Bitmap.Font.Size := 20;

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

    if (Canvas <> nil) then
    begin
      for i := 1 to 10000 do
      begin
        Brush.FillColor := Color32(Random(255), Random(255), Random(255), Random(255));

        Canvas.RenderText(
          Random(Image.Bitmap.Width - 40),
          Random(Image.Bitmap.Height - 40),
          IntToStr(i));
      end;
    end else
    begin
      for i := 1 to 10000 do
        Image.Bitmap.RenderText(
          Random(Image.Bitmap.Width - 40),
          Random(Image.Bitmap.Height - 40),
          IntToStr(i),
          Color32(Random(255), Random(255), Random(255), Random(255)));
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

procedure TFormRenderText.CheckBoxAntiAliasClick(Sender: TObject);
begin
  Draw;
end;

procedure TFormRenderText.CheckBoxCanvas32Click(Sender: TObject);
begin
  CheckBoxAntiAlias.Enabled := not CheckBoxCanvas32.Checked;
  Update;
  Draw;
end;

procedure TFormRenderText.CheckBoxItalicClick(Sender: TObject);
begin
  Draw;
end;

procedure TFormRenderText.CheckBoxBoldClick(Sender: TObject);
begin
  Draw;
end;

procedure TFormRenderText.ComboBoxFontChange(Sender: TObject);
begin
  Image.Bitmap.Font.Name := ComboBoxFont.Text;
  Draw;
end;

end.
