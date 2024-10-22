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
    BtnClickMe: TButton;
    EditText: TEdit;
    Image: TImage32;
    LblEnterText: TLabel;
    PnlControl: TPanel;
    CheckBoxAntiAlias: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure BtnClickMeClick(Sender: TObject);
    procedure EditTextChange(Sender: TObject);
    procedure ImageResize(Sender: TObject);
    procedure CheckBoxAntiAliasClick(Sender: TObject);
  public
    procedure Draw;
  end;

var
  FormRenderText: TFormRenderText;

implementation

{$R *.dfm}

uses
  GR32_System;

procedure TFormRenderText.FormCreate(Sender: TObject);
begin
  Image.SetupBitmap;

  Image.Bitmap.Font.Name := 'Tahoma';
  Image.Bitmap.Font.Size := 20;
  Image.Bitmap.Font.Style := [fsBold, fsItalic];
end;

procedure TFormRenderText.Draw;
var
  SaveQuality: TFontQuality;
begin
  SaveQuality := Image.Bitmap.Font.Quality;

  if (CheckBoxAntiAlias.Checked) then
    Image.Bitmap.Font.Quality := TFontQuality.fqAntialiased
  else
    Image.Bitmap.Font.Quality := TFontQuality.fqNonAntialiased;

  Image.Bitmap.Clear;
  Image.Bitmap.RenderText(10, 10, EditText.Text, clWhite32);

  Image.Bitmap.Font.Quality := SaveQuality;
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

procedure TFormRenderText.BtnClickMeClick(Sender: TObject);
var
  SaveQuality: TFontQuality;
  i: Integer;
  Str: string;
  StopWatch: TStopWatch;
  Bitmap: TBitmap32;
begin
  Screen.Cursor := crHourGlass;

  SaveQuality := Image.Bitmap.Font.Quality;

  if (CheckBoxAntiAlias.Checked) then
    Image.Bitmap.Font.Quality := TFontQuality.fqAntialiased
  else
    Image.Bitmap.Font.Quality := TFontQuality.fqNonAntialiased;

  StopWatch := TStopWatch.StartNew;

  Image.Bitmap.BeginUpdate;

  for i := 1 to 10000 do
    Image.Bitmap.RenderText(
      Random(Image.Bitmap.Width - 40),
      Random(Image.Bitmap.Height - 40),
      IntToStr(Random(100)),
      Color32(Random(255), Random(255), Random(255), Random(255)));

  Image.Bitmap.EndUpdate;

  StopWatch.Stop;

  Image.Bitmap.Font.Quality := SaveQuality;

  Bitmap := TBitmap32.Create;
  try
    str := '  ' + StopWatch.ElapsedMilliseconds.ToString + ' ms';

    // Create a bitmap with the timing text
    Bitmap.Font.Color := clWhite;
    Bitmap.Font.Size := 8;
    Bitmap.Font.Style := [];
    // Size bitmap to text and draw the text
    Bitmap.SetSize(Bitmap.TextWidth(str), Bitmap.TextHeight(str));
    Bitmap.Textout(0, 0, str);

    // Draw timing at lower, right corner
    Bitmap.DrawTo(Image.Bitmap, Image.Bitmap.Width - Bitmap.Width, Image.Bitmap.Height-Bitmap.Height);
  finally
    Bitmap.Free;
  end;

  Screen.Cursor := crDefault;
  Image.Invalidate;
end;

procedure TFormRenderText.CheckBoxAntiAliasClick(Sender: TObject);
begin
  Draw;
end;

end.
