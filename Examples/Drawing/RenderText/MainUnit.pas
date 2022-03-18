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

{$I GR32.inc}

uses
  {$IFDEF FPC} LCLType, LResources, {$ELSE} Windows, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  GR32, ComCtrls, GR32_Image, Buttons;

type
  TFormRenderText = class(TForm)
    BtnAntialias1: TSpeedButton;
    BtnAntialias2: TSpeedButton;
    BtnAntialias3: TSpeedButton;
    BtnAntialias4: TSpeedButton;
    BtnClearType: TSpeedButton;
    BtnClickMe: TButton;
    BtnTextOut: TSpeedButton;
    EditText: TEdit;
    Image: TImage32;
    LblAALevel: TLabel;
    LblEnterText: TLabel;
    PnlControl: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure BtnClickMeClick(Sender: TObject);
    procedure EditTextChange(Sender: TObject);
    procedure ImageResize(Sender: TObject);
    procedure BtnTextOutClick(Sender: TObject);
  public
    AALevel: Integer;
    procedure Draw;
  end;

var
  FormRenderText: TFormRenderText;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

{$IFNDEF FPC}
uses
  Diagnostics;
{$ENDIF}

procedure TFormRenderText.FormCreate(Sender: TObject);
begin
  Image.SetupBitmap;
  with Image.Bitmap.Font do
  begin
    Name := 'Tahoma';
    Size := 20;
    Style := [fsBold, fsItalic];
  end;
  PnlControl.DoubleBuffered := True;
  EditText.DoubleBuffered := True;
end;

procedure TFormRenderText.Draw;
begin
  with Image do
  begin
    Bitmap.Clear;
    Bitmap.RenderText(10, 10, EditText.Text, AALevel, $FFFFFFFF);
    Invalidate;
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

procedure TFormRenderText.BtnClickMeClick(Sender: TObject);
var
  I: Integer;
  Str: string;
begin
  Screen.Cursor := crHourGlass;
{$IFNDEF FPC}
  var StopWatch := TStopWatch.StartNew;
{$ENDIF}
  with Image.Bitmap do
    for I := 0 to 10000 do
      RenderText(
        Random(Width - 40),
        Random(Height - 40),
        IntToStr(Random(100)),
        AALevel,
        Color32(Random(255), Random(255), Random(255), Random(255)));
{$IFNDEF FPC}
  StopWatch.Stop;
  with TBitmap32.Create do
  try
    Font.Color := clWhite;
    Font.Size := 8;
    Font.Style := [];
    SetSize(100,8);
    str := '  '+StopWatch.ElapsedMilliseconds.ToString + ' ms';
    SetSize(TextWidth(str),TextHeight(str));
    Textout(0, 0, str);
    DrawTo(Image.Bitmap, Image.Bitmap.Width - Width, Image.Bitmap.Height-Height);
  finally
    Free;
  end;
  {$ENDIF}
  Screen.Cursor := crDefault;
  Image.Invalidate;
end;

procedure TFormRenderText.BtnTextOutClick(Sender: TObject);
begin
  AALevel := TControl(Sender).Tag;
  Draw;
end;

end.
