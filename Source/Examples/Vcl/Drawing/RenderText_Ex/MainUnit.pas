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
    BtClickMe: TButton;
    EditText: TEdit;
    Image: TImage32;
    LbAALevel: TLabel;
    LbEnterText: TLabel;
    Panel1: TPanel;
    SBAntialias1: TSpeedButton;
    SBAntialias2: TSpeedButton;
    SBAntialias3: TSpeedButton;
    SBAntialias4: TSpeedButton;
    SBClearType: TSpeedButton;
    SBTextOut: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure BtClickMeClick(Sender: TObject);
    procedure EditTextChange(Sender: TObject);
    procedure ImageResize(Sender: TObject);
    procedure SBTextOutClick(Sender: TObject);
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

procedure TFormRenderText.FormCreate(Sender: TObject);
begin
  Image.SetupBitmap;
  with Image.Bitmap.Font do
  begin
    Name := 'Tahoma';
    Size := 20;
    Style := [fsBold, fsItalic];
  end;
  Panel1.DoubleBuffered := True;
  EditText.DoubleBuffered := True;
end;

procedure TFormRenderText.Draw;
begin
  with Image do begin
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

procedure TFormRenderText.BtClickMeClick(Sender: TObject);
var
  I: Integer;
  A,B,C : Int64;
  str   : string;
begin
  Screen.Cursor := crHourGlass;
  {$IFNDEF FPC}
  QueryPerformanceFrequency(C);
  QueryPerformanceCounter(A);
  {$ENDIF}
  with Image.Bitmap do
    for I := 0 to 100 do
      RenderText(
        Random(Width - 40),
        Random(Height - 40),
        IntToStr(Random(100)),
        AALevel,
        Color32(Random(255), Random(255), Random(255), Random(255)));
  {$IFNDEF FPC}
  QueryPerformanceCounter(B);
  with TBitmap32.Create do
  try
    Font.Color := clWhite;
    Font.Size := 8;
    Font.Style := [];
    SetSize(100,8);
    str := FloatToStrF(1000 * (B - A) / C, ffFixed, 4, 4) + ' ms';
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

procedure TFormRenderText.SBTextOutClick(Sender: TObject);
begin
  AALevel := TControl(Sender).Tag;
  Draw;
end;

end.
