unit MainUnit;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
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
 * The Original Code is RenderText Example
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Christian-W. Budde
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.INC}

uses
  {$IFDEF FPC} LCLType, LResources, {$ELSE} Windows, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  GR32, ComCtrls, GR32_Image, Buttons;

type
  TForm1 = class(TForm)
    Image: TImage32;
    Panel1: TPanel;
    EditText: TEdit;
    LbAALevel: TLabel;
    BtClickMe: TButton;
    LbEnterText: TLabel;
    SBTextOut: TSpeedButton;
    SBAntialias1: TSpeedButton;
    SBAntialias2: TSpeedButton;
    SBAntialias3: TSpeedButton;
    SBAntialias4: TSpeedButton;
    SBClearType: TSpeedButton;
    procedure EditTextChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ImageResize(Sender: TObject);
    procedure BtClickMeClick(Sender: TObject);
    procedure SBTextOutClick(Sender: TObject);
  public
    AALevel: Integer;
    procedure Draw;
  end;

var
  Form1: TForm1;

implementation

{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}


procedure TForm1.Draw;
begin
  Image.Bitmap.Clear;
  Image.Bitmap.RenderText(10, 10, EditText.Text, AALevel, $FFFFFFFF);
end;

procedure TForm1.EditTextChange(Sender: TObject);
begin
  Draw;
end;

procedure TForm1.FormCreate(Sender: TObject);
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

procedure TForm1.ImageResize(Sender: TObject);
begin
  Image.SetupBitmap;
  Draw;
end;

procedure TForm1.BtClickMeClick(Sender: TObject);
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
  begin
    Font.Color := clWhite;
    Font.Size := 8;
    Font.Style := [];
    SetSize(100,8);
    str := FloatToStrF(1000*(B-A)/C,ffFixed, 4, 4) + ' ms';
    SetSize(TextWidth(str),TextHeight(str));
    Textout(0, 0, str);
    DrawTo(Image.Bitmap, Image.Bitmap.Width-Width, Image.Bitmap.Height-Height);
  end;
  {$ENDIF}
  Screen.Cursor := crDefault;
end;

procedure TForm1.SBTextOutClick(Sender: TObject);
begin
  AALevel := TControl(Sender).Tag;
  Draw;
end;

{$IFDEF FPC}
initialization
  {$I MainUnit.lrs}
{$ENDIF}

end.
