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
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2004
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, GR32, ComCtrls, GR32_Image, Buttons;

type
  TForm1 = class(TForm)
    Image: TImage32;
    Panel1: TPanel;
    Edit1: TEdit;
    Label1: TLabel;
    Button1: TButton;
    Label2: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    procedure Edit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ImageResize(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  public
    AALevel: Integer;
    procedure Draw;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Draw;
begin
  Image.Bitmap.Clear;
  Image.Bitmap.RenderText(10, 10, Edit1.Text, AALevel, $FFFFFFFF);
end;

procedure TForm1.Edit1Change(Sender: TObject);
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
  Edit1.DoubleBuffered := True;
end;

procedure TForm1.ImageResize(Sender: TObject);
begin
  Image.SetupBitmap;
  Draw;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  I: Integer;
begin
  Screen.Cursor := crHourGlass;
  with Image.Bitmap do
    for I := 0 to 100 do
      RenderText(
        Random(Width - 40),
        Random(Height - 40),
        IntToStr(Random(100)),
        AALevel,
        Color32(Random(255), Random(255), Random(255), Random(255)));
  Screen.Cursor := crDefault;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  AALevel := TControl(Sender).Tag;
  Draw;
end;

end.
