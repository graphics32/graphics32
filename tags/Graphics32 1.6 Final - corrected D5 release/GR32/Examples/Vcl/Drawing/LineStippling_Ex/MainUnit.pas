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
  StdCtrls, GR32, GR32_Image;

type
  TForm1 = class(TForm)
    Image: TImage32;
    ScrollBar1: TScrollBar;
    procedure ScrollBarChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    procedure Spiral(X, Y: Integer);
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.ScrollBarChange(Sender: TObject);
var
  Step: Single;
begin
  Step := ScrollBar1.Position / 100;

  with Image.Bitmap do
  begin
    BeginUpdate;
    Clear(clBlack32);
    SetStipple([clWhite32, clWhite32, clWhite32, clWhite32, 0, 0, 0, 0]);
    StippleStep := Step;
    Spiral(50, 50);

    SetStipple([clWhite32, $00FFFFFF]);
    Spiral(150, 50);

    SetStipple([clWhite32, clRed32, clGreen32, 0, 0, 0]);
    Spiral(50, 150);

    SetStipple([clGreen32, clGreen32, clGreen32, 0, 0, clWhite32, 0, 0]);
    Spiral(150, 150);
    EndUpdate;
  end;
  Image.Repaint;
end;

procedure TForm1.Spiral(X, Y: Integer);
var
  Theta: Single;
begin
  Theta := 0;
  Image.Bitmap.MoveToF(X, Y);
  while Theta < 15 * 3.1415926535 do
  begin
    Image.Bitmap.LineToFSP(X + Cos(Theta) * Theta, Y + Sin(Theta) * Theta);
    Theta := Theta + 0.2;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Image.SetupBitmap;
  ScrollBarChange(Sender);
end;

end.
