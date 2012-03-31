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
 * The Original Code is Line Stippling Example
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, GR32,
  GR32_Image;

type
  TFormLineStippling = class(TForm)
    Image: TImage32;
    ScrollBar: TScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure ScrollBarChange(Sender: TObject);
  public
    procedure Spiral(X, Y: Integer);
  end;

var
  FormLineStippling: TFormLineStippling;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  GR32_Math;

{ TFormLineStippling }

procedure TFormLineStippling.FormCreate(Sender: TObject);
begin
  Image.SetupBitmap;
  ScrollBarChange(Sender);
end;

procedure TFormLineStippling.ScrollBarChange(Sender: TObject);
var
  Step: Single;
begin
  Step := ScrollBar.Position * 0.01;

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

procedure TFormLineStippling.Spiral(X, Y: Integer);
var
  Theta: TFloat;
  Sn, Cn: TFloat;
begin
  Theta := 0;
  Image.Bitmap.MoveToF(X, Y);
  while Theta < 15 * Pi do
  begin
    SinCos(Theta, Sn, Cn);
    Image.Bitmap.LineToFSP(X + Cn * Theta, Y + Sn * Theta);
    Theta := Theta + 0.2;
  end;
end;

end.
