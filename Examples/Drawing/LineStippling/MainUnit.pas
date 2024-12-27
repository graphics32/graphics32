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

{$include GR32.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  GR32,
  GR32_Image;

type
  TFormLineStippling = class(TForm)
    PaintBox: TPaintBox32;
    ScrollBar: TScrollBar;
    TimerAnimate: TTimer;
    procedure ScrollBarChange(Sender: TObject);
    procedure PaintBoxPaintBuffer(Sender: TObject);
    procedure PaintBoxClick(Sender: TObject);
    procedure TimerAnimateTimer(Sender: TObject);
  private
    FStippleCounter: Single;
  public
    procedure Spiral(X, Y: Integer);
  end;

var
  FormLineStippling: TFormLineStippling;

implementation

{$R *.dfm}

uses
  Math,
  GR32_Gamma,
  GR32_Math;

{ TFormLineStippling }

procedure TFormLineStippling.PaintBoxClick(Sender: TObject);
begin
  TimerAnimate.Enabled := not TimerAnimate.Enabled;
end;

procedure TFormLineStippling.PaintBoxPaintBuffer(Sender: TObject);
var
  Step: Single;
  Stipple: TArrayOfColor32;
begin
  Step := ScrollBar.Position * 0.01;

  PaintBox.Buffer.BeginUpdate;
  try

    PaintBox.Buffer.Clear(clBlack32);
    PaintBox.Buffer.StippleStep := Step;

    // Note that we are not using PaintBox.Buffer.Width & Height since
    // PaintBox.BufferOversize might cause the buffer to be bigger than
    // the control.

    // Dynamic array of colors
    Stipple := [clWhite32, clWhite32, clWhite32, clWhite32, 0, 0, 0, 0];
    PaintBox.Buffer.SetStipple(Stipple);
    PaintBox.Buffer.StippleCounter := FStippleCounter;
    Spiral(PaintBox.Width div 4, PaintBox.Height div 4);

    // Static array of colors
    PaintBox.Buffer.SetStipple([clWhite32, $00FFFFFF]);
    PaintBox.Buffer.StippleCounter := FStippleCounter;
    Spiral(3*PaintBox.Width div 4, PaintBox.Height div 4);

    PaintBox.Buffer.SetStipple([clWhite32, clRed32, clGreen32, 0, 0, 0]);
    PaintBox.Buffer.StippleCounter := FStippleCounter;
    Spiral(PaintBox.Width div 4, 3*PaintBox.Height div 4);

    PaintBox.Buffer.SetStipple([clGreen32, clGreen32, clGreen32, 0, 0, clWhite32, 0, 0]);
    PaintBox.Buffer.StippleCounter := FStippleCounter;
    Spiral(3*PaintBox.Width div 4, 3*PaintBox.Height div 4);

  finally
    PaintBox.Buffer.EndUpdate;
  end;
end;

procedure TFormLineStippling.ScrollBarChange(Sender: TObject);
begin
  PaintBox.Invalidate;
end;

procedure TFormLineStippling.Spiral(X, Y: Integer);
var
  Theta: Single;
  Sn, Cn: Single;
  Step: Single;
begin
  PaintBox.Buffer.MoveToF(X, Y);

  Theta := 0;
  Step := 40 / Max(PaintBox.Width, PaintBox.Height);

  while Theta < 15 * Pi do
  begin
    GR32_Math.SinCos(Theta, Sn, Cn);
    PaintBox.Buffer.LineToFSP(X + Cn * Theta * (PaintBox.Width / 220), Y + Sn * Theta * (PaintBox.Height / 220));
    Theta := Theta + Step;
  end;
end;

procedure TFormLineStippling.TimerAnimateTimer(Sender: TObject);
begin
  FStippleCounter := FStippleCounter + 0.1;
  PaintBox.Invalidate;
end;

end.
