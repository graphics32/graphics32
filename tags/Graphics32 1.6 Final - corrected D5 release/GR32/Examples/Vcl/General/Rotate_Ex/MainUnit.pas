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
  GR32, GR32_Image, GR32_Transforms, ComCtrls, Math, GR32_RangeBars;

type
  TForm1 = class(TForm)
    Src: TImage32;
    Dst: TImage32;
    Angle: TGaugeBar;
    procedure FormCreate(Sender: TObject);
    procedure AngleChange(Sender: TObject);
  public
    procedure ScaleRot(Alpha: Single);
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Dst.Bitmap.SetSize(Src.Bitmap.Width, Src.Bitmap.Height);

  // erase the transparency of edge pixels
  SetBorderTransparent(Src.Bitmap, Rect(0, 0, Src.Bitmap.Width - 1, Src.Bitmap.Height - 1));

  // show the picture
  ScaleRot(0);
end;

procedure TForm1.ScaleRot(Alpha: Single);
var
  SrcR: Integer;
  SrcB: Integer;
  T: TAffineTransformation;
  Sx, Sy, Scale: Single;
begin
  SrcR := Src.Bitmap.Width - 1;
  SrcB := Src.Bitmap.Height - 1;
  T := TAffineTransformation.Create;
  T.SrcRect := FloatRect(0, 0, SrcR + 1, SrcB + 1);
  try
    // shift the origin
    T.Clear;

    // move the origin to a center for scaling and rotation
    T.Translate(-SrcR / 2, -SrcB / 2);
    T.Rotate(0, 0, Alpha);
    Alpha := Alpha * 3.14159265358979 / 180;

    // get the width and height of rotated image (without scaling)
    Sx := Abs(SrcR * Cos(Alpha)) + Abs(SrcB * Sin(Alpha));
    Sy := Abs(SrcR * Sin(Alpha)) + Abs(SrcB * Cos(Alpha));

    // calculate a new scale so that the image fits in original boundaries
    Sx := Src.Bitmap.Width / Sx;
    Sy := Src.Bitmap.Height / Sy;
    scale := Min(Sx, Sy);

    T.Scale(Scale, Scale);

    // move the origin back
    T.Translate(SrcR / 2, SrcB / 2);

    // transform the bitmap
    Dst.BeginUpdate;
    Dst.Bitmap.Clear(clGray32);
    Transform(Dst.Bitmap, Src.Bitmap, T);
    Dst.EndUpdate;
    Dst.Repaint;
  finally
    T.Free;
  end;
end;

procedure TForm1.AngleChange(Sender: TObject);
begin
  ScaleRot(-Angle.Position);
end;

end.
