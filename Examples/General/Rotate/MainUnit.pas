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
 * The Original Code is Rotate Example
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
  {$IFNDEF FPC} Windows, {$ELSE} LCLIntf, LCLType, LResources, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls, Math,
  GR32, GR32_Image, GR32_Transforms, GR32_RangeBars;

type
  TFormRotateExample = class(TForm)
    Angle: TGaugeBar;
    Src: TImage32;
    Dst: TImage32;
    procedure FormCreate(Sender: TObject);
    procedure AngleChange(Sender: TObject);
  public
    procedure ScaleRot(Alpha: Single);
  end;

var
  FormRotateExample: TFormRotateExample;

implementation

{$R *.dfm}

uses
  Types,
  GR32_Math,
  GR32.ImageFormats.JPG;


{ TFormRotateExample }

procedure TFormRotateExample.FormCreate(Sender: TObject);
begin
  // load example image
  Src.Bitmap.LoadFromResourceName(HInstance, 'Delphi', RT_RCDATA);

  Dst.Bitmap.SetSize(Src.Bitmap.Width, Src.Bitmap.Height);

  // a workaround to the edge antialiasing problem
  SetBorderTransparent(Src.Bitmap, Src.Bitmap.BoundsRect);

  // show the picture
  ScaleRot(0);
end;

procedure TFormRotateExample.ScaleRot(Alpha: Single);
var
  SrcR: Integer;
  SrcB: Integer;
  T: TAffineTransformation;
  Sn, Cn: TFloat;
  Sx, Sy, Scale: Single;
begin
  SrcR := Src.Bitmap.Width - 1;
  SrcB := Src.Bitmap.Height - 1;

  T := TAffineTransformation.Create;
  try

    T.SrcRect := FloatRect(0, 0, SrcR + 1, SrcB + 1);

    // shift the origin
    T.Clear;

    // move the origin to a center for scaling and rotation
    T.Translate(-SrcR * 0.5, -SrcB * 0.5);
    T.Rotate(0, 0, Alpha);
    Alpha := Alpha * PI / 180;

    // get the width and height of rotated image (without scaling)
    GR32_Math.SinCos(Alpha, Sn, Cn);
    Sx := Abs(SrcR * Cn) + Abs(SrcB * Sn);
    Sy := Abs(SrcR * Sn) + Abs(SrcB * Cn);

    // calculate a new scale so that the image fits in original *bitmap* boundaries
    Sx := Src.Bitmap.Width / Sx;
    Sy := Src.Bitmap.Height / Sy;
    Scale := Min(Sx, Sy);

    T.Scale(Scale);

    // move the origin back
    T.Translate(SrcR * 0.5, SrcB * 0.5);

    // transform the bitmap
    Dst.BeginUpdate;
    try

      Dst.Bitmap.Clear(clBlack32);
      Transform(Dst.Bitmap, Src.Bitmap, T);

    finally
      Dst.EndUpdate;
    end;

  finally
    T.Free;
  end;
end;

procedure TFormRotateExample.AngleChange(Sender: TObject);
begin
  ScaleRot(-Angle.Position);
end;

end.
