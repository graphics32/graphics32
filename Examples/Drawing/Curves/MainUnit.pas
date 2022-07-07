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
 * The Original Code is Curves Example (based on VPR example)
 *
 * The Initial Developer of the Original Code is
 * Mattias Andersson <mattias@centaurix.com>
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Christian-W. Budde (GR32 version 2.0 port)
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  {$IFDEF FPC} LCLIntf, LResources, Buttons, {$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  ExtCtrls, GR32_Image;

type
  TMainForm = class(TForm)
    BtnDrawCurve: TButton;
    CbxUpdate: TCheckBox;
    Img: TImage32;
    procedure BtnDrawCurveClick(Sender: TObject);
    procedure CbxUpdateClick(Sender: TObject);
    procedure ApplicationIdleHandler(Sender: TObject; var Done: Boolean);
  private
  end;

var
  MainForm: TMainForm;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, GR32, GR32_Geometry, GR32_VectorUtils, GR32_Resamplers, GR32_LowLevel,
  GR32_Polygons;

function MakeCurve(const Points: TArrayOfFloatPoint; Kernel: TCustomKernel;
  Closed: Boolean): TArrayOfFloatPoint;
const
  TOLERANCE: TFloat = 20.0;
  THRESHOLD: TFloat = 0.5;
var
  I, H, R: Integer;
  Filter: TFilterMethod;
  WrapProc: TWrapProc;

  procedure AddPoint(const P: TFloatPoint);
  var
    L: Integer;
  begin
    L := Length(Result);
    SetLength(Result, L + 1);
    Result[L] := P;
  end;

  function GetPoint(I: Integer; t: TFloat = 0.0): TFloatPoint;
  var
    f, Index: Integer;
    W: TFloat;
  begin
    Result.X := 0; Result.Y := 0;
    for f := -R to R do
    begin
      Index := WrapProc(I - f, H);
      W := Filter(f + t);
      Result.X := Result.X + W * Points[Index].X;
      Result.Y := Result.Y + W * Points[Index].Y;
    end;
  end;

  procedure Recurse(I: Integer; const P1, P2: TFloatPoint; const t1, t2: TFloat);
  var
    Temp: TFloat;
    P: TFloatPoint;
  begin
    AddPoint(P1);
    Temp := (t1 + t2) * 0.5;
    P := GetPoint(I, Temp);

    if (Abs(CrossProduct(FloatPoint(P1.X - P.X, P1.Y - P.Y),
      FloatPoint(P.X - P2.X, P.Y - P2.Y))) > TOLERANCE) or (t2 - t1 >= THRESHOLD) then
    begin
      Recurse(I, P1, P, t1, Temp);
      Recurse(I, P, P2, Temp, t2);
    end
    else AddPoint(P);
  end;

const
  WRAP_PROC: array[Boolean] of TWrapProc = (Clamp, Wrap);
begin
  SetLength(Result, 0);

  WrapProc := Wrap_PROC[Closed];
  Filter := Kernel.Filter;
  R := Ceil(Kernel.GetWidth);
  H := High(Points);

  for I := 0 to H - 1 do
    Recurse(I, GetPoint(I), GetPoint(I + 1), 0, 1);

  if Closed then
    Recurse(H, GetPoint(H), GetPoint(0), 0, 1)
  else
    AddPoint(GetPoint(H));
end;

procedure TMainForm.BtnDrawCurveClick(Sender: TObject);
var
  PX, PY: TArrayOfFloatPoint;
  I: Integer;
  K: TCustomKernel;
  X, Y: Integer;
begin
  //Randomize;
  Img.SetupBitmap(True, $FF333333);
  SetLength(PX, 8);

  // create a set of random data points
  for I := 0 to High(PX) do
    PX[I] := FloatPoint(Random(Img.Width), Random(Img.Height));

  // create interpolation kernel
  K := TGaussianKernel.Create;
  try
    // subdivide recursively and interpolate
    PY := MakeCurve(PX, K, True);
  finally
    K.Free;
  end;

  // draw result polygon
  PolygonFS(Img.Bitmap, PY, $FFCC3300, pfWinding);

  // draw data points
  for I := 0 to High(PY) do
  begin
    X := Floor(PY[I].X);
    Y := Floor(PY[I].Y);
    Img.Bitmap.FillRects(X, Y, X + 1, Y + 1, $FF00FF00);
  end;
  for I := 0 to High(PX) do
  begin
    PY := Circle(PX[I].X, PX[I].Y, 4);
    PolygonFS(Img.Bitmap, PY, $FF000000);
    PY := Ellipse(PX[I].X, PX[I].Y, 2.75, 2.75);
    PolygonFS(Img.Bitmap, PY, $FF00FF00);
  end;
end;

procedure TMainForm.ApplicationIdleHandler(Sender: TObject; var Done: Boolean);
begin
  BtnDrawCurveClick(Sender);
end;

procedure TMainForm.CbxUpdateClick(Sender: TObject);
begin
  if CbxUpdate.Checked then
    Application.OnIdle := ApplicationIdleHandler
  else
    Application.OnIdle := nil;
end;

end.
