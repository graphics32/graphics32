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
 * The Original Code is Vectorial Polygon Rasterizer for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Christian-W. Budde
 *
 * Portions created by the Initial Developer are Copyright (C) 2012
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

uses
  {$IFDEF FPC}LCLIntf, {$ELSE}Windows, {$ENDIF} SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, GR32, GR32_Image, GR32_Polygons, GR32_Paths;

type
  TFormBezier = class(TForm)
    PaintBox32: TPaintBox32;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox32PaintBuffer(Sender: TObject);
    procedure PaintBox32DblClick(Sender: TObject);
    procedure PaintBox32MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox32MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox32MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    FRenderer: TPolygonRenderer32VPR;
    FCurrentIndex: Integer;
    FPoints: TArrayOfFloatPoint;
  end;

var
  FormBezier: TFormBezier;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, GR32_VectorUtils;

{ TFormBezier }

procedure TFormBezier.FormCreate(Sender: TObject);
begin
  FRenderer := TPolygonRenderer32VPR.Create;
  FRenderer.Bitmap := PaintBox32.Buffer;
  FRenderer.FillMode := pfWinding;

  SetLength(FPoints, 6);
  FPoints[0] := FloatPoint(Random * PaintBox32.Width, Random * PaintBox32.Height);
  FPoints[1] := FloatPoint(Random * PaintBox32.Width, Random * PaintBox32.Height);
  FPoints[2] := FloatPoint(Random * PaintBox32.Width, Random * PaintBox32.Height);
  FPoints[3] := FloatPoint(Random * PaintBox32.Width, Random * PaintBox32.Height);
  FPoints[4] := FloatPoint(Random * PaintBox32.Width, Random * PaintBox32.Height);
  FPoints[5] := FloatPoint(Random * PaintBox32.Width, Random * PaintBox32.Height);

  FCurrentIndex := -1;
end;

procedure TFormBezier.FormDestroy(Sender: TObject);
begin
  FRenderer.Free;
end;

function CubicInterpolation(const Fractional: TFloat;
  const Data0, Data1, Data2, Data3: TFloat): TFloat;
begin
  Result := Data1 + 0.5 * Fractional * (Data2 - Data0 + Fractional *
    (4 * Data2 + 2 * Data0 - 5 * Data1 - Data3 + Fractional *
    (3 * (Data1 - Data2) - Data0 + Data3)));
end;

procedure TFormBezier.PaintBox32DblClick(Sender: TObject);
begin
  FPoints[0] := FloatPoint(Random * PaintBox32.Width, Random * PaintBox32.Height);
  FPoints[1] := FloatPoint(Random * PaintBox32.Width, Random * PaintBox32.Height);
  FPoints[2] := FloatPoint(Random * PaintBox32.Width, Random * PaintBox32.Height);
  FPoints[3] := FloatPoint(Random * PaintBox32.Width, Random * PaintBox32.Height);
  FPoints[4] := FloatPoint(Random * PaintBox32.Width, Random * PaintBox32.Height);
  FPoints[5] := FloatPoint(Random * PaintBox32.Width, Random * PaintBox32.Height);
  PaintBox32.Invalidate;
end;

procedure TFormBezier.PaintBox32MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
begin
  FCurrentIndex := -1;
  for Index := 0 to Length(FPoints) - 1 do
  begin
    if Sqr(FPoints[Index].X - X) + Sqr(FPoints[Index].Y - Y)  < 25 then
    begin
      FCurrentIndex := Index;
      Exit;
    end;
  end;
end;

procedure TFormBezier.PaintBox32MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if FCurrentIndex >= 0 then
  begin
    FPoints[FCurrentIndex] := FloatPoint(X, Y);
    PaintBox32.Invalidate;
  end;
end;

procedure TFormBezier.PaintBox32MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FCurrentIndex := -1;
end;

procedure TFormBezier.PaintBox32PaintBuffer(Sender: TObject);
var
  Index: Integer;
  Val: Double;
  Fractional: Double;
  Indices: array [0..3] of Integer;
  PolyCount: Integer;
  Outline: TArrayOfArrayOfFloatPoint;
begin
  PaintBox32.Buffer.Clear($FFFFFFFF);

  Outline := BuildPolyPolyLine(PolyPolygon(FPoints), True, 2);

  PolyCount := Length(Outline);
  SetLength(Outline, PolyCount + Length(FPoints));
  for Index := 0 to Length(FPoints) - 1 do
    Outline[PolyCount + Index] := Circle(FPoints[Index].X, FPoints[Index].Y, 5, 32);

  FRenderer.Color := $80000080;
  FRenderer.PolyPolygonFS(Outline);

  FRenderer.Color := $FF000000;
  with TFlattenedPath.Create do
  try
    BeginPath;
    MoveTo(FPoints[0]);
    Val := 0;
    while Val < Length(FPoints) do
    begin
      Indices[0] := (Length(FPoints) + Trunc(Val) - 2 + 1) mod Length(FPoints);
      Indices[1] := (Indices[0] + 1) mod Length(FPoints);
      Indices[2] := (Indices[1] + 1) mod Length(FPoints);
      Indices[3] := (Indices[2] + 1) mod Length(FPoints);

      Fractional := Frac(Val);

      LineTo(
        CubicInterpolation(Fractional, FPoints[Indices[0]].X,
          FPoints[Indices[1]].X, FPoints[Indices[2]].X, FPoints[Indices[3]].X),
        CubicInterpolation(Fractional, FPoints[Indices[0]].Y,
          FPoints[Indices[1]].Y, FPoints[Indices[2]].Y, FPoints[Indices[3]].Y));
      Val := Val + 0.03;
    end;
    ClosePath;
    EndPath;
    FRenderer.PolyPolygonFS(BuildPolyPolyline(Path, False, 2));
  finally
    Free;
  end;
end;

end.
