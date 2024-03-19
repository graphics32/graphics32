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

{$I GR32.inc}

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
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FRenderer: TPolygonRenderer32VPR;
    FCurrentIndex: Integer;
    FVertices: TArrayOfFloatPoint;
    procedure RandomizeVertices;
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
  Math,
  Types,
  GR32_Math,
  GR32_LowLevel,
  GR32_VectorUtils;

{ TFormBezier }

procedure TFormBezier.FormCreate(Sender: TObject);
begin
  FRenderer := TPolygonRenderer32VPR.Create(PaintBox32.Buffer);

  SetLength(FVertices, 6);
  RandomizeVertices;

  FCurrentIndex := -1;
end;

procedure TFormBezier.FormDestroy(Sender: TObject);
begin
  FRenderer.Free;
end;

procedure TFormBezier.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    27: Close;
    13:
      begin
        RandomizeVertices;
        PaintBox32.Invalidate;
      end;
    187:
      begin
        SetLength(FVertices, Length(FVertices) + 1);
        with PaintBox32 do
          FVertices[Length(FVertices) - 1] := FloatPoint(Random * Width,
            Random * Height);
        PaintBox32.Invalidate;
      end;
    189:
      if Length(FVertices) > 5 then
      begin
        SetLength(FVertices, Length(FVertices) - 1);
        PaintBox32.Invalidate;
      end;
  end;

end;

function CubicInterpolation(const Fractional: TFloat;
  const Data0, Data1, Data2, Data3: TFloat): TFloat;
begin
  Result := Data1 + 0.5 * Fractional * (Data2 - Data0 + Fractional *
    (4 * Data2 + 2 * Data0 - 5 * Data1 - Data3 + Fractional *
    (3 * (Data1 - Data2) - Data0 + Data3)));
end;

procedure TFormBezier.RandomizeVertices;
var
  Index: Integer;
begin
  with PaintBox32 do
    for Index := 0 to High(FVertices) do
      FVertices[Index] := FloatPoint(Random * Width, Random * Height);
end;

procedure TFormBezier.PaintBox32DblClick(Sender: TObject);
begin
  RandomizeVertices;
  PaintBox32.Invalidate;
end;

procedure TFormBezier.PaintBox32MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
  Dist, MinDist: TFloat;
  MinDistIndex: Integer;
begin
  FCurrentIndex := -1;
  for Index := 0 to Length(FVertices) - 1 do
    if Sqr(FVertices[Index].X - X) + Sqr(FVertices[Index].Y - Y)  < 25 then
    begin
      if (Length(FVertices) > 5) and (Button = mbRight) then
      begin
        if Index < Length(FVertices) - 1 then
          Move(FVertices[Index + 1], FVertices[Index],
            (Length(FVertices) - Index - 1) * SizeOf(TFloatPoint));
        SetLength(FVertices, Length(FVertices) - 1);
        PaintBox32.Invalidate;
      end
      else
        FCurrentIndex := Index;
      Exit;
    end;

  if Button = mbLeft then
  begin
    MinDistIndex := 0;
    MinDist := Sqr(X - FVertices[0].X) + Sqr(Y - FVertices[0].Y);
    for Index := 1 to High(FVertices) do
    begin
      Dist := Sqr(X - FVertices[Index].X) + Sqr(Y - FVertices[Index].Y);
      if Dist < MinDist then
      begin
        MinDistIndex := Index;
        MinDist := Dist;
      end;
    end;

    SetLength(FVertices, Length(FVertices) + 1);
    Move(FVertices[MinDistIndex], FVertices[MinDistIndex + 1],
      (Length(FVertices) - MinDistIndex) * SizeOf(TFloatPoint));
    FCurrentIndex := MinDistIndex;
    FVertices[FCurrentIndex] := FloatPoint(X, Y);
    PaintBox32.Invalidate;
  end;
end;

procedure TFormBezier.PaintBox32MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if FCurrentIndex >= 0 then
  begin
    FVertices[FCurrentIndex] := FloatPoint(X, Y);
    PaintBox32.Invalidate;
  end;
end;

procedure TFormBezier.PaintBox32MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FCurrentIndex >= 0 then
  with PaintBox32 do
    begin
      FVertices[FCurrentIndex].X := EnsureRange(FVertices[FCurrentIndex].X,
        0, Width);
      FVertices[FCurrentIndex].Y := EnsureRange(FVertices[FCurrentIndex].Y,
        0, Height);
    end;

  FCurrentIndex := -1;
  PaintBox32.Invalidate;
end;

procedure TFormBezier.PaintBox32PaintBuffer(Sender: TObject);
var
  Index: Integer;
  Val: Double;
  Fractional: Double;
  Indices: array [0..3] of Integer;
  PolyCount: Integer;
  Outline: TArrayOfArrayOfFloatPoint;
const
  CVertexCountStep = 64;
begin
  PaintBox32.Buffer.Clear($FFFFFFFF);

  Outline := BuildPolyPolyLine(PolyPolygon(FVertices), True, 2);

  PolyCount := Length(Outline);
  SetLength(Outline, PolyCount + Length(FVertices));
  for Index := 0 to Length(FVertices) - 1 do
    Outline[PolyCount + Index] := Circle(FVertices[Index].X, FVertices[Index].Y, 5, 32);

  FRenderer.Color := $80000080;
  FRenderer.PolyPolygonFS(Outline);

  SetLength(Outline, 1, CVertexCountStep);
  Outline[0, 0] := FVertices[0];
  Index := 0;
  Val := 0;
  while Val < Length(FVertices) do
  begin
    Indices[0] := (Length(FVertices) + Trunc(Val) - 2 + 1) mod Length(FVertices);
    Indices[1] := (Indices[0] + 1) mod Length(FVertices);
    Indices[2] := (Indices[1] + 1) mod Length(FVertices);
    Indices[3] := (Indices[2] + 1) mod Length(FVertices);

    Fractional := Frac(Val);

    Inc(Index);
    if Index = Length(Outline[0]) then
      SetLength(Outline[0], Length(Outline[0]) + CVertexCountStep);

    Outline[0, Index] := FloatPoint(
      CubicInterpolation(Fractional, FVertices[Indices[0]].X,
        FVertices[Indices[1]].X, FVertices[Indices[2]].X,
        FVertices[Indices[3]].X),
      CubicInterpolation(Fractional, FVertices[Indices[0]].Y,
        FVertices[Indices[1]].Y, FVertices[Indices[2]].Y,
        FVertices[Indices[3]].Y));
    Val := Val + 0.03;
  end;
  SetLength(Outline[0], Index + 1);
  FRenderer.Color := $FF000000;
  FRenderer.PolyPolygonFS(BuildPolyPolyline(Outline, True, 2));
end;

end.
