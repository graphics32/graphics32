unit GR32_Geometry;

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
 * The Original Code is Additional Math Routines for Graphics32
 *
 * The Initial Developers of the Original Code are
 * Mattias Andersson <mattias@centaurix.com>
 * Michael Hansen <dyster_tid@hotmail.com>
 *
 * Portions created by the Initial Developers are Copyright (C) 2005-2007
 * the Initial Developers. All Rights Reserved.
 *
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  Math, GR32, GR32_Math;

type
  PVector2X = ^TVector2X;
  //TFixedVector = TFixedPoint;
  TVector2X = array [0..1] of TFixed;

  PVector2F = ^TVector2F;
  //TVector2F = TFloatPoint;
  TVector2F = array [0..1] of TFloat;

function VectorAdd(const V1, V2: TVector2F): TVector2F;  overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function VectorAdd(const V: TVector2F; Value: TFloat): TVector2F; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function VectorSub(const V1, V2: TVector2F): TVector2F; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function VectorSub(const V: TVector2F; Value: TFloat): TVector2F; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function VectorMul(const V1, V2: TVector2F): TVector2F; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function VectorScale(const V: TVector2F; Scale: TFloat): TVector2F; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function VectorDiv(const V: TVector2F; Divisor: TFloat): TVector2F; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function VectorDiv(const V1, V2: TVector2F): TVector2F; overload;{$IFDEF USEINLINING} inline; {$ENDIF}

function VectorCombine(const V1, V2: TVector2F; W: TFloat): TVector2F; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function VectorAbs(const V: TVector2F): TVector2F; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function VectorNeg(const V: TVector2F): TVector2F; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function VectorAverage(const V1, V2: TVector2F): TVector2F; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function VectorMax(const V1, V2: TVector2F): TVector2F; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function VectorMin(const V1, V2: TVector2F): TVector2F; overload;{$IFDEF USEINLINING} inline; {$ENDIF}

function VectorDot(const V1, V2: TVector2F): TFloat; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function VectorDistance(const V1, V2: TVector2F): TFloat; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function VectorNorm(const V: TVector2F): TFloat; overload;{$IFDEF USEINLINING} inline; {$ENDIF}

function VectorNormalize(const V: TVector2F): TFloat;


// Returns true if the line segments AB and CD intersect.
function SegmentsIntersect(const A, B, C, D: TVector2F): Boolean;

implementation

function VectorAdd(const V1, V2: TVector2F): TVector2F;
begin
  Result[0] := V1[0] + V2[0];
  Result[1] := V1[1] + V2[1];
end;

function VectorAdd(const V: TVector2F; Value: TFloat): TVector2F;
begin
  Result[0] := V[0] + Value;
  Result[1] := V[1] + Value;
end;

function VectorSub(const V1, V2: TVector2F): TVector2F;
begin
  Result[0] := V1[0] - V2[0];
  Result[1] := V1[1] - V2[1];
end;

function VectorSub(const V: TVector2F; Value: TFloat): TVector2F;
begin
  Result[0] := V[0] - Value;
  Result[1] := V[1] - Value;
end;

function VectorMul(const V1, V2: TVector2F): TVector2F;
begin
  Result[0] := V1[0] * V2[0];
  Result[1] := V1[1] * V2[1];
end;

function VectorScale(const V: TVector2F; Scale: TFloat): TVector2F;
begin
  Result[0] := V[0] * Scale;
  Result[1] := V[1] * Scale;
end;

function VectorDiv(const V: TVector2F; Divisor: TFloat): TVector2F;
begin
  Divisor := 1 / Divisor;
  Result[0] := V[0] * Divisor;
  Result[1] := V[1] * Divisor;
end;

function VectorDiv(const V1, V2: TVector2F): TVector2F;
begin
  Result[0] := V1[0] / V2[0];
  Result[1] := V1[1] / V2[1];
end;

function VectorCombine(const V1, V2: TVector2F; W: TFloat): TVector2F;
begin
  Result[0] := V2[0] + (V1[0] - V2[0]) * W;
  Result[1] := V2[1] + (V1[1] - V2[1]) * W;
end;

function VectorAbs(const V: TVector2F): TVector2F;
begin
  Result[0] := System.Abs(V[0]);
  Result[1] := System.Abs(V[1]);
end;

function VectorNeg(const V: TVector2F): TVector2F;
begin
  Result[0] := -V[0];
  Result[1] := -V[1];
end;

function VectorAverage(const V1, V2: TVector2F): TVector2F;
begin
  Result[0] := (V1[0] + V2[0]) * 0.5;
  Result[1] := (V1[1] + V2[1]) * 0.5;
end;

function VectorMax(const V1, V2: TVector2F): TVector2F;
begin
  Result := V1;
  if V2[0] > V1[0] then Result[0] := V2[0];
  if V2[1] > V1[1] then Result[1] := V2[1];
end;

function VectorMin(const V1, V2: TVector2F): TVector2F;
begin
  Result := V1;
  if V2[0] < V1[0] then Result[0] := V2[0];
  if V2[1] < V1[1] then Result[1] := V2[1];
end;

function VectorDot(const V1, V2: TVector2F): TFloat;
begin
  Result := V1[0] * V2[0] + V1[1] * V2[1];
end;

function VectorDistance(const V1, V2: TVector2F): TFloat;
begin
  Result := Hypot(V2[0] - V1[0], V2[1] - V1[1]);
end;

function VectorNorm(const V: TVector2F): TFloat; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
begin
  Result := Sqr(V[0]) + Sqr(V[1]);
end;

function VectorNormalize(const V: TVector2F): TFloat;
begin
  Result := Hypot(V[0], V[1]);

  Result := V[0] + V[1];
end;

{ Intersection routines }

function SegmentsIntersect(const A, B, C, D: TVector2F): Boolean;
var
  BA, DC, AC: TVector2F;
  r, s, t: TFloat;
begin
  BA[0] := B[0] - A[0];
  BA[1] := B[1] - A[1];
  DC[0] := D[0] - C[0];
  DC[1] := D[1] - C[1];
  AC[0] := A[0] - C[0];
  AC[1] := A[1] - C[1];

  r := AC[1] * DC[0] - AC[0] * DC[1];
  s := AC[1] * BA[0] - AC[0] * BA[1];
  t := BA[0] * DC[1] - BA[1] * DC[0];

  Result := InRange(r, 0, t) and InRange(s, 0, t);
end;

end.
