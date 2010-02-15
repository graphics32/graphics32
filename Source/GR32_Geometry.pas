unit GR32_Geometry;

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
 * The Original Code is Additional Math Routines for Graphics32
 *
 * The Initial Developers of the Original Code are
 * Mattias Andersson <mattias@centaurix.com>
 * Michael Hansen <dyster_tid@hotmail.com>
 *
 * Portions created by the Initial Developers are Copyright (C) 2005-2009
 * the Initial Developers. All Rights Reserved.
 *
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  Math, GR32, GR32_Math;

type
  PFixedVector = ^TFixedVector;
  TFixedVector = TFixedPoint;

  PFloatVector = ^TFloatVector;
  TFloatVector = TFloatPoint;

function Add(const V1, V2: TFloatVector): TFloatVector;  overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function Add(const V: TFloatVector; Value: TFloat): TFloatVector; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function Sub(const V1, V2: TFloatVector): TFloatVector; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function Sub(const V: TFloatVector; Value: TFloat): TFloatVector; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function Mul(const V1, V2: TFloatVector): TFloatVector; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function Mul(const V: TFloatVector; Multiplier: TFloat): TFloatVector; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function Divide(const V: TFloatVector; Divisor: TFloat): TFloatVector; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function Divide(const V1, V2: TFloatVector): TFloatVector; overload;{$IFDEF USEINLINING} inline; {$ENDIF}

function Combine(const V1, V2: TFloatVector; W: TFloat): TFloatVector; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function AbsV(const V: TFloatVector): TFloatVector; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function Neg(const V: TFloatVector): TFloatVector; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function Average(const V1, V2: TFloatVector): TFloatVector; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function Max(const V1, V2: TFloatVector): TFloatVector; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function Min(const V1, V2: TFloatVector): TFloatVector; overload;{$IFDEF USEINLINING} inline; {$ENDIF}

function Dot(const V1, V2: TFloatVector): TFloat; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function Distance(const V1, V2: TFloatVector): TFloat; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function SqrDistance(const V1, V2: TFloatVector): TFloat; overload;{$IFDEF USEINLINING} inline; {$ENDIF}

// Fixed Overloads
function Add(const V1, V2: TFixedVector): TFixedVector;  overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function Add(const V: TFixedVector; Value: TFixed): TFixedVector; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function Sub(const V1, V2: TFixedVector): TFixedVector; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function Sub(const V: TFixedVector; Value: TFixed): TFixedVector; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function Mul(const V1, V2: TFixedVector): TFixedVector; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function Mul(const V: TFixedVector; Multiplier: TFixed): TFixedVector; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function Divide(const V: TFixedVector; Divisor: TFixed): TFixedVector; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function Divide(const V1, V2: TFixedVector): TFixedVector; overload;{$IFDEF USEINLINING} inline; {$ENDIF}

function Combine(const V1, V2: TFixedVector; W: TFixed): TFixedVector; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function AbsV(const V: TFixedVector): TFixedVector; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function Neg(const V: TFixedVector): TFixedVector; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function Average(const V1, V2: TFixedVector): TFixedVector; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function Max(const V1, V2: TFixedVector): TFixedVector; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function Min(const V1, V2: TFixedVector): TFixedVector; overload;{$IFDEF USEINLINING} inline; {$ENDIF}

function Dot(const V1, V2: TFixedVector): TFixed; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function Distance(const V1, V2: TFixedVector): TFixed; overload;{$IFDEF USEINLINING} inline; {$ENDIF}
function SqrDistance(const V1, V2: TFixedVector): TFixed; overload;{$IFDEF USEINLINING} inline; {$ENDIF}

implementation

function Add(const V1, V2: TFloatVector): TFloatVector;
begin
  Result.X := V1.X + V2.X;
  Result.Y := V1.Y + V2.Y;
end;

function Add(const V: TFloatVector; Value: TFloat): TFloatVector;
begin
  Result.X := V.X + Value;
  Result.Y := V.Y + Value;
end;

function Sub(const V1, V2: TFloatVector): TFloatVector;
begin
  Result.X := V1.X - V2.X;
  Result.Y := V1.Y - V2.Y;
end;

function Sub(const V: TFloatVector; Value: TFloat): TFloatVector;
begin
  Result.X := V.X - Value;
  Result.Y := V.Y - Value;
end;

function Mul(const V1, V2: TFloatVector): TFloatVector;
begin
  Result.X := V1.X * V2.X;
  Result.Y := V1.Y * V2.Y;
end;

function Mul(const V: TFloatVector; Multiplier: TFloat): TFloatVector;
begin
  Result.X := V.X * Multiplier;
  Result.Y := V.Y * Multiplier;
end;

function Divide(const V: TFloatVector; Divisor: TFloat): TFloatVector;
begin
  Divisor := 1 / Divisor;
  Result.X := V.X * Divisor;
  Result.Y := V.Y * Divisor;
end;

function Divide(const V1, V2: TFloatVector): TFloatVector;
begin
  Result.X := V1.X / V2.X;
  Result.Y := V1.Y / V2.Y;
end;

function Combine(const V1, V2: TFloatVector; W: TFloat): TFloatVector;
begin
  Result.X := V2.X + (V1.X - V2.X) * W;
  Result.Y := V2.Y + (V1.Y - V2.Y) * W;
end;

function AbsV(const V: TFloatVector): TFloatVector;
begin
  Result.X := System.Abs(V.X);
  Result.Y := System.Abs(V.Y);
end;

function Neg(const V: TFloatVector): TFloatVector;
begin
  Result.X := - V.X;
  Result.Y := - V.Y;
end;

function Average(const V1, V2: TFloatVector): TFloatVector;
begin
  Result.X := (V1.X + V2.X) * 0.5;
  Result.Y := (V1.Y + V2.Y) * 0.5;
end;

function Max(const V1, V2: TFloatVector): TFloatVector;
begin
  Result := V1;
  if V2.X > V1.X then Result.X := V2.X;
  if V2.Y > V1.Y then Result.Y := V2.Y;
end;

function Min(const V1, V2: TFloatVector): TFloatVector;
begin
  Result := V1;
  if V2.X < V1.X then Result.X := V2.X;
  if V2.Y < V1.Y then Result.Y := V2.Y;
end;

function Dot(const V1, V2: TFloatVector): TFloat;
begin
  Result := V1.X * V2.X + V1.Y * V2.Y;
end;

function Distance(const V1, V2: TFloatVector): TFloat;
begin
  Result := Hypot(V2.X - V1.X, V2.Y - V1.Y);
end;

function SqrDistance(const V1, V2: TFloatVector): TFloat;
begin
  Result := Sqr(V2.X - V1.X) + Sqr(V2.Y - V1.Y);
end;

// Fixed overloads

function Add(const V1, V2: TFixedVector): TFixedVector;
begin
  Result.X := V1.X + V2.X;
  Result.Y := V1.Y + V2.Y;
end;

function Add(const V: TFixedVector; Value: TFixed): TFixedVector;
begin
  Result.X := V.X + Value;
  Result.Y := V.Y + Value;
end;

function Sub(const V1, V2: TFixedVector): TFixedVector;
begin
  Result.X := V1.X - V2.X;
  Result.Y := V1.Y - V2.Y;
end;

function Sub(const V: TFixedVector; Value: TFixed): TFixedVector;
begin
  Result.X := V.X - Value;
  Result.Y := V.Y - Value;
end;

function Mul(const V1, V2: TFixedVector): TFixedVector;
begin
  Result.X := FixedMul(V1.X, V2.X);
  Result.Y := FixedMul(V1.Y, V2.Y);
end;

function Mul(const V: TFixedVector; Multiplier: TFixed): TFixedVector;
begin
  Result.X := FixedMul(V.X, Multiplier);
  Result.Y := FixedMul(V.Y, Multiplier);
end;

function Divide(const V: TFixedVector; Divisor: TFixed): TFixedVector;
var
  D: TFloat;
begin
  D := FIXEDONE / Divisor;
  Result.X := Round(V.X * D);
  Result.Y := Round(V.Y * D);
end;

function Divide(const V1, V2: TFixedVector): TFixedVector;
begin
  Result.X := FixedDiv(V1.X, V2.X);
  Result.Y := FixedDiv(V1.Y, V2.Y);
end;

function Combine(const V1, V2: TFixedVector; W: TFixed): TFixedVector;
begin
  Result.X := V2.X + FixedMul(V1.X - V2.X, W);
  Result.Y := V2.Y + FixedMul(V1.Y - V2.Y, W);
end;

function AbsV(const V: TFixedVector): TFixedVector;
begin
  Result.X := System.Abs(V.X);
  Result.Y := System.Abs(V.Y);
end;

function Neg(const V: TFixedVector): TFixedVector;
begin
  Result.X := - V.X;
  Result.Y := - V.Y;
end;

function Average(const V1, V2: TFixedVector): TFixedVector;
begin
  Result.X := (V1.X + V2.X) div 2;
  Result.Y := (V1.Y + V2.Y) div 2;
end;

function Max(const V1, V2: TFixedVector): TFixedVector;
begin
  Result := V1;
  if V2.X > V1.X then Result.X := V2.X;
  if V2.Y > V1.Y then Result.Y := V2.Y;
end;

function Min(const V1, V2: TFixedVector): TFixedVector;
begin
  Result := V1;
  if V2.X < V1.X then Result.X := V2.X;
  if V2.Y < V1.Y then Result.Y := V2.Y;
end;

function Dot(const V1, V2: TFixedVector): TFixed;
begin
  Result := FixedMul(V1.X, V2.X) + FixedMul(V1.Y, V2.Y);
end;

function Distance(const V1, V2: TFixedVector): TFixed;
begin
  Result := Fixed(Hypot((V2.X - V1.X) * FixedToFloat, (V2.Y - V1.Y) * FixedToFloat));
end;

function SqrDistance(const V1, V2: TFixedVector): TFixed;
begin
  Result := FixedSqr(V2.X - V1.X) + FixedSqr(V2.Y - V1.Y);
end;

end.
