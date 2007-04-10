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
  PFixedVector = ^TFixedVector;
  TFixedVector = TFixedPoint;

  PFloatVector = ^TFloatVector;
  TFloatVector = TFloatPoint;

function Add(const V1, V2: TFloatVector): TFloatVector;  overload;
function Add(const V: TFloatVector; Value: TFloat): TFloatVector; overload;
function Sub(const V1, V2: TFloatVector): TFloatVector; overload;
function Sub(const V: TFloatVector; Value: TFloat): TFloatVector; overload;
function Mul(const V1, V2: TFloatVector): TFloatVector; overload;
function Mul(const V: TFloatVector; Multiplier: TFloat): TFloatVector; overload;
function Divide(const V: TFloatVector; Divisor: TFloat): TFloatVector; overload;
function Divide(const V1, V2: TFloatVector): TFloatVector; overload;

function Combine(const V1, V2: TFloatVector; W: TFloat): TFloatVector; overload;
function Abs(const V: TFloatVector): TFloatVector; overload;
function Neg(const V: TFloatVector): TFloatVector;
function Average(const V1, V2: TFloatVector): TFloatVector; overload;
function Max(const V1, V2: TFloatVector): TFloatVector; overload;
function Min(const V1, V2: TFloatVector): TFloatVector; overload;

function Dot(const V1, V2: TFloatVector): TFloat; overload;
function Distance(const V1, V2: TFloatVector): TFloat; overload;


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

function Abs(const V: TFloatVector): TFloatVector;
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

end.
