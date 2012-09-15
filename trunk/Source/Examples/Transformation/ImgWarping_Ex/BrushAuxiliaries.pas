unit BrushAuxiliaries;
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
 * The Original Code is Image Warping Example
 *
 * The Initial Developers of the Original Code is:
 *
 * Michael Hansen <dyster_tid@hotmail.com>
 * Mattias Andersson <mattias@centaurix.com>
 *
 * Portions created by the Initial Developer are Copyright (C) 2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 *
 * ***** END LICENSE BLOCK ***** *)

interface

uses
  Classes, GR32, GR32_Layers;

type
  { TBrushLayer }
  TBrushLayer = class(TPositionedLayer)
  private
    FCenter: TPoint;
    FRadius: Integer;
    procedure SetCenter(const Value: TPoint);
    procedure SetRadius(const Value: Integer);
  protected
    procedure Paint(Buffer: TBitmap32); override;
  public
    property Radius: Integer read FRadius write SetRadius;
    property Center: TPoint read FCenter write SetCenter;
  end;

  { TGenericBrush }
  TGenericBrush = class
  private
    FPressure: Single;
    FPinch: Single;
    FFeather: Single;
    procedure SetPinch(const Value: Single);
    procedure SetPressure(const Value: Single);
    procedure SetFeather(const Value: Single);
  public
    constructor Create;
    property Pressure : Single read FPressure write SetPressure;
    property Pinch: Single read FPinch write SetPinch;
    property Feather: Single read FFeather write SetFeather;
    function Weight(X, Y: Single): Single;
  end;

function PinchPop(X, Pinch: Single): Single;
function FeatherFunc(R, Feather: Single): Single;

implementation

uses Math, GR32_LowLevel;

function PinchPop(X, Pinch: Single): Single;
begin
  if (X <= -1) or (X >= 1) then
  begin
    Result := 0;
    Exit;
  end
  else if Pinch = 1 then
  begin
    Result := 1;
  end
  else if Pinch = -1 then
  begin
    if Fixed(X) = 0 then
      Result := 1
    else
      Result := 0;
  end
  else
  begin
    if Pinch > 0 then
      Pinch := 1 / (1 - Pinch)
    else
      Pinch := Pinch + 1;
    Result := Power(Abs(1 - Abs(Power(X, Pinch))), 1 / Pinch);
  end;
end;

function FeatherFunc(R, Feather: Single): Single;
begin
  if Feather <= 0 then
    Result := 1
  else
  begin
    Feather := 1 / Feather;
    Result := (1 - R) * Feather;
    Result := Constrain(Result, 0, 1);
  end;
end;

{ TBrushLayer }

procedure TBrushLayer.Paint(Buffer: TBitmap32);
var
  I: Integer;
  P: TFixedPoint;
  Steps: Integer;
  Scale: Single;
begin
  Buffer.MoveToF(Center.X + Radius, Center.Y);
  Buffer.PenColor := $A0ffffff;
  Steps := Radius + 4;
  If Odd(Steps) then Inc(Steps);
  Scale := 2*PI/STEPS;
  for I := 1 to STEPS do
  begin
    Buffer.PenColor := (not Buffer.PenColor) and $00ffffff + $A0000000;
    P.X := Fixed(Center.X + Cos(I * SCALE) * Radius);
    P.Y := Fixed(Center.Y + Sin(I * SCALE) * Radius);
    Buffer.LineToXS(P.X, P.Y);
  end;
end;

procedure TBrushLayer.SetCenter(const Value: TPoint);
begin
  if (Value.X <> FCenter.X) or (Value.Y <> FCenter.Y) then
  begin
    // Clearing old position
    with FCenter do
      Changed(Rect(X - Radius, Y - Radius, X + Radius + 1, Y + Radius + 1));
    FCenter := Value;
    // Painting new position
    with FCenter do
      Changed(Rect(X - Radius, Y - Radius, X + Radius + 1, Y + Radius + 1));
  end;
end;

procedure TBrushLayer.SetRadius(const Value: Integer);
begin
  FRadius := Value;
  with FCenter do
    Changed(Rect(X - Radius, Y - Radius, X + Radius + 1, Y + Radius + 1));
end;

{ TGenericBrush }

function TGenericBrush.Weight(X, Y: Single): Single;
var
  R: Single;
begin
  R := Hypot(X, Y);
  Result := PinchPop(R, Pinch);
  Result := Result * FeatherFunc(R, Feather);
  Result := Constrain(Result * Pressure, 0, 1);
end;

constructor TGenericBrush.Create;
begin
  inherited;
  FPinch := 0;
  FPressure := 1.0;
  FFeather := 0;
end;

procedure TGenericBrush.SetPinch(const Value: Single);
begin
  FPinch := EnsureRange(Value, -1, 1);
end;

procedure TGenericBrush.SetPressure(const Value: Single);
begin
  FPressure := EnsureRange(Value, 0, 1);
end;

procedure TGenericBrush.SetFeather(const Value: Single);
begin
  FFeather := EnsureRange(Value, 0, 1);
end;

end.
