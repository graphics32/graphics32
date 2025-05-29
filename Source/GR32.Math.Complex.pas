unit GR32.Math.Complex;


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
 * The Original Code is Complex Math for Delphi
 *
 * The Initial Developers of the Original Code are
 * Hallvard Vassbotn, Anders Melander
 *
 * Portions created by the Initial Developer are Copyright (C) 2006
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

uses
  Types,
  Variants,
  SysUtils,
  Math;

//------------------------------------------------------------------------------
//
//      TComplex
//
//------------------------------------------------------------------------------
// Originally adapted from code by Hallvard Vassbotn.
//------------------------------------------------------------------------------
type
  TComplex = record
  strict private
    class constructor Create;
    procedure Defuzz;
    class procedure AssertImaginaryIsZero(const AValue: TComplex); static;
  public
    var
      Real: Double;
      Imaginary: Double;

    class var
      Symbol: string;                 // defaults to 'i'
      SymbolBeforeImaginary: Boolean; // defaults to false
      DefuzzAtZero: Boolean;          // defaults to true

    class function From(const AReal: Double): TComplex; overload; static;
    class function From(const AReal, AImaginary: Double): TComplex; overload; static;
    class function FromPolar(const AModulus, APhase: Double): TComplex; static;

    function Modulus: Double;
    function Phase: Double;

    function IsInfinite: Boolean;
    function IsComplexInfinite: Boolean;
    function IsNaN: Boolean;
    function IsZero: Boolean;

    class function Abs(const AValue: TComplex): Double; static; // Magnitude
    class function AbsSqr(const AValue: TComplex): Double; static;
    class function Angle(const AValue: TComplex): Double; static;
    class function Sign(const AValue: TComplex): TComplex; static;
    class function Conjugate(const AValue: TComplex): TComplex; static;
    class function Inverse(const AValue: TComplex): TComplex; static;
    class function Exp(const AValue: TComplex): TComplex; static;
    class function Ln(const AValue: TComplex): TComplex; static;
    class function Log10(const AValue: TComplex): TComplex; static;
    class function Log2(const AValue: TComplex): TComplex; static;
    class function LogN(const AValue: TComplex; const X: Double): TComplex; static;
    class function Sqr(const AValue: TComplex): TComplex; static;
    class function Sqrt(const AValue: TComplex): TComplex; static;
    class function Power(const AValue, APower: TComplex): TComplex; static;

    class function Cos(const AValue: TComplex): TComplex; static;
    class function Sin(const AValue: TComplex): TComplex; static;
    class function Tan(const AValue: TComplex): TComplex; static;
    class function Cot(const AValue: TComplex): TComplex; static; // Cotan
    class function Csc(const AValue: TComplex): TComplex; static; // Cosecant
    class function Sec(const AValue: TComplex): TComplex; static; // Secant
    class function ArcCos(const AValue: TComplex): TComplex; static;
    class function ArcSin(const AValue: TComplex): TComplex; static;
    class function ArcTan(const AValue: TComplex): TComplex; static;
    class function ArcCot(const AValue: TComplex): TComplex; static;
    class function ArcCsc(const AValue: TComplex): TComplex; static;
    class function ArcSec(const AValue: TComplex): TComplex; static;
    class function CosH(const AValue: TComplex): TComplex; static;
    class function SinH(const AValue: TComplex): TComplex; static;
    class function TanH(const AValue: TComplex): TComplex; static;
    class function CotH(const AValue: TComplex): TComplex; static;
    class function CscH(const AValue: TComplex): TComplex; static;
    class function SecH(const AValue: TComplex): TComplex; static;
    class function ArcCosH(const AValue: TComplex): TComplex; static;
    class function ArcSinH(const AValue: TComplex): TComplex; static;
    class function ArcTanH(const AValue: TComplex): TComplex; static;
    class function ArcCotH(const AValue: TComplex): TComplex; static;
    class function ArcCscH(const AValue: TComplex): TComplex; static;
    class function ArcSecH(const AValue: TComplex): TComplex; static;

    class function Parse(const AText: string): TComplex; overload; static;
    class function Parse(const AText: string; const AFormatSettings: TFormatSettings): TComplex; overload; static;

    function ToString: string; overload;
    function ToString(const AFormatSettings: TFormatSettings): string; overload;

    class function Frac(const AValue: TComplex): Double; static;
    class function Int(const AValue: TComplex): Double; static;

    class function Compare(const Left, Right: TComplex): Integer; static;
    class function Equals(const Left, Right: TComplex): Boolean; static;

{$IFNDEF FPC}
    class operator Round(const AValue: TComplex): Int64;                                        {$if defined(StaticOperators)}static;{$ifend}
    class operator Trunc(const AValue: TComplex): Int64;                                        {$if defined(StaticOperators)}static;{$ifend}
{$ENDIF}

    class operator Equal(const Left, Right: TComplex): Boolean;                                 {$if defined(StaticOperators)}static;{$ifend}
    class operator NotEqual(const Left, Right: TComplex): Boolean;                              {$if defined(StaticOperators)}static;{$ifend}
    class operator LessThan(const Left, Right: TComplex): Boolean;                              {$if defined(StaticOperators)}static;{$ifend}
    class operator LessThanOrEqual(const Left, Right: TComplex): Boolean;
    class operator GreaterThan(const Left, Right: TComplex): Boolean;                           {$if defined(StaticOperators)}static;{$ifend}
    class operator GreaterThanOrEqual(const Left, Right: TComplex): Boolean;                    {$if defined(StaticOperators)}static;{$ifend}

    class operator Add(const Left, Right: TComplex): TComplex;                                  {$if defined(StaticOperators)}static;{$ifend}
    class operator Add(const Left: Double; const Right: TComplex): TComplex;                    {$if defined(StaticOperators)}static;{$ifend}
    class operator Add(const Left: TComplex; const Right: Double): TComplex;                    {$if defined(StaticOperators)}static;{$ifend}
    class operator Subtract(const Left, Right: TComplex): TComplex;                             {$if defined(StaticOperators)}static;{$ifend}
    class operator Subtract(const Left: Double; const Right: TComplex): TComplex;               {$if defined(StaticOperators)}static;{$ifend}
    class operator Subtract(const Left: TComplex; const Right: Double): TComplex;               {$if defined(StaticOperators)}static;{$ifend}
    class operator Multiply(const Left, Right: TComplex): TComplex;                             {$if defined(StaticOperators)}static;{$ifend}
    class operator Multiply(const Left: Double; const Right: TComplex): TComplex;               {$if defined(StaticOperators)}static;{$ifend}
    class operator Multiply(const Left: TComplex; const Right: Double): TComplex;
    class operator Divide(const Left, Right: TComplex): TComplex;                               {$if defined(StaticOperators)}static;{$ifend}
    class operator Divide(const Left: Double; const Right: TComplex): TComplex;                 {$if defined(StaticOperators)}static;{$ifend}
    class operator Divide(const Left: TComplex; const Right: Double): TComplex;                 {$if defined(StaticOperators)}static;{$ifend}
    class operator Negative(const AValue: TComplex): TComplex;                                  {$if defined(StaticOperators)}static;{$ifend}

    class operator Implicit(const AValue: Double): TComplex;                                    {$if defined(StaticOperators)}static;{$ifend}
    class operator Implicit(const AValue: Integer): TComplex;                                   {$if defined(StaticOperators)}static;{$ifend}
    class operator Implicit(const AValue: Int64): TComplex;
    class operator Implicit(const AValue: Variant): TComplex;                                   {$if defined(StaticOperators)}static;{$ifend}
    class operator Implicit(const AValue: string): TComplex;

    class operator Explicit(const AValue: TComplex): Double;                                    {$if defined(StaticOperators)}static;{$ifend}
    class operator Explicit(const AValue: TComplex): Integer;                                   {$if defined(StaticOperators)}static;{$ifend}
    class operator Explicit(const AValue: TComplex): Int64;                                     {$if defined(StaticOperators)}static;{$ifend}
    class operator Explicit(const AValue: TComplex): string;                                    {$if defined(StaticOperators)}static;{$ifend}
  end;


const
  ComplexOne: TComplex = (Real: 1; Imaginary: 0);
  ComplexMinusOne: TComplex = (Real: -1; Imaginary: 0);
  ComplexImaginaryOne: TComplex = (Real: 0; Imaginary: 1);
  ComplexImaginaryMinusOne: TComplex = (Real: 0; Imaginary: -1);
  ComplexHalfPi: TComplex = (Real: PI/2; Imaginary: 0);
  ComplexZero: TComplex = (Real: 0; Imaginary: 0);
  ComplexInfinity: TComplex = (Real: NaN; Imaginary: NaN);
  ComplexPositiveInfinity: TComplex = (Real: Math.Infinity; Imaginary: 0);
  ComplexNegativeInfinity: TComplex = (Real: Math.NegInfinity; Imaginary: 0);


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  SysConst;

//------------------------------------------------------------------------------

procedure ZeroDivideError;
begin
  raise EZeroDivide.Create(SysConst.SDivByZero);
end;

//------------------------------------------------------------------------------

class constructor TComplex.Create;
begin
  Symbol := 'i';
  SymbolBeforeImaginary := False;
  DefuzzAtZero := True;
end;

procedure TComplex.Defuzz;
begin
  if Math.IsZero(Real) then
    Real := 0;
  if Math.IsZero(Imaginary) then
    Imaginary := 0;
end;

class procedure TComplex.AssertImaginaryIsZero(const AValue: TComplex);
begin
  if not Math.IsZero(AValue.Imaginary) then
    raise EConvertError.CreateFmt('Can not simplify, imaginary part is non-zero [%s]', [AValue.ToString]);
end;

//------------------------------------------------------------------------------

class function TComplex.From(const AReal: Double): TComplex;
begin
  Result.Real := AReal;
  Result.Imaginary := 0;
  if DefuzzAtZero then
    Result.Defuzz;
end;

class function TComplex.From(const AReal, AImaginary: Double): TComplex;
begin
  Result.Real := AReal;
  Result.Imaginary := AImaginary;
  if DefuzzAtZero then
    Result.Defuzz;
end;

class function TComplex.FromPolar(const AModulus, APhase: Double): TComplex;
begin
  Result := TComplex.From(
    AModulus * System.Cos(APhase),
    AModulus * System.Sin(APhase));
end;

//------------------------------------------------------------------------------

function TComplex.Modulus: Double;
begin
  Result := System.Sqr(Real) + System.Sqr(Imaginary);
end;

function TComplex.Phase: Double;
begin
  Result := ArcTan2(Imaginary, Real);
end;

//------------------------------------------------------------------------------

function TComplex.IsZero: Boolean;
begin
  Result := Math.IsZero(Real) and Math.IsZero(Imaginary);
end;

function TComplex.IsInfinite: Boolean;
begin
  Result := Math.IsInfinite(Real) or Math.IsInfinite(Imaginary);
end;

function TComplex.IsComplexInfinite: Boolean;
begin
  Result := Math.IsNaN(Real) and Math.IsNaN(Imaginary);
end;

function TComplex.IsNaN: Boolean;
begin
  Result := Math.IsNaN(Real) xor Math.IsNaN(Imaginary);
end;

//------------------------------------------------------------------------------

class function TComplex.Abs(const AValue: TComplex): Double;
begin
  Result := System.Sqrt(AbsSqr(AValue));
end;

class function TComplex.AbsSqr(const AValue: TComplex): Double;
begin
  Result := AValue.Modulus;
end;

class function TComplex.Angle(const AValue: TComplex): Double;
begin
  Result := AValue.Phase;
end;

class function TComplex.Sign(const AValue: TComplex): TComplex;
begin
  if AValue.IsZero then
    Result := ComplexZero
  else
    Result := AValue / Sqrt(From(AbsSqr(AValue), 0));
end;

class function TComplex.Conjugate(const AValue: TComplex): TComplex;
begin
  Result.Real := AValue.Real;
  Result.Imaginary := -AValue.Imaginary;
end;

class function TComplex.Inverse(const AValue: TComplex): TComplex;
var
  LDenominator: Double;
begin
  LDenominator := AbsSqr(AValue);
  if Math.IsZero(LDenominator) then
    ZeroDivideError;
  Result := From(AValue.Real / LDenominator, -(AValue.Imaginary / LDenominator));
end;

class function TComplex.Exp(const AValue: TComplex): TComplex;
var
  LExp: Double;
begin
  LExp := System.Exp(AValue.Real);
  Result := From(LExp * System.Cos(AValue.Imaginary),
                 LExp * System.Sin(AValue.Imaginary));
end;

class operator TComplex.Explicit(const AValue: TComplex): string;
begin
  Result := AValue.ToString;
end;

class function TComplex.Ln(const AValue: TComplex): TComplex;
begin
  if AValue.IsZero then
    Result := ComplexNegativeInfinity
  else
    Result := From(System.Ln(AValue.Modulus), AValue.Phase);
end;

class function TComplex.Log10(const AValue: TComplex): TComplex;
begin
  if AValue.IsZero then
    Result := ComplexNegativeInfinity
  else
    Result := From(System.Ln(AValue.Modulus), AValue.Phase) /
              From(System.Ln(10), 0);
end;

class function TComplex.Log2(const AValue: TComplex): TComplex;
begin
  if AValue.IsZero then
    Result := ComplexNegativeInfinity
  else
    Result := From(System.Ln(AValue.Modulus), AValue.Phase) /
              From(System.Ln(2), 0);
end;

class function TComplex.LogN(const AValue: TComplex; const X: Double): TComplex;
begin
  if AValue.IsZero and (X > 0) and (X <> 1) then
    Result := ComplexNegativeInfinity
  else
    Result := From(System.Ln(AValue.Modulus), AValue.Phase) /
              Ln(From(X, 0));
end;

class function TComplex.Sqr(const AValue: TComplex): TComplex;
begin
  Result := From(System.Sqr(AValue.Real) - System.Sqr(AValue.Imaginary),
                 2.0 * (AValue.Real * AValue.Imaginary));
end;

class function TComplex.Sqrt(const AValue: TComplex): TComplex;
var
  LValue: Double;
begin
  if AValue.IsZero then
    Result := ComplexZero
  else
  if (AValue.Real > 0) then
  begin
    LValue := Abs(AValue) + AValue.Real;
    Result := From(System.Sqrt(LValue * 0.5),
                   AValue.Imaginary / System.Sqrt(LValue * 2));
  end else
  begin
    LValue := Abs(AValue) - AValue.Real;
    if (AValue.Imaginary < 0) then
      Result := From(System.Abs(AValue.Imaginary) / System.Sqrt(LValue * 2),
                     -System.Sqrt(LValue * 0.5))
    else
      Result := From(System.Abs(AValue.Imaginary) / System.Sqrt(LValue * 2),
                     System.Sqrt(LValue * 0.5));
  end;
end;

class function TComplex.Power(const AValue, APower: TComplex): TComplex;
begin
  if Math.IsZero(AbsSqr(AValue)) then
    if Math.IsZero(AbsSqr(APower)) then
      Result := ComplexOne
    else
      Result := ComplexZero
  else
    Result := Exp(Ln(AValue) * APower);
end;

//------------------------------------------------------------------------------

class function TComplex.Cos(const AValue: TComplex): TComplex;
begin
  Result := From(System.Cos(AValue.Real) * Math.CosH(AValue.Imaginary),
                 -System.Sin(AValue.Real) * Math.SinH(AValue.Imaginary));
end;

class function TComplex.Sin(const AValue: TComplex): TComplex;
begin
  Result := From(System.Sin(AValue.Real) * Math.CosH(AValue.Imaginary),
                 System.Cos(AValue.Real) * Math.SinH(AValue.Imaginary));
end;

class function TComplex.Tan(const AValue: TComplex): TComplex;
var
  LDenominator: Double;
begin
  if (AValue = ComplexHalfPi) then
    Result := ComplexInfinity
  else
  begin
    LDenominator := System.Cos(2.0 * AValue.Real) + Math.CosH(2.0 * AValue.Imaginary);
    if Math.IsZero(LDenominator) then
      ZeroDivideError;
    Result := From(System.Sin(2.0 * AValue.Real) / LDenominator,
                   Math.SinH(2.0 * AValue.Imaginary) / LDenominator);
  end;
end;

class function TComplex.Cot(const AValue: TComplex): TComplex;
begin
  if AValue.IsZero then
    Result := ComplexInfinity
  else
    Result := Cos(AValue) / Sin(AValue);
end;

class function TComplex.Csc(const AValue: TComplex): TComplex;
begin
  if AValue.IsZero then
    Result := ComplexInfinity
  else
    Result := ComplexOne / Sin(AValue);
end;

class function TComplex.Sec(const AValue: TComplex): TComplex;
begin
  if AValue.IsZero then
    Result := ComplexInfinity
  else
    Result := ComplexOne / Cos(AValue);
end;

class function TComplex.ArcCos(const AValue: TComplex): TComplex;
begin
  Result := ComplexHalfPi + (ComplexImaginaryOne * Ln((ComplexImaginaryOne * AValue) + Sqrt(ComplexOne - Sqr(AValue))));
end;

class function TComplex.ArcSin(const AValue: TComplex): TComplex;
begin
  Result := ComplexImaginaryMinusOne * Ln((ComplexImaginaryOne * AValue) + Sqrt(ComplexOne - Sqr(AValue)));
end;

class function TComplex.ArcTan(const AValue: TComplex): TComplex;
begin
  Result := ComplexHalfPi * (Ln(ComplexOne - (ComplexImaginaryOne * AValue)) - Ln((ComplexImaginaryOne * AValue) - ComplexOne));
end;

class function TComplex.ArcCot(const AValue: TComplex): TComplex;
begin
  Result := ArcTan(Inverse(AValue));
end;

class function TComplex.ArcCsc(const AValue: TComplex): TComplex;
begin
  if AValue.IsZero then
    Result := ComplexInfinity
  else
    Result := ArcSin(Inverse(AValue));
end;

class function TComplex.ArcSec(const AValue: TComplex): TComplex;
begin
  if AValue.IsZero then
    Result := ComplexInfinity
  else
    Result := ArcCos(Inverse(AValue));
end;

class function TComplex.CosH(const AValue: TComplex): TComplex;
begin
  Result := From(Math.CosH(AValue.Real) * System.Cos(AValue.Imaginary),
                 Math.SinH(AValue.Real) * System.Sin(AValue.Imaginary));
end;

class function TComplex.SinH(const AValue: TComplex): TComplex;
begin
  Result := From(Math.CosH(AValue.Real) * System.Cos(AValue.Imaginary),
                 Math.SinH(AValue.Real) * System.Sin(AValue.Imaginary));
end;

class function TComplex.TanH(const AValue: TComplex): TComplex;
begin
  if AValue.IsZero then
    Result := ComplexZero
  else
    Result := SinH(AValue) / CosH(AValue);
end;

class function TComplex.CotH(const AValue: TComplex): TComplex;
begin
  if AValue.IsZero then
    Result := ComplexInfinity
  else
    Result := Inverse(TanH(AValue));
end;

class function TComplex.CscH(const AValue: TComplex): TComplex;
begin
  if AValue.IsZero then
    Result := ComplexInfinity
  else
    Result := Inverse(SinH(AValue));
end;

class function TComplex.SecH(const AValue: TComplex): TComplex;
begin
  Result := Inverse(CosH(AValue));
end;

class function TComplex.ArcCosH(const AValue: TComplex): TComplex;
begin
  Result := Ln(AValue + Sqrt(AValue - ComplexOne) * Sqrt(AValue + ComplexOne));
end;

class function TComplex.ArcSinH(const AValue: TComplex): TComplex;
begin
  Result := ArcSin(AValue * ComplexImaginaryOne) * ComplexImaginaryMinusOne;
end;

class function TComplex.ArcTanH(const AValue: TComplex): TComplex;
begin
  if AValue = ComplexOne then
    Result := ComplexPositiveInfinity
  else
  if AValue = ComplexMinusOne then
    Result := ComplexNegativeInfinity
  else
    Result := ArcTan(AValue * ComplexImaginaryOne) * ComplexImaginaryMinusOne;
end;

class function TComplex.ArcCotH(const AValue: TComplex): TComplex;
begin
  if AValue = ComplexOne then
    Result := ComplexPositiveInfinity
  else
  if AValue = ComplexMinusOne then
    Result := ComplexNegativeInfinity
  else
    Result := ArcTanH(Inverse(AValue));
end;

class function TComplex.ArcCscH(const AValue: TComplex): TComplex;
begin
  if AValue.IsZero then
    Result := ComplexInfinity
  else
    Result := ArcSinH(Inverse(AValue));
end;

class function TComplex.ArcSecH(const AValue: TComplex): TComplex;
begin
  if AValue.IsZero then
    Result := ComplexInfinity
  else
    Result := ArcCosH(Inverse(AValue));
end;

//------------------------------------------------------------------------------

class function TComplex.Parse(const AText: string): TComplex;
begin
  Result := TComplex.Parse(AText, FormatSettings);
end;

class function TComplex.Parse(const AText: string; const AFormatSettings: TFormatSettings): TComplex;

  function ParseNumber(const AText: string; out ARest: string; out ANumber: Double): Boolean;
  var
    LAt: Integer;
    LFirstPart: string;
  begin
    Result := True;
    ARest := '';
    Val(AText, ANumber, LAt); // TODO : Use something else
    if LAt <> 0 then
    begin
      ARest := Copy(AText, LAt, MaxInt);
      LFirstPart := Copy(AText, 1, LAt - 1);
      Val(LFirstPart, ANumber, LAt); // TODO : Ditto
      if LAt <> 0 then
        Result := False;
    end;
  end;

  function ParseWhiteSpace(const AText: string; out ARest: string): Boolean;
  var
    LAt: Integer;
  begin
    LAt := 1;
    ARest := '';
    if AText <> '' then
    begin
      while AText[LAt] = ' ' do
        Inc(LAt);
      ARest := Copy(AText, LAt, MaxInt);
    end;
    Result := ARest <> '';
  end;

  procedure ParseError(const AText, ALeftOver, AMessage: string);
  begin
    raise EConvertError.CreateFmt('%s [%s<?>%s]', [AMessage,
      Copy(AText, 1, Length(AText) - Length(ALeftOver)),
      Copy(AText, Length(AText) - Length(ALeftOver) + 1, MaxInt)]);
  end;

var
  LPart, LLeftover: string;
  LReal, LImaginary: Double;
  LSign: Integer;
begin
  // where to start?
  LLeftover := AText;

  // first get the real portion
  if not ParseNumber(LLeftover, LPart, LReal) then
    ParseError(AText, LLeftover, 'Can not parse real portion');

  // is that it?
  if not ParseWhiteSpace(LPart, LLeftover) then
    Result := TComplex.From(LReal)

  // if there is more then parse the TComplex part
  else
  begin

    // look for the concat symbol
    LSign := 1;
    if LLeftover[1] = '-' then
      LSign := -1
    else
    if LLeftover[1] <> '+' then
      ParseError(AText, LLeftover, 'Required sign symbol missing (''+'' or ''-'')');
    LPart := Copy(LLeftover, 2, MaxInt);

    // skip any whitespace
    ParseWhiteSpace(LPart, LLeftover);

    // symbol before?
    if SymbolBeforeImaginary then
    begin
      if not SameText(Copy(LLeftOver, 1, Length(Symbol)), Symbol) then
        ParseError(AText, LLeftover, Format('Required ''%s'' symbol missing', [Symbol]));
      LPart := Copy(LLeftover, Length(Symbol) + 1, MaxInt);

      // skip any whitespace
      ParseWhiteSpace(LPart, LLeftover);
    end;

    // imaginary part
    if not ParseNumber(LLeftover, LPart, LImaginary) then
      ParseError(AText, LLeftover, 'Can not parse imaginary portion');

    // correct for sign
    LImaginary := LImaginary * LSign;

    // symbol after?
    if not SymbolBeforeImaginary then
    begin
      // skip any whitespace
      ParseWhiteSpace(LPart, LLeftover);

      // make sure there is symbol!
      if not SameText(Copy(LLeftOver, 1, Length(Symbol)), Symbol) then
        ParseError(AText, LLeftover, Format('Required ''%s'' symbol missing', [Symbol]));
      LPart := Copy(LLeftover, Length(Symbol) + 1, MaxInt);
    end;

    // make sure the rest of the string is whitespaces
    ParseWhiteSpace(LPart, LLeftover);
    if LLeftover <> '' then
      ParseError(AText, LLeftover, 'Unexpected characters');

    // make it then
    Result := TComplex.From(LReal, LImaginary);
  end;
end;

//------------------------------------------------------------------------------

function TComplex.ToString: string;
begin
  Result := ToString(FormatSettings);
end;

function TComplex.ToString(const AFormatSettings: TFormatSettings): string;
const
  cFormats: array[Boolean] of string = ('%2:s %1:s %3:s%0:s',
                                        '%2:s %1:s %0:s%3:s');
  cSign: array[Boolean] of string = ('-', '+');
var
  RealStr, IStr: string;
begin
  RealStr := FloatToStr(Real, AFormatSettings);
  IStr := FloatToStr(System.Abs(Imaginary), AFormatSettings);

  Result := Format(cFormats[SymbolBeforeImaginary], [Symbol, cSign[Imaginary >= 0], RealStr, IStr]);
end;

//------------------------------------------------------------------------------

class function TComplex.Frac(const AValue: TComplex): Double;
begin
  Result := System.Frac(Double(AValue));
end;

class function TComplex.Int(const AValue: TComplex): Double;
begin
  Result := System.Int(Double(AValue));
end;

//------------------------------------------------------------------------------

class function TComplex.Compare(const Left, Right: TComplex): Integer;
begin
  if (Left = Right) then
    Result := 0
  else
    Result := -1;
end;

class function TComplex.Equals(const Left, Right: TComplex): Boolean;
begin
  Result := (Left = Right);
end;

//------------------------------------------------------------------------------

{$IFNDEF FPC}
class operator TComplex.Round(const AValue: TComplex): Int64;
begin
  Result := System.Round(Double(AValue));
end;

class operator TComplex.Trunc(const AValue: TComplex): Int64;
begin
  Result := System.Trunc(Double(AValue));
end;
{$ENDIF}

//------------------------------------------------------------------------------

class operator TComplex.Equal(const Left, Right: TComplex): Boolean;
begin
  Result := SameValue(Left.Real, Right.Real) and
            SameValue(Left.Imaginary, Right.Imaginary);
end;

class operator TComplex.NotEqual(const Left, Right: TComplex): Boolean;
begin
  Result := not (Left = Right);
end;

class operator TComplex.LessThan(const Left, Right: TComplex): Boolean;
begin
  Result := False;
end;

class operator TComplex.LessThanOrEqual(const Left, Right: TComplex): Boolean;
begin
  Result := (Left = Right);
end;

class operator TComplex.GreaterThan(const Left, Right: TComplex): Boolean;
begin
  Result := False;
end;

class operator TComplex.GreaterThanOrEqual(const Left, Right: TComplex): Boolean;
begin
  Result := (Left = Right);
end;

//------------------------------------------------------------------------------

class operator TComplex.Add(const Left, Right: TComplex): TComplex;
begin
  Result.Real := Left.Real + Right.Real;
  Result.Imaginary := Left.Imaginary + Right.Imaginary;
end;

class operator TComplex.Add(const Left: TComplex; const Right: Double): TComplex;
begin
  Result.Real := Left.Real + Right;
  Result.Imaginary := Left.Imaginary;
end;

class operator TComplex.Add(const Left: Double; const Right: TComplex): TComplex;
begin
  Result.Real := Left + Right.Real;
  Result.Imaginary := Right.Imaginary;
end;

class operator TComplex.Subtract(const Left, Right: TComplex): TComplex;
begin
  Result.Real := Left.Real - Right.Real;
  Result.Imaginary := Left.Imaginary - Right.Imaginary;
end;

class operator TComplex.Subtract(const Left: TComplex; const Right: Double): TComplex;
begin
  Result.Real := Left.Real - Right;
  Result.Imaginary := Left.Imaginary;
end;

class operator TComplex.Subtract(const Left: Double; const Right: TComplex): TComplex;
begin
  Result.Real := Left - Right.Real;
  Result.Imaginary := -Right.Imaginary;
end;

class operator TComplex.Multiply(const Left, Right: TComplex): TComplex;
begin
  Result.Real :=      (Left.Real * Right.Real) - (Left.Imaginary * Right.Imaginary);
  Result.Imaginary := (Left.Real * Right.Imaginary) + (Left.Imaginary * Right.Real);
end;

class operator TComplex.Multiply(const Left: TComplex; const Right: Double): TComplex;
begin
  Result.Real := Left.Real * Right;
  Result.Imaginary := Left.Imaginary * Right;
end;

class operator TComplex.Multiply(const Left: Double; const Right: TComplex): TComplex;
begin
  Result.Real := Left * Right.Real;
  Result.Imaginary := Left * Right.Imaginary;
end;

class operator TComplex.Divide(const Left, Right: TComplex): TComplex;
var
  LDenominator: Double;
begin
  LDenominator := (Right.Real * Right.Real) + (Right.Imaginary * Right.Imaginary);
  if Math.IsZero(LDenominator) then
    ZeroDivideError;
  Result.Real := ((Left.Real * Right.Real) + (Left.Imaginary * Right.Imaginary)) / LDenominator;
  Result.Imaginary := ((Left.Imaginary * Right.Real) - (Left.Real * Right.Imaginary)) / LDenominator;
end;

class operator TComplex.Divide(const Left: TComplex; const Right: Double): TComplex;
begin
  Result := Left * (1.0 / Right);
end;

class operator TComplex.Divide(const Left: Double; const Right: TComplex): TComplex;
var
  R, LDenominator: Double;
begin
  if (System.Abs(Right.Real) >= System.Abs(Right.Imaginary)) then
  begin
    if Math.IsZero(Right.Real) then
      ZeroDivideError;
    R := Right.Imaginary / Right.Real;
    LDenominator := Right.Real + R * Right.Imaginary;
    if Math.IsZero(LDenominator) then
      ZeroDivideError;
    Result.Real := Left / LDenominator;
    Result.Imaginary := -R * Result.Real;
  end else
  begin
    if Math.IsZero(Right.Imaginary) then
      ZeroDivideError;
    R := Right.Real / Right.Imaginary;
    LDenominator := Right.Imaginary + R * Right.Real;
    if Math.IsZero(LDenominator) then
      ZeroDivideError;
    Result.Imaginary := -Left / LDenominator;
    Result.Real := -R * Result.Imaginary;
  end;
end;

class operator TComplex.Negative(const AValue: TComplex): TComplex;
begin
  Result.Real := -AValue.Real;
  Result.Imaginary := -AValue.Imaginary;
end;

//------------------------------------------------------------------------------

class operator TComplex.Implicit(const AValue: Double): TComplex;
begin
  Result.Real := AValue;
end;

class operator TComplex.Implicit(const AValue: Integer): TComplex;
begin
  Result.Real := AValue;
end;

class operator TComplex.Implicit(const AValue: Int64): TComplex;
begin
  Result.Real := AValue;
end;

class operator TComplex.Implicit(const AValue: Variant): TComplex;
begin
  Result.Real := Double(AValue);
end;

class operator TComplex.Implicit(const AValue: string): TComplex;
begin
  Result := TComplex.Parse(AValue);
end;

//------------------------------------------------------------------------------

class operator TComplex.Explicit(const AValue: TComplex): Double;
begin
  AssertImaginaryIsZero(AValue);
  Result := AValue.Real;
end;

class operator TComplex.Explicit(const AValue: TComplex): Integer;
begin
  AssertImaginaryIsZero(AValue);
  Result := Round(AValue.Real);
end;

class operator TComplex.Explicit(const AValue: TComplex): Int64;
begin
  AssertImaginaryIsZero(AValue);
  Result := Round(AValue.Real);
end;

//------------------------------------------------------------------------------

end.

