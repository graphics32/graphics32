unit GR32_Math;

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
 * The Initial Developer of the Original Code is
 * Mattias Andersson <mattias@centaurix.com>
 * (parts of this unit were moved here from GR32_System.pas and GR32.pas by Alex A. Denisov)
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *  Michael Hansen <dyster_tid@hotmail.com>
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  GR32,
{$IFDEF FPC}
  GR32_Math_FPC,
{$ENDIF}
  GR32_Bindings;

//------------------------------------------------------------------------------
//
//      Fixed point math routines
//
//------------------------------------------------------------------------------
function FixedFloor(A: TFixed): Integer;
function FixedCeil(A: TFixed): Integer;
function FixedMul(A, B: TFixed): TFixed;
function FixedDiv(A, B: TFixed): TFixed;
function OneOver(Value: TFixed): TFixed;
function FixedRound(A: TFixed): Integer; {$IFDEF PUREPASCAL} inline; {$ENDIF}
function FixedSqr(Value: TFixed): TFixed;
function FixedSqrtLP(Value: TFixed): TFixed;      // 8-bit precision
function FixedSqrtHP(Value: TFixed): TFixed;      // 16-bit precision
// Fixed point interpolation
function FixedCombine(W, X, Y: TFixed): TFixed;


//------------------------------------------------------------------------------
//
//      Trigonometric routines
//
//------------------------------------------------------------------------------
procedure SinCos(const Theta: TFloat; out Sin, Cos: TFloat); overload;
procedure SinCos(const Theta, Radius: Single; out Sin, Cos: Single); overload;
procedure SinCos(const Theta, ScaleX, ScaleY: TFloat; out Sin, Cos: Single); overload;
function Hypot(const X, Y: TFloat): TFloat; overload;
function Hypot(const X, Y: Integer): Integer; overload;
// Fast*: Fast approximations
function FastSqrt(const Value: TFloat): TFloat; {$IFDEF PUREPASCAL} inline; {$ENDIF}
function FastSqrtBab1(const Value: TFloat): TFloat;
function FastSqrtBab2(const Value: TFloat): TFloat;
function FastInvSqrt(const Value: TFloat): TFloat; {$IFDEF PUREPASCAL} inline; {$ENDIF}


//------------------------------------------------------------------------------
//
//      Misc. Routines
//
//------------------------------------------------------------------------------
{ MulDiv a faster implementation of Windows.MulDiv funtion }
// The MSDN documentation for MulDiv states:
// [...] the return value is the result of the multiplication and division, rounded
// to the nearest integer. If the result is a positive half integer (ends in .5),
// it is rounded up. If the result is a negative half integer, it is rounded down.
function MulDiv(Multiplicand, Multiplier, Divisor: Integer): Integer;

function DivMod(Dividend, Divisor: Integer; var Remainder: Integer): Integer;

// Power of 2 functions. Only valid for values >= 0.
// Determine if X is a power of 2, returns true when X = 1,2,4,8,16 etc.
function IsPowerOf2(Value: Integer): Boolean; {$IFDEF USEINLINING} inline; {$ENDIF}
// Returns X rounded DOWN to the PREVIOUS power of two, i.e. 5->4, 7->4, 8->4, 9->8
function PrevPowerOf2(Value: Integer): Integer;
// Returns X rounded UP to the NEXT power of two, i.e. 5->8, 7->8, 8->16, 15->16
function NextPowerOf2(Value: Integer): Integer;

// fast average without overflow, useful for e.g. fixed point math
function Average(A, B: Integer): Integer; {$IFDEF PUREPASCAL} inline; {$ENDIF}
// fast sign function
function Sign(Value: Integer): Integer; {$IFDEF PUREPASCAL} inline; {$ENDIF}


//------------------------------------------------------------------------------
//
//      Modulus
//
//------------------------------------------------------------------------------
// See also: https://en.wikipedia.org/wiki/Modulo
//------------------------------------------------------------------------------
//
// FMod(Numerator, Denominator)
//
// Similar to Mod() but for floating point values.
// Returns a value in the [0..Denominator) range. I.e. Denominator is exclusive.
// NAN is not checked. If Denominator=0, An exception is raised or INF or NAN is
// returned depending on the implementation
//
// Equivalent to the Delphi RTL Math.FMod function.
//
//   Result := Numerator - Denominator * Trunc(Numerator / Denominator);
//
function FMod(ANumerator, ADenominator: Double): Double; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function FMod(ANumerator, ADenominator: TFloat): TFloat; overload; {$IFDEF USEINLINING} inline; {$ENDIF}

//
// FloatMod(Numerator, Denominator)
//
// Returns a value in the [0..Denominator) range. I.e. Denominator is exclusive.
// NAN is not checked. If Denominator=0, Numerator is returned.
//
// Note that, unlike FMod, FloatMod uses the Floor() definition of modulus:
//
//   Result := Numerator - Denominator * Floor(Numerator / Denominator);
//
// While FMod uses the Trunc definition:
//
//   Result := Numerator - Denominator * Trunc(Numerator / Denominator);
//
// For an implementation using the Trunc() definition, see the
// FloatRemainder function.
//
function FloatMod(ANumerator, ADenominator: Double): Double; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function FloatMod(ANumerator, ADenominator: TFloat): TFloat; overload; {$IFDEF USEINLINING} inline; {$ENDIF}

//
// FloatRemainder(Numerator, Denominator)
//
// Returns a value in the [0..Denominator) range. I.e. Denominator is exclusive.
// NAN is not checked. If Denominator=0, Numerator is returned.
//
// Similar to the FloatMod function but uses Round() instead of Floor():
//
//   Result := Numerator - Denominator * Round(Numerator / Denominator);
//
// This corresponds to the C++ remainder() function.
//
function FloatRemainder(ANumerator, ADenominator: Double): Double; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function FloatRemainder(ANumerator, ADenominator: TFloat): TFloat; overload; {$IFDEF USEINLINING} inline; {$ENDIF}


//------------------------------------------------------------------------------
//
//      Prefix Sum
//
//------------------------------------------------------------------------------
// Also known as: CumSum, Cumulative Sum
//------------------------------------------------------------------------------
type
  TCumSumProc = procedure(Values: PSingleArray; Count: Integer);

var
  CumSum: TCumSumProc;


//------------------------------------------------------------------------------
//
//      Bindings
//
//------------------------------------------------------------------------------
type
  TFloatMod_FProc = function(ANumerator, ADenominator: TFloat): TFloat;
  TFloatMod_DProc = function(ANumerator, ADenominator: Double): Double;

var
  FloatMod_F: TFloatMod_FProc; // Single
  FloatMod_D: TFloatMod_DProc; // Double
  FloatRemainder_F: TFloatMod_FProc; // Single
  FloatRemainder_D: TFloatMod_DProc; // Double
  FMod_F: TFloatMod_FProc; // Single
  FMod_D: TFloatMod_DProc; // Double

var
  MathRegistry: TFunctionRegistry;

const
  FID_CUMSUM            = 0;
  FID_FLOATMOD_F        = 1;
  FID_FLOATMOD_D        = 2;
  FID_FLOATREMAINDER_F  = 3;
  FID_FLOATREMAINDER_D  = 4;
  FID_FMOD_F            = 5;
  FID_FMOD_D            = 6;

const
  MathBindingFlagPascal = $0001;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  Math,
  GR32_System;

{$IFNDEF PUREPASCAL}
const
  // Rounding control values for use with the SSE4.1 ROUNDSS instruction
  ROUND_TO_NEAREST_INT  = $00; // Round
  ROUND_TO_NEG_INF      = $01; // Floor
  ROUND_TO_POS_INF      = $02; // Ceil
  ROUND_TO_ZERO         = $03; // Trunc
  ROUND_CUR_DIRECTION   = $04; // Rounds using default from MXCSR register

  ROUND_RAISE_EXC       = $00; // Raise exceptions
  ROUND_NO_EXC          = $08; // Suppress exceptions
{$ENDIF}

{$IFDEF PUREPASCAL}
const
  FixedOneS: Single = 65536;
{$ENDIF}


//------------------------------------------------------------------------------
//
//      Fixed-point math
//
//------------------------------------------------------------------------------
// FixedFloor
//------------------------------------------------------------------------------
{$IFDEF PUREPASCAL}

function FixedFloor(A: TFixed): Integer;
begin
  Result := A div FixedOne;
end;

{$ELSE}

function FixedFloor(A: TFixed): Integer; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$if defined(TARGET_x86)}

        SAR     EAX, 16

{$elseif defined(TARGET_x64)}

        MOV     EAX, ECX
        SAR     EAX, 16

{$else}
{$error 'Missing target'}
{$ifend}
end;

{$ENDIF}


//------------------------------------------------------------------------------
// FixedCeil
//------------------------------------------------------------------------------
{$IFDEF PUREPASCAL}

function FixedCeil(A: TFixed): Integer;
begin
  Result := (A + $FFFF) div FixedOne;
end;

{$ELSE}

function FixedCeil(A: TFixed): Integer; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$if defined(TARGET_x86)}

        ADD     EAX, $0000FFFF
        SAR     EAX, 16

{$elseif defined(TARGET_x64)}

        MOV     EAX, ECX
        ADD     EAX, $0000FFFF
        SAR     EAX, 16

{$else}
{$error 'Missing target'}
{$ifend}
end;

{$ENDIF}


//------------------------------------------------------------------------------
// FixedRound
//------------------------------------------------------------------------------
{$IFDEF PUREPASCAL}

function FixedRound(A: TFixed): Integer;
begin
  Result := (A + $7FFF);

  Result := (Cardinal(Result) shr 16) or (($10000000 - (Cardinal((Result and a) shr 31))) shl 16); // [*]

  { [*] Above line is just a branchless version of:
  if Integer(Result and A) < 0 then
    Result := (Result shr 16) or $FFFF0000
  else
    Result := (Result shr 16);
  }
end;

{$ELSE}

function FixedRound(A: TFixed): Integer; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$if defined(TARGET_x86)}

        ADD     EAX, FixedHalf
        SAR     EAX, 16

{$elseif defined(TARGET_x64)}

        MOV     EAX, ECX
        ADD     EAX, FixedHalf
        SAR     EAX, 16

{$else}
{$error 'Missing target'}
{$ifend}
end;

{$ENDIF}


//------------------------------------------------------------------------------
// FixedMul
//------------------------------------------------------------------------------
{$IFDEF PUREPASCAL}

function FixedMul(A, B: TFixed): TFixed;
begin
  Result := Round(A * FixedToFloat * B);
end;

{$ELSE}

function FixedMul(A, B: TFixed): TFixed; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$if defined(TARGET_x86)}

        IMUL    EDX
        SHRD    EAX, EDX, 16

{$elseif defined(TARGET_x64)}

        MOV     EAX, ECX
        IMUL    EDX
        SHRD    EAX, EDX, 16

{$else}
{$error 'Missing target'}
{$ifend}
end;

{$ENDIF}


//------------------------------------------------------------------------------
// FixedDiv
//------------------------------------------------------------------------------
{$IFDEF PUREPASCAL}

function FixedDiv(A, B: TFixed): TFixed;
begin
  Result := Round(A / B * FixedOne);
end;

{$ELSE}

function FixedDiv(A, B: TFixed): TFixed; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$if defined(TARGET_x86)}

        MOV     ECX, B
        CDQ
        SHLD    EDX, EAX, 16
        SHL     EAX, 16
        IDIV    ECX

{$elseif defined(TARGET_x64)}

        MOV     EAX, ECX
        MOV     ECX, EDX
        CDQ
        SHLD    EDX, EAX, 16
        SHL     EAX, 16
        IDIV    ECX

{$else}
{$error 'Missing target'}
{$ifend}
end;

{$ENDIF}


//------------------------------------------------------------------------------
// OneOver
//------------------------------------------------------------------------------
{$IFDEF PUREPASCAL}

function OneOver(Value: TFixed): TFixed;
const
  Dividend: Single = 4294967296; // FixedOne * FixedOne
begin
  Result := Round(Dividend / Value);
end;

{$ELSE}

function OneOver(Value: TFixed): TFixed; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$if defined(TARGET_x86)}

        MOV     ECX, Value
        XOR     EAX, EAX
        MOV     EDX, 1
        IDIV    ECX

{$elseif defined(TARGET_x64)}

        XOR     EAX, EAX
        MOV     EDX, 1
        IDIV    ECX

{$else}
{$error 'Missing target'}
{$ifend}
end;

{$ENDIF}


//------------------------------------------------------------------------------
// FixedSqr
//------------------------------------------------------------------------------
{$IFDEF PUREPASCAL}

function FixedSqr(Value: TFixed): TFixed;
begin
  Result := Round(Value * FixedToFloat * Value);
end;

{$ELSE}

function FixedSqr(Value: TFixed): TFixed; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$if defined(TARGET_x86)}

        IMUL    EAX
        SHRD    EAX, EDX, 16

{$elseif defined(TARGET_x64)}

        MOV     EAX, Value
        IMUL    EAX
        SHRD    EAX, EDX, 16

{$else}
{$error 'Missing target'}
{$ifend}
end;

{$ENDIF}


//------------------------------------------------------------------------------
// FixedSqrt
//------------------------------------------------------------------------------
{$IFDEF PUREPASCAL}

function FixedSqrtLP(Value: TFixed): TFixed;
begin
  Result := Round(Sqrt(Value * FixedOneS));
end;

{$ELSE}

function FixedSqrtLP(Value: TFixed): TFixed; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$if defined(TARGET_x86)}

        PUSH    EBX
        MOV     ECX, EAX
        XOR     EAX, EAX
        MOV     EBX, $40000000
@SqrtLP1:
        MOV     EDX, ECX
        SUB     EDX, EBX
        JL      @SqrtLP2
        SUB     EDX, EAX
        JL      @SqrtLP2
        MOV     ECX, EDX
        SHR     EAX, 1
        OR      EAX, EBX
        SHR     EBX, 2
        JNZ     @SqrtLP1
        SHL     EAX, 8
        JMP     @SqrtLP3
@SqrtLP2:
        SHR     EAX, 1
        SHR     EBX, 2
        JNZ     @SqrtLP1
        SHL     EAX, 8
@SqrtLP3:
        POP     EBX

{$elseif defined(TARGET_x64)}

        XOR     EAX, EAX
        MOV     R8D, $40000000
@SqrtLP1:
        MOV     EDX, ECX
        SUB     EDX, R8D
        JL      @SqrtLP2
        SUB     EDX, EAX
        JL      @SqrtLP2
        MOV     ECX, EDX
        SHR     EAX, 1
        OR      EAX, R8D
        SHR     R8D, 2
        JNZ     @SqrtLP1
        SHL     EAX, 8
        RET
@SqrtLP2:
        SHR     EAX, 1
        SHR     R8D, 2
        JNZ     @SqrtLP1
        SHL     EAX, 8

{$else}
{$error 'Missing target'}
{$ifend}
end;

{$ENDIF}

//------------------------------------------------------------------------------

{$IFDEF PUREPASCAL}

function FixedSqrtHP(Value: TFixed): TFixed;
begin
  Result := Round(Sqrt(Value * FixedOneS));
end;

{$ELSE}

function FixedSqrtHP(Value: TFixed): TFixed; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$if defined(TARGET_x86)}

        PUSH    EBX
        MOV     ECX, EAX
        XOR     EAX, EAX
        MOV     EBX, $40000000
@SqrtHP1:
        MOV     EDX, ECX
        SUB     EDX, EBX
        JB      @SqrtHP2
        SUB     EDX, EAX
        JB      @SqrtHP2
        MOV     ECX, EDX
        SHR     EAX, 1
        OR      EAX, EBX
        SHR     EBX, 2
        JNZ     @SqrtHP1
        JMP     @SqrtHP5
@SqrtHP2:
        SHR     EAX, 1
        SHR     EBX, 2
        JNZ     @SqrtHP1
@SqrtHP5:
        MOV     EBX, $00004000
        SHL     EAX, 16
        SHL     ECX, 16
@SqrtHP3:
        MOV     EDX, ECX
        SUB     EDX, EBX
        JB      @SqrtHP4
        SUB     EDX, EAX
        JB      @SqrtHP4
        MOV     ECX, EDX
        SHR     EAX, 1
        OR      EAX, EBX
        SHR     EBX, 2
        JNZ     @SqrtHP3
        JMP     @SqrtHP6
@SqrtHP4:
        SHR     EAX, 1
        SHR     EBX, 2
        JNZ     @SqrtHP3
@SqrtHP6:
        POP     EBX

{$elseif defined(TARGET_x64)}

        XOR     EAX, EAX
        MOV     R8D, $40000000
@SqrtHP1:
        MOV     EDX, ECX
        SUB     EDX, R8D
        JB      @SqrtHP2
        SUB     EDX, EAX
        JB      @SqrtHP2
        MOV     ECX, EDX
        SHR     EAX, 1
        OR      EAX, R8D
        SHR     R8D, 2
        JNZ     @SqrtHP1
        JMP     @SqrtHP5
@SqrtHP2:
        SHR     EAX, 1
        SHR     R8D, 2
        JNZ     @SqrtHP1
@SqrtHP5:
        MOV     R8D, $00004000
        SHL     EAX, 16
        SHL     ECX, 16
@SqrtHP3:
        MOV     EDX, ECX
        SUB     EDX, R8D
        JB      @SqrtHP4
        SUB     EDX, EAX
        JB      @SqrtHP4
        MOV     ECX, EDX
        SHR     EAX, 1
        OR      EAX, R8D
        SHR     R8D, 2
        JNZ     @SqrtHP3
        RET
@SqrtHP4:
        SHR     EAX, 1
        SHR     R8D, 2
        JNZ     @SqrtHP3

{$else}
{$error 'Missing target'}
{$ifend}
end;

{$ENDIF}


//------------------------------------------------------------------------------
// FixedCombine
//------------------------------------------------------------------------------
// combine fixed value X and fixed value Y with the weight of X given in W
// Result Z = W * X + (1 - W) * Y = Y + (X - Y) * W
// Fixed Point Version: Result Z = Y + (X - Y) * W / 65536
//------------------------------------------------------------------------------
{$IFDEF PUREPASCAL}

function FixedCombine(W, X, Y: TFixed): TFixed;
begin
  Result := Round(Y + (X - Y) * FixedToFloat * W);
end;

{$ELSE}

function FixedCombine(W, X, Y: TFixed): TFixed; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
// EAX <- W, EDX <- X, ECX <- Y
asm
{$if defined(TARGET_x86)}

        SUB     EDX, ECX
        IMUL    EDX
        SHRD    EAX, EDX, 16
        ADD     EAX, ECX

{$elseif defined(TARGET_x64)}

        MOV     EAX, ECX
        SUB     EDX, R8D
        IMUL    EDX
        SHRD    EAX, EDX, 16
        ADD     EAX, R8D

{$else}
{$error 'Missing target'}
{$ifend}

end;
{$ENDIF}


//------------------------------------------------------------------------------
//
//      Trigonometry
//
//------------------------------------------------------------------------------
// SinCos
//------------------------------------------------------------------------------
{$if defined(PUREPASCAL) or defined(NATIVE_SINCOS)}

procedure SinCos(const Theta: TFloat; out Sin, Cos: TFloat);
var
  S, C: Extended;
begin
  Math.SinCos(Theta, S, C);
  Sin := S;
  Cos := C;
end;

{$else}

procedure SinCos(const Theta: TFloat; out Sin, Cos: TFloat); {$IFDEF FPC} assembler; {$ENDIF}
{$if defined(TARGET_x86)}

asm
        FLD     Theta
        FSINCOS
        FSTP    DWORD PTR [EDX] // cosine
        FSTP    DWORD PTR [EAX] // sine

{$elseif defined(TARGET_x64)}

var
  Temp: TFloat;
asm
        MOVD    Temp, Theta
        FLD     Temp
        FSINCOS
        FSTP    [Sin] // cosine
        FSTP    [Cos] // sine

{$else}
{$error 'Missing target'}
{$ifend}
end;

{$ifend}


//------------------------------------------------------------------------------

{$if defined(PUREPASCAL) or defined(NATIVE_SINCOS)}

procedure SinCos(const Theta, Radius: TFloat; out Sin, Cos: TFloat);
var
  S, C: Extended;
begin
  Math.SinCos(Theta, S, C);
  Sin := S * Radius;
  Cos := C * Radius;
end;

{$else}

procedure SinCos(const Theta, Radius: TFloat; out Sin, Cos: TFloat); {$IFDEF FPC} assembler; {$ENDIF}
{$if defined(TARGET_x86)}

asm
        FLD     Theta
        FSINCOS
        FMUL    Radius
        FSTP    DWORD PTR [EDX] // cosine
        FMUL    Radius
        FSTP    DWORD PTR [EAX] // sine

{$elseif defined(TARGET_x64)}

var
  Temp: TFloat;
asm
        MOVD    Temp, Theta
        FLD     Temp
        MOVD    Temp, Radius
        FSINCOS
        FMUL    Temp
        FSTP    [Cos]
        FMUL    Temp
        FSTP    [Sin]

{$else}
{$error 'Missing target'}
{$ifend}
end;

{$ifend}

//------------------------------------------------------------------------------

{$if defined(PUREPASCAL) or defined(NATIVE_SINCOS)}

procedure SinCos(const Theta, ScaleX, ScaleY: TFloat; out Sin, Cos: Single);
var
  S, C: Extended;
begin
  Math.SinCos(Theta, S, C);
  Sin := S * ScaleX;
  Cos := C * ScaleY;
end;

{$else}

procedure SinCos(const Theta, ScaleX, ScaleY: TFloat; out Sin, Cos: Single);  {$IFDEF FPC} assembler; {$ENDIF}
{$if defined(TARGET_x86)}

asm
        FLD     Theta
        FSINCOS
        FMUL    ScaleX
        FSTP    DWORD PTR [EDX] // cosine
        FMUL    ScaleY
        FSTP    DWORD PTR [EAX] // sine

{$elseif defined(TARGET_x64)}

var
  Temp: TFloat;
asm
        MOVD    Temp, Theta
        FLD     Temp
        FSINCOS
        MOVD    Temp, ScaleX
        FMUL    Temp
        FSTP    [Cos]
        MOVD    Temp, ScaleY
        FMUL    Temp
        FSTP    [Sin]

{$else}
{$error 'Missing target'}
{$ifend}
end;

{$ifend}


//------------------------------------------------------------------------------
// Hypot
//------------------------------------------------------------------------------
{$IFDEF PUREPASCAL}

function Hypot(const X, Y: TFloat): TFloat;
begin
  Result := Sqrt(Sqr(X) + Sqr(Y));
end;

{$ELSE}

function Hypot(const X, Y: TFloat): TFloat; {$IFDEF FPC} assembler; {$IFDEF TARGET_X64} nostackframe; {$ENDIF}{$ENDIF}
asm
{$if defined(TARGET_x86)}

        FLD     X
        FMUL    ST,ST
        FLD     Y
        FMUL    ST,ST
        FADDP   ST(1),ST
        FSQRT
        FWAIT

{$elseif defined(TARGET_x64)}

        MULSS   XMM0, XMM0
        MULSS   XMM1, XMM1
        ADDSS   XMM0, XMM1
        SQRTSS  XMM0, XMM0

{$else}
{$error 'Missing target'}
{$ifend}
end;

{$ENDIF}

//------------------------------------------------------------------------------

{$if defined(PUREPASCAL) or (True)}

function Hypot(const X, Y: Integer): Integer;
begin
  Result := Round(Math.Hypot(X, Y));
end;

{$else}

// TODO : Disabled for some reason. Document why!
function Hypot(const X, Y: Integer): Integer; {$IFDEF FPC} assembler; {$IFDEF TARGET_X64}nostackframe;{$ENDIF} {$ENDIF}
asm
{$if defined(TARGET_x86)}

        IMUL    RAX, RCX, RDX

{$elseif defined(TARGET_x64)}

        FLD     X
        FMUL    ST,ST
        FLD     Y
        FMUL    ST,ST
        FADDP   ST(1),ST
        FSQRT
        FISTP   [ESP - 4]
        MOV     EAX, [ESP - 4]
        FWAIT
{$else}
{$error 'Missing target'}
{$ifend}
end;

{$ifend}


//------------------------------------------------------------------------------
//
//      Fast approximations
//
//------------------------------------------------------------------------------
// FastSqrt
//------------------------------------------------------------------------------
{$IFDEF PUREPASCAL}

function FastSqrt(const Value: TFloat): TFloat;
var
  I: Integer absolute Value;
  J: Integer absolute Result;
begin
  J := (I - $3F800000) div 2 + $3F800000;
end;

{$ELSE}

function FastSqrt(const Value: TFloat): TFloat; {$IFDEF FPC} assembler; {$IFDEF TARGET_X64}nostackframe;{$ENDIF} {$ENDIF}
asm
{$if defined(TARGET_x86)}
        //
        // Sqrt(x) = x * InvSqrt(x)
        //
        // RSQRT is accurate only to ~11 bits.
        // Note: RSQRT(0) = INF, INF*0 = NAN !
        //

        MOV     ECX, [Value]
        MOVD    XMM0, ECX

        RSQRTSS XMM1, XMM0
        MULSS   XMM1, XMM0

        UCOMISS XMM1, XMM1      // when XMM1=NAN then XMM1<>XMM1
        MOVD    EAX, XMM1
        CMOVP   EAX, ECX        // Result := Value (which we assume is zero) if Result was NAN

        MOV     [Result], EAX


(* Fast, but pretty bad, approximations:
   see http://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Approximations_that_depend_on_IEEE_representation

        MOV     EAX, DWORD PTR Value

{ As outlined in the wikipedia article:
        SUB     EAX, $00800000
        SAR     EAX, 1
        ADD     EAX, $20000000
}
{ Previous GR32 implementation:
        SUB     EAX, $3F800000
        SAR     EAX, 1
        ADD     EAX, $3F800000
}
        MOV     DWORD PTR [ESP - 4], EAX
        FLD     DWORD PTR [ESP - 4]
*)

{$elseif defined(TARGET_x64)}

        SQRTSS  XMM0, XMM0

{$else}
{$error 'Missing target'}
{$ifend}
end;

{$ENDIF}


//------------------------------------------------------------------------------
// FastSqrtBab1
//------------------------------------------------------------------------------
// See http://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Approximations_that_depend_on_IEEE_representation
// Additionally one babylonian step added
//------------------------------------------------------------------------------
{$IFDEF PUREPASCAL}

function FastSqrtBab1(const Value: TFloat): TFloat;
const
  CHalf : TFloat = 0.5;
var
  I: Integer absolute Value;
  J: Integer absolute Result;
begin
  J := (I - $3F800000) div 2 + $3F800000;
  Result := CHalf * (Result + Value / Result);
end;

{$ELSE}

function FastSqrtBab1(const Value: TFloat): TFloat; {$IFDEF FPC} assembler; {$IFDEF TARGET_X64}nostackframe;{$ENDIF} {$ENDIF}
const
  CHalf : TFloat = 0.5;
asm
{$if defined(TARGET_x86)}

        MOV     EAX, Value
        SUB     EAX, $3F800000
        SAR     EAX, 1
        ADD     EAX, $3F800000
        MOV     DWORD PTR [ESP - 4], EAX
        FLD     Value
        FDIV    DWORD PTR [ESP - 4]
        FADD    DWORD PTR [ESP - 4]
        FMUL    CHalf

{$elseif defined(TARGET_x64)}

        SQRTSS  XMM0, XMM0

{$else}
{$error 'Missing target'}
{$ifend}
end;

{$ENDIF}


//------------------------------------------------------------------------------
// FastSqrtBab2
//------------------------------------------------------------------------------
// See http://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Approximations_that_depend_on_IEEE_representation
// Additionally two babylonian steps added
//------------------------------------------------------------------------------
{$IFDEF PUREPASCAL}

function FastSqrtBab2(const Value: TFloat): TFloat;
const
  CQuarter : TFloat = 0.25;
var
  J: Integer absolute Result;
begin
 Result := Value;
 J := ((J - (1 shl 23)) shr 1) + (1 shl 29);
 Result := Result + Value / Result;
 Result := CQuarter * Result + Value / Result;
end;

{$ELSE}

function FastSqrtBab2(const Value: TFloat): TFloat; {$IFDEF FPC} assembler; {$IFDEF TARGET_X64}nostackframe;{$ENDIF} {$ENDIF}
const
  CHalf : TFloat = 0.5;
asm
{$if defined(TARGET_x86)}

        MOV     EAX, Value
        SUB     EAX, $3F800000
        SAR     EAX, 1
        ADD     EAX, $3F800000
        MOV     DWORD PTR [ESP - 4], EAX
        FLD     Value
        FDIV    DWORD PTR [ESP - 4]
        FADD    DWORD PTR [ESP - 4]
        FMUL    CHalf

{$elseif defined(TARGET_x64)}

        MOVD    EAX, Value
        SUB     EAX, $3F800000
        SAR     EAX, 1
        ADD     EAX, $3F800000
        MOVD    XMM1, EAX
        DIVSS   XMM0, XMM1
        ADDSS   XMM0, XMM1
        MOVD    XMM1, [RIP + CHalf]
        MULSS   XMM0, XMM1

{$else}
{$error 'Missing target'}
{$ifend}
end;

{$ENDIF}


//------------------------------------------------------------------------------
// FastInvSqrt
//------------------------------------------------------------------------------
{$IFDEF PUREPASCAL}

function FastInvSqrt(const Value: TFloat): TFloat;
var
  IntCst : Cardinal absolute result;
begin
  Result := Value;
  IntCst := ($BE6EB50C - IntCst) shr 1;
  Result := 0.5 * Result * (3 - Value * Sqr(Result));
end;

{$ELSE}

function FastInvSqrt(const Value: TFloat): TFloat; {$IFDEF FPC} assembler; {$IFDEF TARGET_X64}nostackframe;{$ENDIF}{$ENDIF}
//
// Note: RSQRT is accurate only to ~11 bits.
//
asm
{$if defined(TARGET_x86)}

        MOVSS   XMM0, [Value]
        RSQRTSS XMM0, XMM0
        MOVSS   [Result], XMM0

{$elseif defined(TARGET_x64)}

        RSQRTSS XMM0, XMM0

{$else}
{$error 'Missing target'}
{$ifend}
end;

{$ENDIF}


//------------------------------------------------------------------------------
//
//      Misc. Routines
//
//------------------------------------------------------------------------------
//
//      MulDiv
//
//------------------------------------------------------------------------------
{$IFDEF PUREPASCAL}

function MulDiv(Multiplicand, Multiplier, Divisor: Integer): Integer;
begin
  Result := (Int64(Multiplicand) * Int64(Multiplier) + Divisor div 2) div Divisor;
end;

{$ELSE}

function MulDiv(Multiplicand, Multiplier, Divisor: Integer): Integer; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$if defined(TARGET_x86)}

        PUSH    EBX             // Imperative save
        PUSH    ESI             // of EBX and ESI

        MOV     EBX, EAX        // Result will be negative or positive so set rounding direction
        XOR     EBX, EDX        //  Negative: substract 1 in case of rounding
        XOR     EBX, ECX        //  Positive: add 1

        OR      EAX, EAX        // Make all operands positive, ready for unsigned operations
        JNS     @m1Ok           // minimizing branching
        NEG     EAX
@m1Ok:
        OR      EDX, EDX
        JNS     @m2Ok
        NEG     EDX
@m2Ok:
        OR      ECX, ECX
        JNS     @DivOk
        NEG     ECX
@DivOK:
        MUL     EDX             // Unsigned multiply (Multiplicand*Multiplier)

        MOV     ESI, EDX        // Check for overflow, by comparing
        SHL     ESI, 1          // 2 times the high-order 32 bits of the product (EDX)
        CMP     ESI, ECX        // with the Divisor.
        JAE     @Overfl         // If equal or greater than overflow with division anticipated

        DIV     ECX             // Unsigned divide of product by Divisor

        SUB     ECX, EDX        // Check if the result must be adjusted by adding or substracting
        CMP     ECX, EDX        // 1 (*.5 -> nearest integer), by comparing the difference of
        JA      @NoAdd          // Divisor and remainder with the remainder. If it is greater then
        INC     EAX             // no rounding needed; add 1 to result otherwise
@NoAdd:
        OR      EBX, EDX        // From unsigned operations back the to original sign of the result
        JNS     @Exit           // must be positive
        NEG     EAX             // must be negative
        JMP     @Exit
@Overfl:
        OR      EAX, -1         //  3 bytes alternative for MOV EAX,-1. Windows.MulDiv "overflow"
                                //  and "zero-divide" return value
@Exit:
        POP     ESI             // Restore
        POP     EBX             // esi and EBX

{$elseif defined(TARGET_x64)}

        MOV     EAX, ECX        // Result will be negative or positive so set rounding direction
        XOR     ECX, EDX        //  Negative: substract 1 in case of rounding
        XOR     ECX, R8D        //  Positive: add 1

        OR      EAX, EAX        // Make all operands positive, ready for unsigned operations
        JNS     @m1Ok           // minimizing branching
        NEG     EAX
@m1Ok:
        OR      EDX, EDX
        JNS     @m2Ok
        NEG     EDX
@m2Ok:
        OR      R8D, R8D
        JNS     @DivOk
        NEG     R8D
@DivOK:
        MUL     EDX             // Unsigned multiply (Multiplicand*Multiplier)

        MOV     R9D, EDX        // Check for overflow, by comparing
        SHL     R9D, 1          // 2 times the high-order 32 bits of the product (EDX)
        CMP     R9D, R8D        // with the Divisor.
        JAE     @Overfl         // If equal or greater than overflow with division anticipated

        DIV     R8D             // Unsigned divide of product by Divisor

        SUB     R8D, EDX        // Check if the result must be adjusted by adding or substracting
        CMP     R8D, EDX        // 1 (*.5 -> nearest integer), by comparing the difference of
        JA      @NoAdd          // Divisor and remainder with the remainder. If it is greater then
        INC     EAX             // no rounding needed; add 1 to result otherwise
@NoAdd:
        OR      ECX, EDX        // From unsigned operations back the to original sign of the result
        JNS     @Exit           // must be positive
        NEG     EAX             // must be negative
        JMP     @Exit
@Overfl:
        OR      EAX, -1         //  3 bytes alternative for MOV EAX,-1. Windows.MulDiv "overflow"
                                //  and "zero-divide" return value
@Exit:

{$else}
{$error 'Missing target'}
{$ifend}
end;

{$ENDIF}


//------------------------------------------------------------------------------
//
//      IsPowerOf2
//
//------------------------------------------------------------------------------
// Returns true when X = 1,2,4,8,16 etc.
//------------------------------------------------------------------------------
function IsPowerOf2(Value: Integer): Boolean;
begin
  Result := (Value <> 0) and (Cardinal(Value) and (Cardinal(Value) - 1) = 0);
end;


//------------------------------------------------------------------------------
//
//      PrevPowerOf2
//
//------------------------------------------------------------------------------
// Returns X rounded down to the power of two
//------------------------------------------------------------------------------
{$IFDEF PUREPASCAL}

function PrevPowerOf2(Value: Integer): Integer;
begin
  Result := Value;
  Result := Result or (Result shr 1);
  Result := Result or (Result shr 2);
  Result := Result or (Result shr 4);
  Result := Result or (Result shr 8);
  Result := Result or (Result shr 16);
  Dec(Result, Result shr 1);
end;

{$ELSE}

function PrevPowerOf2(Value: Integer): Integer; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$if defined(TARGET_x86)}

        BSR     ECX, EAX
        SHR     EAX, CL
        SHL     EAX, CL

{$elseif defined(TARGET_x64)}

        MOV     EAX, Value
        BSR     ECX, EAX
        SHR     EAX, CL
        SHL     EAX, CL

{$else}
{$error 'Missing target'}
{$ifend}
end;

{$ENDIF}


//------------------------------------------------------------------------------
//
//      NextPowerOf2
//
//------------------------------------------------------------------------------
// Returns X rounded up to the power of two, i.e. 5 -> 8, 7 -> 8, 15 -> 16
//------------------------------------------------------------------------------
{$IFDEF PUREPASCAL}

function NextPowerOf2(Value: Integer): Integer;
begin
  if (Value = 0) then
    Exit(1);
  Result := Value-1;
  Result := Result or (Result shr 1);
  Result := Result or (Result shr 2);
  Result := Result or (Result shr 4);
  Result := Result or (Result shr 8);
  Result := Result or (Result shr 16);
  Inc(Result);
end;

{$ELSE}

function NextPowerOf2(Value: Integer): Integer; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$if defined(TARGET_x86)}

        DEC     EAX
        JLE     @1
        BSR     ECX, EAX
        MOV     EAX, 2
        SHL     EAX, CL
        RET
@1:
        MOV     EAX, 1

{$elseif defined(TARGET_x64)}

        MOV     EAX, Value
        DEC     EAX
        JLE     @1
        BSR     ECX, EAX
        MOV     EAX, 2
        SHL     EAX, CL
        RET
@1:
        MOV     EAX, 1

{$else}
{$error 'Missing target'}
{$ifend}
end;

{$ENDIF}


//------------------------------------------------------------------------------
//
//      Average
//
//------------------------------------------------------------------------------
// Fast average without overflow, useful e.g. for fixed point math
// (A + B) / 2 = (A and B) + (A xor B) / 2
//------------------------------------------------------------------------------
{$IFDEF PUREPASCAL}

function Average(A, B: Integer): Integer;
begin
  Result := (A and B) + (A xor B) div 2;
end;

{$ELSE}

function Average(A, B: Integer): Integer; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$if defined(TARGET_x86)}

        MOV     ECX, EDX
        XOR     EDX, EAX
        SAR     EDX, 1
        AND     EAX, ECX
        ADD     EAX, EDX

{$elseif defined(TARGET_x64)}

        MOV     EAX, A
        MOV     ECX, EDX
        XOR     EDX, EAX
        SAR     EDX, 1
        AND     EAX, ECX
        ADD     EAX, EDX

{$else}
{$error 'Missing target'}
{$ifend}
end;

{$ENDIF}


//------------------------------------------------------------------------------
//
//      Sign
//
//------------------------------------------------------------------------------
{$IFDEF PUREPASCAL}

function Sign(Value: Integer): Integer;
begin
  // Defer to Math.Sign
  Result := Integer(Math.Sign(Value));
end;

{$ELSE}

function Sign(Value: Integer): Integer; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$if defined(TARGET_x86)}

        { New algorithm provides no speed saving under 32-bit, so just use this
          smaller one }
        CDQ
        NEG     EAX
        ADC     EDX, EDX
        MOV     EAX, EDX

{$elseif defined(TARGET_x64)}

  {$IFDEF MSWINDOWS}
        XOR     EDX, EDX
        TEST    ECX, ECX
        SETG    DL
        SAR     ECX, 31
        LEA     EAX, [EDX + ECX]
  {$ELSE}
        XOR     EDX, EDX
        TEST    EDI, EDI
        SETG    DL
        SAR     EDI, 31
        LEA     EAX, [EDX + EDI]
  {$ENDIF}

{$else}
{$error 'Missing target'}
{$ifend}
end;

{$ENDIF}


//------------------------------------------------------------------------------
//
//      FloatMod
//
//------------------------------------------------------------------------------
function FloatMod(ANumerator, ADenominator: Double): Double;
begin
  Result := FloatMod_D(ANumerator, ADenominator);
end;

function FloatMod(ANumerator, ADenominator: TFloat): TFloat;
begin
  Result := FloatMod_F(ANumerator, ADenominator);
end;

//------------------------------------------------------------------------------

function FloatMod_F_Pas(ANumerator, ADenominator: TFloat): TFloat;
begin
  if ((ANumerator >= 0) and (ANumerator < ADenominator)) or (ADenominator = 0) then
    Result := ANumerator
  else
    Result := ANumerator - ADenominator * Floor(ANumerator / ADenominator);
end;

function FloatMod_D_Pas(ANumerator, ADenominator: Double): Double;
begin
  if ((ANumerator >= 0) and (ANumerator < ADenominator)) or (ADenominator = 0) then
    Result := ANumerator
  else
    Result := ANumerator - ADenominator * Floor(ANumerator / ADenominator);
end;

//------------------------------------------------------------------------------

{$ifndef PUREPASCAL}

// Note: FloatMod_F_SSE41 and FloatRemainder_F_SSE41 are identical except for the ROUNDSS parameter. Keep in sync!
// Note: Float*_D_SSE41 and Float*_F_SSE41 are the exact same except the D variant uses the *d instructions and and the F
//       variant uses the *s instructions. Keep in sync!
function FloatMod_F_SSE41(ANumerator, ADenominator: TFloat): TFloat; {$IFDEF FPC} assembler; {$IFDEF TARGET_X64}nostackframe;{$ENDIF} {$ENDIF}
asm
{$if defined(TARGET_x86)}
        movss   xmm0, ANumerator
        movss   xmm1, ADenominator
{$ifend}
        xorps   xmm2, xmm2

        // if (ANumerator < 0) then...
        comiss  xmm0, xmm2
        // ...do modulus...
        jb      @@do_mod

        // if (ADenominator > ANumerator) then...
        comiss  xmm1, xmm0
        // ...Result := ANumerator
        ja     @@return_value

@@do_mod:
        // if (ADenominator = 0) then...
        ucomiss xmm1, xmm2
        lahf                            // AH <- Status flags
        test    ah, $44                 // Test(AH, ZF or PF)
        // ...Result := ANumerator
        jnp     @@return_value

        // a := ANumerator / ADenominator
        movss   xmm2, xmm0
        divss   xmm2, xmm1
        // b := Floor(a)
        roundss xmm2, xmm2, ROUND_TO_NEG_INF or ROUND_NO_EXC
        // c := ADenominator * b
        mulss   xmm2, xmm1
        // Result := ANumerator - c;
        subss   xmm0, xmm2
        // Fall through...

@@return_value:
{$if defined(TARGET_x86)}
        movss   Result, xmm0
{$elseif not defined(TARGET_x64)}
{$error 'Missing target'}
{$ifend}
end;

// Note: FloatMod_D_SSE41 and FloatRemainder_D_SSE41 are identical except for the ROUNDSD parameter. Keep in sync!
// Note: Float*_D_SSE41 and Float*_F_SSE41 are the exact same except the D variant uses the *d instructions and and the F
//       variant uses the *s instructions. Keep in sync!
function FloatMod_D_SSE41(ANumerator, ADenominator: Double): Double; {$IFDEF FPC} assembler; {$IFDEF TARGET_X64}nostackframe;{$ENDIF} {$ENDIF}
asm
{$if defined(TARGET_x86)}
        movsd   xmm0, ANumerator        // XMM0 <- ANumerator
        movsd   xmm1, ADenominator      // XMM1 <- ADenominator
{$ifend}
        xorpd   xmm2, xmm2              // XMM2 <- 0

        // if (ANumerator < 0) then...
        comisd  xmm0, xmm2
        // ...do modulus...
        jb      @@do_mod

        // if (ADenominator > ANumerator) then...
        comisd  xmm1, xmm0
        // ...Result := ANumerator
        ja     @@return_value

@@do_mod:
        // if (ADenominator = 0) then...
        ucomisd xmm1, xmm2
        lahf                            // AH <- Status flags
        test    ah, $44                 // Test(AH, ZF or PF)
        // ...Result := ANumerator
        jnp     @@return_value

        // a := ANumerator / ADenominator
        movsd   xmm2, xmm0
        divsd   xmm2, xmm1
        // b := Floor(a)
        roundsd xmm2, xmm2, ROUND_TO_NEG_INF or ROUND_NO_EXC
        // c := ADenominator * b
        mulsd   xmm2, xmm1
        // Result := ANumerator - c;
        subsd   xmm0, xmm2
        // Fall through...

@@return_value:
{$if defined(TARGET_x86)}
        movsd   Result, xmm0
{$elseif not defined(TARGET_x64)}
{$error 'Missing target'}
{$ifend}
end;

{$endif PUREPASCAL}


//------------------------------------------------------------------------------
//
//      FloatRemainder
//
//------------------------------------------------------------------------------
function FloatRemainder(ANumerator, ADenominator: Double): Double;
begin
  Result := FloatRemainder_D(ANumerator, ADenominator);
end;

function FloatRemainder(ANumerator, ADenominator: TFloat): TFloat;
begin
  Result := FloatRemainder_F(ANumerator, ADenominator);
end;

//------------------------------------------------------------------------------

function FloatRemainder_D_Pas(ANumerator, ADenominator: Double): Double;
begin
  if ((ANumerator >= 0) and (ANumerator < ADenominator)) or (ADenominator = 0) then
    Result := ANumerator
  else
    Result := ANumerator - ADenominator * Round(ANumerator / ADenominator);
end;

function FloatRemainder_F_Pas(ANumerator, ADenominator: TFloat): TFloat;
begin
  if ((ANumerator >= 0) and (ANumerator < ADenominator)) or (ADenominator = 0) then
    Result := ANumerator
  else
    Result := ANumerator - ADenominator * Round(ANumerator / ADenominator);
end;

//------------------------------------------------------------------------------

{$ifndef PUREPASCAL}

// Note: FloatMod_F_SSE41 and FloatRemainder_F_SSE41 are identical except for the ROUNDSS parameter. Keep in sync!
// Note: Float*_D_SSE41 and Float*_F_SSE41 are the exact same except the D variant uses the *d instructions and and the F
//       variant uses the *s instructions. Keep in sync!
function FloatRemainder_F_SSE41(ANumerator, ADenominator: TFloat): TFloat; {$IFDEF FPC} assembler; {$IFDEF TARGET_X64}nostackframe;{$ENDIF} {$ENDIF}
asm
{$if defined(TARGET_x86)}
        movss   xmm0, ANumerator
        movss   xmm1, ADenominator
{$ifend}
        xorps   xmm2, xmm2

        // if (ANumerator < 0) then...
        comiss  xmm0, xmm2
        // ...do modulus...
        jb      @@do_mod

        // if (ADenominator > ANumerator) then...
        comiss  xmm1, xmm0
        // ...Result := ANumerator
        ja     @@return_value

@@do_mod:
        // if (ADenominator = 0) then...
        ucomiss xmm1, xmm2
        lahf                            // AH <- Status flags
        test    ah, $44                 // Test(AH, ZF or PF)
        // ...Result := ANumerator
        jnp     @@return_value

        // a := ANumerator / ADenominator
        movss   xmm2, xmm0
        divss   xmm2, xmm1
        // b := Round(a)
        roundss xmm2, xmm2, ROUND_TO_NEAREST_INT or ROUND_NO_EXC
        // c := ADenominator * b
        mulss   xmm2, xmm1
        // Result := ANumerator - c;
        subss   xmm0, xmm2
        // Fall through...

@@return_value:
{$if defined(TARGET_x86)}
        movss   Result, xmm0
{$elseif not defined(TARGET_x64)}
{$error 'Missing target'}
{$ifend}
end;

// Note: FloatMod_D_SSE41 and FloatRemainder_D_SSE41 are identical except for the ROUNDSD parameter. Keep in sync!
// Note: Float*_D_SSE41 and Float*_F_SSE41 are the exact same except the D variant uses the *d instructions and and the F
//       variant uses the *s instructions. Keep in sync!
function FloatRemainder_D_SSE41(ANumerator, ADenominator: Double): Double; {$IFDEF FPC} assembler; {$IFDEF TARGET_X64}nostackframe;{$ENDIF} {$ENDIF}
asm
{$if defined(TARGET_x86)}
        movsd   xmm0, ANumerator        // XMM0 <- ANumerator
        movsd   xmm1, ADenominator      // XMM1 <- ADenominator
{$ifend}
        xorpd   xmm2, xmm2

        // if (ANumerator < 0) then...
        comisd  xmm0, xmm2
        // ...do modulus...
        jb      @@do_mod

        // if (ADenominator > ANumerator) then...
        comisd  xmm1, xmm0
        // ...Result := ANumerator
        ja     @@return_value

@@do_mod:
        // if (ADenominator = 0) then...
        ucomisd xmm1, xmm2
        lahf                            // AH <- Status flags
        test    ah, $44                 // Test(AH, ZF or PF)
        // ...Result := ANumerator
        jnp     @@return_value

        // a := ANumerator / ADenominator
        movsd   xmm2, xmm0
        divsd   xmm2, xmm1
        // b := Floor(a)
        roundsd xmm2, xmm2, ROUND_TO_NEAREST_INT or ROUND_NO_EXC
        // c := ADenominator * b
        mulsd   xmm2, xmm1
        // Result := ANumerator - c;
        subsd   xmm0, xmm2
        // Fall through...

@@return_value:
{$if defined(TARGET_x86)}
        movsd   Result, xmm0
{$elseif not defined(TARGET_x64)}
{$error 'Missing target'}
{$ifend}
end;

{$endif PUREPASCAL}


//------------------------------------------------------------------------------
//
//      FMod
//
//------------------------------------------------------------------------------
function FMod(ANumerator, ADenominator: Double): Double;
begin
  Result := FMod_D(ANumerator, ADenominator);
end;

function FMod(ANumerator, ADenominator: TFloat): TFloat;
begin
  Result := FMod_F(ANumerator, ADenominator);
end;

//------------------------------------------------------------------------------

function FMod_F_Pas(ANumerator, ADenominator: TFloat): TFloat;
begin
  Result := ANumerator - ADenominator * Trunc(ANumerator / ADenominator);
end;

function FMod_D_Pas(ANumerator, ADenominator: Double): Double;
begin
  Result := ANumerator - ADenominator * Trunc(ANumerator / ADenominator);
end;

//------------------------------------------------------------------------------

{$ifndef PUREPASCAL}

// Note: FMod_F_SSE2 and FMod_D_SSE2 are the exact same except the D variant uses the *d instructions and and the F
//       variant uses the *s instructions. Keep in sync!
function FMod_F_SSE2(ANumerator, ADenominator: TFloat): TFloat; {$IFDEF FPC} assembler; {$IFDEF TARGET_X64}nostackframe;{$ENDIF} {$ENDIF}
asm
{$if defined(TARGET_x86)}
        movss   xmm0, ANumerator        // XMM0 <- ANumerator
        movss   xmm1, ADenominator      // XMM1 <- ADenominator
{$ifend}

        // a := ANumerator
        movss   xmm2, xmm0
        // a := ANumerator / ADenominator
        divss   xmm2, xmm1
        // b := Trunc(a)
        cvttss2si ecx, xmm2
        cvtsi2ss xmm2, ecx
        // c := b*ADenominator
        mulss   xmm2, xmm1
        // Result := ANumerator - c;
        subss   xmm0, xmm2

{$if defined(TARGET_x86)}
        movss   Result, xmm0
{$elseif not defined(TARGET_x64)}
{$error 'Missing target'}
{$ifend}
end;

function FMod_D_SSE2(ANumerator, ADenominator: Double): Double; {$IFDEF FPC} assembler; {$IFDEF TARGET_X64}nostackframe;{$ENDIF} {$ENDIF}
asm
{$if defined(TARGET_x86)}
        movsd   xmm0, ANumerator        // XMM0 <- ANumerator
        movsd   xmm1, ADenominator      // XMM1 <- ADenominator
{$ifend}

        // a := ANumerator
        movsd   xmm2, xmm0
        // a := ANumerator / ADenominator
        divsd   xmm2, xmm1
        // b := Trunc(a)
        cvttsd2si ecx, xmm2
        cvtsi2sd xmm2, ecx
        // c := b*ADenominator
        mulsd   xmm2, xmm1
        // Result := ANumerator - c;
        subsd   xmm0, xmm2

{$if defined(TARGET_x86)}
        movsd   Result, xmm0
{$elseif not defined(TARGET_x64)}
{$error 'Missing target'}
{$ifend}
end;

{$endif PUREPASCAL}


//------------------------------------------------------------------------------

{$ifndef PUREPASCAL}

// Note: FMod_F_SSE41 and FMod_D_SSE41 are the exact same except the D variant uses the *d instructions and and the F
//       variant uses the *s instructions. Keep in sync!
function FMod_F_SSE41(ANumerator, ADenominator: TFloat): TFloat; {$IFDEF FPC} assembler; {$IFDEF TARGET_X64}nostackframe;{$ENDIF} {$ENDIF}
asm
{$if defined(TARGET_x86)}
        movss   xmm0, ANumerator        // XMM0 <- ANumerator
        movss   xmm1, ADenominator      // XMM1 <- ADenominator
{$ifend}

        // a := ANumerator
        movss   xmm2, xmm0
        // a := ANumerator / ADenominator
        divss   xmm2, xmm1
        // b := Trunc(a)
        roundss xmm2, xmm2, ROUND_TO_ZERO or ROUND_NO_EXC
        // c := b*ADenominator
        mulss   xmm2, xmm1
        // Result := ANumerator - c;
        subss   xmm0, xmm2

{$if defined(TARGET_x86)}
        movss   Result, xmm0
{$elseif not defined(TARGET_x64)}
{$error 'Missing target'}
{$ifend}
end;

function FMod_D_SSE41(ANumerator, ADenominator: Double): Double; {$IFDEF FPC} assembler; {$IFDEF TARGET_X64}nostackframe;{$ENDIF} {$ENDIF}
asm
{$if defined(TARGET_x86)}
        movsd   xmm0, ANumerator        // XMM0 <- ANumerator
        movsd   xmm1, ADenominator      // XMM1 <- ADenominator
{$ifend}

        // a := ANumerator
        movsd   xmm2, xmm0
        // a := ANumerator / ADenominator
        divsd   xmm2, xmm1
        // b := Trunc(a)
        roundsd xmm2, xmm2, ROUND_TO_ZERO or ROUND_NO_EXC
        // c := b*ADenominator
        mulsd   xmm2, xmm1
        // Result := ANumerator - c;
        subsd   xmm0, xmm2

{$if defined(TARGET_x86)}
        movsd   Result, xmm0
{$elseif not defined(TARGET_x64)}
{$error 'Missing target'}
{$ifend}
end;

{$endif PUREPASCAL}


//------------------------------------------------------------------------------
//
//      DivMod
//
//------------------------------------------------------------------------------
{$IFDEF PUREPASCAL}

function DivMod(Dividend, Divisor: Integer; var Remainder: Integer): Integer;
begin
  Result := Dividend div Divisor;
  Remainder := Dividend mod Divisor;
end;

{$ELSE}

function DivMod(Dividend, Divisor: Integer; var Remainder: Integer): Integer; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$if defined(TARGET_x86)}

        PUSH    EDX
        CDQ
        IDIV    DWORD PTR [ESP]
        ADD     ESP, $04
        MOV     DWORD PTR [ECX], edx

{$elseif defined(TARGET_x64)}

  {$IFDEF MSWINDOWS}
        MOV     EAX, ECX
        MOV     ECX, EDX
        CDQ
        IDIV    ECX
        MOV     [R8],EDX
  {$ELSE}
        MOV     EAX, EDI
        MOV     RDI, RDX
        CDQ
        IDIV    ESI
        MOV     [RDI],EDX
  {$ENDIF}

{$else}
{$error 'Missing target'}
{$ifend}
end;

{$ENDIF}


//------------------------------------------------------------------------------
//
//      CumSum
//
//------------------------------------------------------------------------------
procedure CumSum_Pas(Values: PSingleArray; Count: Integer);
var
  I: Integer;
  V: TFloat;
begin
  V := Values[0];
  for I := 1 to Count - 1 do
  begin
    if PInteger(@Values[I])^ <> 0 then // TODO : It's probably faster to just do the add than to do a test and a branch
      V := V + Values[I];
    Values[I] := V;
  end;
end;

//------------------------------------------------------------------------------

{$IFNDEF PUREPASCAL}

// Aligned SSE2 version -- Credits: Sanyin <prevodilac@hotmail.com>
procedure CumSum_SSE2(Values: PSingleArray; Count: Integer); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$if defined(TARGET_x86)}

        MOV     ECX,EDX
        CMP     ECX,2       // if count < 2, exit
        JL      @END
        CMP     ECX,32      // if count < 32, avoid SSE2 overhead
        JL      @SMALL

{--- align memory ---}
        PUSH    EBX
        PXOR    XMM4,XMM4
        MOV     EBX,EAX
        AND     EBX,15       // get aligned count
        JZ      @ENDALIGNING // already aligned
        ADD     EBX,-16
        NEG     EBX          // get bytes to advance
        JZ      @ENDALIGNING // already aligned

        MOV     ECX,EBX
        SAR     ECX,2        // div with 4 to get cnt
        SUB     EDX,ECX

        ADD     EAX,4
        DEC     ECX
        JZ      @SETUPLAST   // one element

@ALIGNINGLOOP:
        FLD     DWORD PTR [EAX-4]
        FADD    DWORD PTR [EAX]
        FSTP    DWORD PTR [EAX]
        ADD     EAX,4
        DEC     ECX
        JNZ     @ALIGNINGLOOP

@SETUPLAST:
        MOVUPS  XMM4,[EAX-4]
        PSLLDQ  XMM4,12
        PSRLDQ  XMM4,12

@ENDALIGNING:
        POP     EBX
        PUSH    EBX
        MOV     ECX,EDX
        SAR     ECX,2
@LOOP:
        MOVAPS  XMM0,[EAX]
        PXOR    XMM5,XMM5
        PCMPEQD XMM5,XMM0
        PMOVMSKB EBX,XMM5
        CMP     EBX,$0000FFFF
        JNE     @NORMAL
        PSHUFD  XMM0,XMM4,0
        JMP     @SKIP

@NORMAL:
        ADDPS   XMM0,XMM4
        PSHUFD  XMM1,XMM0,$e4
        PSLLDQ  XMM1,4
        PSHUFD  XMM2,XMM1,$90
        PSHUFD  XMM3,XMM1,$40
        ADDPS   XMM2,XMM3
        ADDPS   XMM1,XMM2
        ADDPS   XMM0,XMM1

        PSHUFLW XMM4,XMM0,$E4
        PSRLDQ  XMM4,12

@SKIP:
        PREFETCHNTA [eax+16*16*2]
        MOVAPS  [EAX],XMM0
        ADD     EAX,16
        SUB     ECX,1
        JNZ     @LOOP
        POP     EBX
        MOV     ECX,EDX
        SAR     ECX,2
        SHL     ECX,2
        SUB     EDX,ECX
        MOV     ECX,EDX
        JZ      @END

@LOOP2:
        FLD     DWORD PTR [EAX-4]
        FADD    DWORD PTR [EAX]
        FSTP    DWORD PTR [EAX]
        ADD     EAX,4
        DEC     ECX
        JNZ     @LOOP2
        JMP     @END

@SMALL:
        MOV     ECX,EDX
        ADD     EAX,4
        DEC     ECX
@LOOP3:
        FLD     DWORD PTR [EAX-4]
        FADD    DWORD PTR [EAX]
        FSTP    DWORD PTR [EAX]
        ADD     EAX,4
        DEC     ECX
        JNZ     @LOOP3
@END:

{$elseif defined(TARGET_x64)}

        CMP     EDX,2       // if count < 2, exit
        JL      @END

        MOV     RAX,RCX
        MOV     ECX,EDX

        CMP     ECX,32      // if count < 32, avoid SSE2 overhead
        JL      @SMALL

{--- align memory ---}
        PXOR    XMM4,XMM4
        MOV     R8D,EAX
        AND     R8D,15       // get aligned count
        JZ      @ENDALIGNING // already aligned
        ADD     R8D,-16
        NEG     R8D          // get bytes to advance
        JZ      @ENDALIGNING // already aligned

        MOV     ECX,R8D
        SAR     ECX,2        // div with 4 to get cnt
        SUB     EDX,ECX

        ADD     RAX,4
        DEC     ECX
        JZ      @SETUPLAST   // one element

@ALIGNINGLOOP:
        FLD     DWORD PTR [RAX - 4]
        FADD    DWORD PTR [RAX]
        FSTP    DWORD PTR [RAX]
        ADD     RAX,4
        DEC     ECX
        JNZ     @ALIGNINGLOOP

@SETUPLAST:
        MOVUPS  XMM4,[RAX - 4]
        PSLLDQ  XMM4,12
        PSRLDQ  XMM4,12

@ENDALIGNING:
        MOV     ECX,EDX
        SAR     ECX,2
@LOOP:
        MOVAPS  XMM0,[RAX]
        PXOR    XMM5,XMM5
        PCMPEQD XMM5,XMM0
        PMOVMSKB R8D,XMM5
        CMP     R8D,$0000FFFF
        JNE     @NORMAL
        PSHUFD  XMM0,XMM4,0
        JMP     @SKIP

@NORMAL:
        ADDPS   XMM0,XMM4
        PSHUFD  XMM1,XMM0,$e4
        PSLLDQ  XMM1,4
        PSHUFD  XMM2,XMM1,$90
        PSHUFD  XMM3,XMM1,$40
        ADDPS   XMM2,XMM3
        ADDPS   XMM1,XMM2
        ADDPS   XMM0,XMM1

        PSHUFLW XMM4,XMM0,$E4
        PSRLDQ  XMM4,12

@SKIP:
        PREFETCHNTA [RAX + 32 * 2]
        MOVAPS  [RAX],XMM0
        ADD     RAX,16
        SUB     ECX,1
        JNZ     @LOOP
        MOV     ECX,EDX
        SAR     ECX,2
        SHL     ECX,2
        SUB     EDX,ECX
        MOV     ECX,EDX
        JZ      @END

@LOOP2:
        FLD     DWORD PTR [RAX - 4]
        FADD    DWORD PTR [RAX]
        FSTP    DWORD PTR [RAX]
        ADD     RAX,4
        DEC     ECX
        JNZ     @LOOP2
        JMP     @END

@SMALL:
        ADD     RAX,4
        DEC     ECX
@LOOP3:
        FLD     DWORD PTR [RAX - 4]
        FADD    DWORD PTR [RAX]
        FSTP    DWORD PTR [RAX]
        ADD     RAX,4
        DEC     ECX
        JNZ     @LOOP3
@END:

{$else}
{$error 'Missing target'}
{$ifend}
end;

{$ENDIF}


//------------------------------------------------------------------------------
//
//      Bindings
//
//------------------------------------------------------------------------------
procedure RegisterBindings;
begin
  MathRegistry := NewRegistry('GR32_Math bindings');

  MathRegistry.RegisterBinding(FID_CUMSUM, @@CumSum);
  MathRegistry.RegisterBinding(FID_FLOATMOD_F, @@FloatMod_F);
  MathRegistry.RegisterBinding(FID_FLOATMOD_D, @@FloatMod_D);
  MathRegistry.RegisterBinding(FID_FLOATREMAINDER_F, @@FloatRemainder_F);
  MathRegistry.RegisterBinding(FID_FLOATREMAINDER_D, @@FloatRemainder_D);
  MathRegistry.RegisterBinding(FID_FMOD_F, @@FMod_F);
  MathRegistry.RegisterBinding(FID_FMOD_D, @@FMod_D);

  // pure pascal
  MathRegistry.Add(FID_CUMSUM, @CumSum_Pas, MathBindingFlagPascal);
  MathRegistry.Add(FID_FLOATMOD_F, @FloatMod_F_Pas, MathBindingFlagPascal);
  MathRegistry.Add(FID_FLOATMOD_D, @FloatMod_D_Pas, MathBindingFlagPascal);
  MathRegistry.Add(FID_FLOATREMAINDER_F, @FloatRemainder_F_Pas, MathBindingFlagPascal);
  MathRegistry.Add(FID_FLOATREMAINDER_D, @FloatRemainder_D_Pas, MathBindingFlagPascal);
  MathRegistry.Add(FID_FMOD_F, @FMod_F_Pas, MathBindingFlagPascal);
  MathRegistry.Add(FID_FMOD_D, @FMod_D_Pas, MathBindingFlagPascal);

{$IFNDEF PUREPASCAL}
{$IFNDEF OMIT_SSE2}
  MathRegistry.Add(FID_CUMSUM, @CumSum_SSE2, [isSSE2]);
  MathRegistry.Add(FID_FLOATMOD_F, @FloatMod_F_SSE41, [isSSE41]);
  MathRegistry.Add(FID_FLOATMOD_D, @FloatMod_D_SSE41, [isSSE41]);
  MathRegistry.Add(FID_FLOATREMAINDER_F, @FloatRemainder_F_SSE41, [isSSE41]);
  MathRegistry.Add(FID_FLOATREMAINDER_D, @FloatRemainder_D_SSE41, [isSSE41]);
  MathRegistry.Add(FID_FMOD_F, @FMod_F_SSE2, [isSSE2]);
  MathRegistry.Add(FID_FMOD_D, @FMod_D_SSE2, [isSSE2]);
  MathRegistry.Add(FID_FMOD_F, @FMod_F_SSE41, [isSSE41]);
  MathRegistry.Add(FID_FMOD_D, @FMod_D_SSE41, [isSSE41]);
{$ENDIF}
{$ENDIF}

  MathRegistry.RebindAll;
end;

//------------------------------------------------------------------------------

initialization
  RegisterBindings;
end.

