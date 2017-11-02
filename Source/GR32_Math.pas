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

uses GR32;

{ Fixed point math routines }
function FixedFloor(A: TFixed): Integer;
function FixedCeil(A: TFixed): Integer;
function FixedMul(A, B: TFixed): TFixed;
function FixedDiv(A, B: TFixed): TFixed;
function OneOver(Value: TFixed): TFixed;
function FixedRound(A: TFixed): Integer;
function FixedSqr(Value: TFixed): TFixed;
function FixedSqrtLP(Value: TFixed): TFixed;      // 8-bit precision
function FixedSqrtHP(Value: TFixed): TFixed;      // 16-bit precision
// Fixed point interpolation
function FixedCombine(W, X, Y: TFixed): TFixed;


{ Trigonometric routines }

procedure SinCos(const Theta: TFloat; out Sin, Cos: TFloat); overload;
procedure SinCos(const Theta, Radius: Single; out Sin, Cos: Single); overload;
procedure SinCos(const Theta, ScaleX, ScaleY: TFloat; out Sin, Cos: Single); overload;
function Hypot(const X, Y: TFloat): TFloat; overload;
function Hypot(const X, Y: Integer): Integer; overload;
function FastSqrt(const Value: TFloat): TFloat;
function FastSqrtBab1(const Value: TFloat): TFloat;
function FastSqrtBab2(const Value: TFloat): TFloat;
function FastInvSqrt(const Value: Single): Single; {$IFDEF INLININGSUPPORTED} inline; {$ENDIF} overload;


{ Misc. Routines }

{ MulDiv a faster implementation of Windows.MulDiv funtion }
function MulDiv(Multiplicand, Multiplier, Divisor: Integer): Integer;

// tells if X is a power of 2, returns true when X = 1,2,4,8,16 etc.
function IsPowerOf2(Value: Integer): Boolean; {$IFDEF INLININGSUPPORTED} inline; {$ENDIF}
// returns X rounded down to the nearest power of two
function PrevPowerOf2(Value: Integer): Integer;
// returns X rounded down to the nearest power of two, i.e. 5 -> 8, 7 -> 8, 15 -> 16
function NextPowerOf2(Value: Integer): Integer;

// fast average without overflow, useful for e.g. fixed point math
function Average(A, B: Integer): Integer;
// fast sign function
function Sign(Value: Integer): Integer;

function FloatMod(x, y: Double): Double; {$IFDEF INLININGSUPPORTED} inline; {$ENDIF}

function DivMod(Dividend, Divisor: Integer; var Remainder: Integer): Integer;


{$IFDEF FPC}
{$IFDEF TARGET_X64}
(*
  FPC has no similar {$EXCESSPRECISION OFF} directive,
  but we can easily emulate that by overriding some internal math functions
*)
function PI: Single; [internproc: fpc_in_pi_real];
//function Abs(D: Single): Single; [internproc: fpc_in_abs_real];
//function Sqr(D: Single): Single; [internproc: fpc_in_sqr_real];
function Sqrt(D: Single): Single; [internproc: fpc_in_sqrt_real];
function ArcTan(D: Single): Single; [internproc: fpc_in_arctan_real];
function Ln(D: Single): Single; [internproc: fpc_in_ln_real];
function Sin(D: Single): Single; [internproc: fpc_in_sin_real];
function Cos(D: Single): Single; [internproc: fpc_in_cos_real];
function Exp(D: Single): Single; [internproc: fpc_in_exp_real];
function Round(D: Single): Int64; [internproc: fpc_in_round_real];
function Frac(D: Single): Single; [internproc: fpc_in_frac_real];
function Int(D: Single): Single; [internproc: fpc_in_int_real];
function Trunc(D: Single): Int64; [internproc: fpc_in_trunc_real];

function Ceil(X: Single): Integer; {$IFDEF INLININGSUPPORTED} inline; {$ENDIF}
function Floor(X: Single): Integer; {$IFDEF INLININGSUPPORTED} inline; {$ENDIF}
{$ENDIF}
{$ENDIF}

type
  TCumSumProc = procedure(Values: PSingleArray; Count: Integer);

var
  CumSum: TCumSumProc;

implementation

uses
  Math, GR32_System;

{$IFDEF PUREPASCAL}
const
  FixedOneS: Single = 65536;
{$ENDIF}


{$IFDEF FPC}
{$IFDEF TARGET_X64}
function Ceil(X: Single): Integer;
begin
  Result := Trunc(X);
  if (X - Result) > 0 then
    Inc(Result);
end;

function Floor(X: Single): Integer;
begin
  Result := Trunc(X);
  if (X - Result) < 0 then
    Dec(Result);
end;
{$ENDIF}
{$ENDIF}


{ Fixed-point math }

function FixedFloor(A: TFixed): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := A div FIXEDONE;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
        SAR     EAX, 16
{$ENDIF}
{$IFDEF TARGET_x64}
        MOV     EAX, ECX
        SAR     EAX, 16
{$ENDIF}
{$ENDIF}
end;

function FixedCeil(A: TFixed): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := (A + $FFFF) div FIXEDONE;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
        ADD     EAX, $0000FFFF
        SAR     EAX, 16
{$ENDIF}
{$IFDEF TARGET_x64}
        MOV     EAX, ECX
        ADD     EAX, $0000FFFF
        SAR     EAX, 16
{$ENDIF}
{$ENDIF}
end;

function FixedRound(A: TFixed): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := (A + $7FFF) div FIXEDONE;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
        ADD     EAX, $00007FFF
        SAR     EAX, 16
{$ENDIF}
{$IFDEF TARGET_x64}
        MOV     EAX, ECX
        ADD     EAX, $00007FFF
        SAR     EAX, 16
{$ENDIF}
{$ENDIF}
end;

function FixedMul(A, B: TFixed): TFixed;
{$IFDEF PUREPASCAL}
begin
  Result := Round(A * FixedToFloat * B);
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
        IMUL    EDX
        SHRD    EAX, EDX, 16
{$ENDIF}
{$IFDEF TARGET_x64}
        MOV     EAX, ECX
        IMUL    EDX
        SHRD    EAX, EDX, 16
{$ENDIF}
{$ENDIF}
end;

function FixedDiv(A, B: TFixed): TFixed;
{$IFDEF PUREPASCAL}
begin
  Result := Round(A / B * FixedOne);
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
        MOV     ECX, B
        CDQ
        SHLD    EDX, EAX, 16
        SHL     EAX, 16
        IDIV    ECX
{$ENDIF}
{$IFDEF TARGET_x64}
        MOV     EAX, ECX
        MOV     ECX, EDX
        CDQ
        SHLD    EDX, EAX, 16
        SHL     EAX, 16
        IDIV    ECX
{$ENDIF}
{$ENDIF}
end;

function OneOver(Value: TFixed): TFixed;
{$IFDEF PUREPASCAL}
const
  Dividend: Single = 4294967296; // FixedOne * FixedOne
begin
  Result := Round(Dividend / Value);
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
        MOV     ECX, Value
        XOR     EAX, EAX
        MOV     EDX, 1
        IDIV    ECX
{$ENDIF}
{$IFDEF TARGET_x64}
        XOR     EAX, EAX
        MOV     EDX, 1
        IDIV    ECX
{$ENDIF}
{$ENDIF}
end;

function FixedSqr(Value: TFixed): TFixed;
{$IFDEF PUREPASCAL}
begin
  Result := Round(Value * FixedToFloat * Value);
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
        IMUL    EAX
        SHRD    EAX, EDX, 16
{$ENDIF}
{$IFDEF TARGET_x64}
        MOV     EAX, Value
        IMUL    EAX
        SHRD    EAX, EDX, 16
{$ENDIF}
{$ENDIF}
end;

function FixedSqrtLP(Value: TFixed): TFixed;
{$IFDEF PUREPASCAL}
begin
  Result := Round(Sqrt(Value * FixedOneS));
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
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
        MOV     ECX,EDX
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
{$ENDIF}
{$IFDEF TARGET_x64}
        PUSH    RBX
        XOR     EAX, EAX
        MOV     EBX, $40000000
@SqrtLP1:
        MOV     EDX, ECX
        SUB     EDX, EBX
        JL      @SqrtLP2
        SUB     EDX, EAX
        JL      @SqrtLP2
        MOV     ECX,EDX
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
        POP     RBX
{$ENDIF}
{$ENDIF}
end;

function FixedSqrtHP(Value: TFixed): TFixed;
{$IFDEF PUREPASCAL}
begin
  Result := Round(Sqrt(Value * FixedOneS));
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
        PUSH    EBX
        MOV     ECX, EAX
        XOR     EAX, EAX
        MOV     EBX, $40000000
@SqrtHP1:
        MOV     EDX, ECX
        SUB     EDX, EBX
        jb      @SqrtHP2
        SUB     EDX, EAX
        jb      @SqrtHP2
        MOV     ECX,EDX
        SHR     EAX, 1
        OR      EAX, EBX
        SHR     EBX, 2
        JNZ     @SqrtHP1
        JZ      @SqrtHP5
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
        jb      @SqrtHP4
        SUB     EDX, EAX
        jb      @SqrtHP4
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
{$ENDIF}
{$IFDEF TARGET_x64}
        PUSH    RBX
        XOR     EAX, EAX
        MOV     EBX, $40000000
@SqrtHP1:
        MOV     EDX, ECX
        SUB     EDX, EBX
        jb      @SqrtHP2
        SUB     EDX, EAX
        jb      @SqrtHP2
        MOV     ECX,EDX
        SHR     EAX, 1
        OR      EAX, EBX
        SHR     EBX, 2
        JNZ     @SqrtHP1
        JZ      @SqrtHP5
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
        jb      @SqrtHP4
        SUB     EDX, EAX
        jb      @SqrtHP4
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
        POP     RBX
{$ENDIF}
{$ENDIF}
end;

function FixedCombine(W, X, Y: TFixed): TFixed;
// EAX <- W, EDX <- X, ECX <- Y
// combine fixed value X and fixed value Y with the weight of X given in W
// Result Z = W * X + (1 - W) * Y = Y + (X - Y) * W
// Fixed Point Version: Result Z = Y + (X - Y) * W / 65536
{$IFDEF PUREPASCAL}
begin
  Result := Round(Y + (X - Y) * FixedToFloat * W);
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
        SUB     EDX, ECX
        IMUL    EDX
        SHRD    EAX, EDX, 16
        ADD     EAX, ECX
{$ENDIF}
{$IFDEF TARGET_x64}
        MOV     EAX, ECX
        SUB     EDX, R8D
        IMUL    EDX
        SHRD    EAX, EDX, 16
        ADD     EAX, R8D
{$ENDIF}
{$ENDIF}
end;

{ Trigonometry }

procedure SinCos(const Theta: TFloat; out Sin, Cos: TFloat);
{$IFDEF NATIVE_SINCOS}
var
  S, C: Extended;
begin
  Math.SinCos(Theta, S, C);
  Sin := S;
  Cos := C;
{$ELSE}
{$IFDEF TARGET_x64}
var
  Temp: TFloat;
{$ENDIF}
asm
{$IFDEF TARGET_x86}
        FLD     Theta
        FSINCOS
        FSTP    DWORD PTR [EDX] // cosine
        FSTP    DWORD PTR [EAX] // sine
{$ENDIF}
{$IFDEF TARGET_x64}
        MOVD    Temp, Theta
        FLD     Temp
        FSINCOS
        FSTP    [Sin] // cosine
        FSTP    [Cos] // sine
{$ENDIF}
{$ENDIF}
end;

procedure SinCos(const Theta, Radius: TFloat; out Sin, Cos: TFloat);
{$IFDEF NATIVE_SINCOS}
var
  S, C: Extended;
begin
  Math.SinCos(Theta, S, C);
  Sin := S * Radius;
  Cos := C * Radius;
{$ELSE}
{$IFDEF TARGET_x64}
var
  Temp: TFloat;
{$ENDIF}
asm
{$IFDEF TARGET_x86}
        FLD     Theta
        FSINCOS
        FMUL    Radius
        FSTP    DWORD PTR [EDX] // cosine
        FMUL    Radius
        FSTP    DWORD PTR [EAX] // sine
{$ENDIF}
{$IFDEF TARGET_x64}
        MOVD    Temp, Theta
        FLD     Temp
        MOVD    Temp, Radius
        FSINCOS
        FMUL    Temp
        FSTP    [Cos]
        FMUL    Temp
        FSTP    [Sin]
{$ENDIF}
{$ENDIF}
end;

procedure SinCos(const Theta, ScaleX, ScaleY: TFloat; out Sin, Cos: Single); overload;
{$IFDEF NATIVE_SINCOS}
var
  S, C: Extended;
begin
  Math.SinCos(Theta, S, C);
  Sin := S * ScaleX;
  Cos := C * ScaleY;
{$ELSE}
{$IFDEF TARGET_x64}
var
  Temp: TFloat;
{$ENDIF}
asm
{$IFDEF TARGET_x86}
        FLD     Theta
        FSINCOS
        FMUL    ScaleX
        FSTP    DWORD PTR [EDX] // cosine
        FMUL    ScaleY
        FSTP    DWORD PTR [EAX] // sine
{$ENDIF}
{$IFDEF TARGET_x64}
        MOVD    Temp, Theta
        FLD     Temp
        FSINCOS
        MOVD    Temp, ScaleX
        FMUL    Temp
        FSTP    [Cos]
        MOVD    Temp, ScaleY
        FMUL    Temp
        FSTP    [Sin]
{$ENDIF}
{$ENDIF}
end;

function Hypot(const X, Y: TFloat): TFloat;
{$IFDEF PUREPASCAL}
begin
  Result := Sqrt(Sqr(X) + Sqr(Y));
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
        FLD     X
        FMUL    ST,ST
        FLD     Y
        FMUL    ST,ST
        FADDP   ST(1),ST
        FSQRT
        FWAIT
{$ENDIF}
{$IFDEF TARGET_x64}
        MULSS   XMM0, XMM0
        MULSS   XMM1, XMM1
        ADDSS   XMM0, XMM1
        SQRTSS  XMM0, XMM0
{$ENDIF}
{$ENDIF}
end;

function Hypot(const X, Y: Integer): Integer;
//{$IFDEF PUREPASCAL}
begin
  Result := Round(Math.Hypot(X, Y));
(*
{$ELSE}
asm
{$IFDEF TARGET_x64}
        IMUL    RAX, RCX, RDX
{$ELSE}
        FLD     X
        FMUL    ST,ST
        FLD     Y
        FMUL    ST,ST
        FADDP   ST(1),ST
        FSQRT
        FISTP   [ESP - 4]
        MOV     EAX, [ESP - 4]
        FWAIT
{$ENDIF}
{$ENDIF}
*)
end;

function FastSqrt(const Value: TFloat): TFloat;
// see http://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Approximations_that_depend_on_IEEE_representation
{$IFDEF PUREPASCAL}
var
  I: Integer absolute Value;
  J: Integer absolute Result;
begin
  J := (I - $3F800000) div 2 + $3F800000;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
        MOV     EAX, DWORD PTR Value
        SUB     EAX, $3F800000
        SAR     EAX, 1
        ADD     EAX, $3F800000
        MOV     DWORD PTR [ESP - 4], EAX
        FLD     DWORD PTR [ESP - 4]
{$ENDIF}
{$IFDEF TARGET_x64}
        SQRTSS  XMM0, XMM0
{$ENDIF}
{$ENDIF}
end;

function FastSqrtBab1(const Value: TFloat): TFloat;
// see http://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Approximations_that_depend_on_IEEE_representation
// additionally one babylonian step added
{$IFNDEF PUREPASCAL}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
{$ENDIF}
const
  CHalf : TFloat = 0.5;
{$IFDEF PUREPASCAL}
var
  I: Integer absolute Value;
  J: Integer absolute Result;
begin
  J := (I - $3F800000) div 2 + $3F800000;
  Result := CHalf * (Result + Value / Result);
{$ELSE}
asm
{$IFDEF TARGET_x86}
        MOV     EAX, Value
        SUB     EAX, $3F800000
        SAR     EAX, 1
        ADD     EAX, $3F800000
        MOV     DWORD PTR [ESP - 4], EAX
        FLD     Value
        FDIV    DWORD PTR [ESP - 4]
        FADD    DWORD PTR [ESP - 4]
        FMUL    CHalf
{$ENDIF}
{$IFDEF TARGET_x64}
        SQRTSS  XMM0, XMM0
{$ENDIF}
{$ENDIF}
end;

function FastSqrtBab2(const Value: TFloat): TFloat;
// see http://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Approximations_that_depend_on_IEEE_representation
// additionally two babylonian steps added
{$IFDEF PUREPASCAL}
const
  CQuarter : TFloat = 0.25;
var
  J: Integer absolute Result;
begin
 Result := Value;
 J := ((J - (1 shl 23)) shr 1) + (1 shl 29);
 Result := Result + Value / Result;
 Result := CQuarter * Result + Value / Result;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
const
  CHalf : TFloat = 0.5;
asm
{$IFDEF TARGET_x86}
        MOV     EAX, Value
        SUB     EAX, $3F800000
        SAR     EAX, 1
        ADD     EAX, $3F800000
        MOV     DWORD PTR [ESP - 4], EAX
        FLD     Value
        FDIV    DWORD PTR [ESP - 4]
        FADD    DWORD PTR [ESP - 4]
        FMUL    CHalf
{$ENDIF}
{$IFDEF TARGET_x64}
        MOVD    EAX, Value
        SUB     EAX, $3F800000
        SAR     EAX, 1
        ADD     EAX, $3F800000
        MOVD    XMM1, EAX
        DIVSS   XMM0, XMM1
        ADDSS   XMM0, XMM1
        MOVD    XMM1, [RIP + CHalf]
        MULSS   XMM0, XMM1
{$ENDIF}
{$ENDIF}
end;

function FastInvSqrt(const Value: Single): Single;
var
  IntCst : Cardinal absolute result;
begin
  Result := Value;
  IntCst := ($BE6EB50C - IntCst) shr 1;
  Result := 0.5 * Result * (3 - Value * Sqr(Result));
end;

{ Misc. }

function MulDiv(Multiplicand, Multiplier, Divisor: Integer): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := Int64(Multiplicand) * Int64(Multiplier) div Divisor;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
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
{$ENDIF}
{$IFDEF TARGET_x64}
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
{$ENDIF}
{$ENDIF}
end;

function IsPowerOf2(Value: Integer): Boolean;
//returns true when X = 1,2,4,8,16 etc.
begin
  Result := Value and (Value - 1) = 0;
end;

function PrevPowerOf2(Value: Integer): Integer;
//returns X rounded down to the power of two
{$IFDEF PUREPASCAL}
begin
  Result := 1;
  while Value shr 1 > 0 do
    Result := Result shl 1;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
        BSR     ECX, EAX
        SHR     EAX, CL
        SHL     EAX, CL
{$ENDIF}
{$IFDEF TARGET_x64}
        MOV     EAX, Value
        BSR     ECX, EAX
        SHR     EAX, CL
        SHL     EAX, CL
{$ENDIF}
{$ENDIF}
end;

function NextPowerOf2(Value: Integer): Integer;
//returns X rounded up to the power of two, i.e. 5 -> 8, 7 -> 8, 15 -> 16
{$IFDEF PUREPASCAL}
begin
  Result := 2;
  while Value shr 1 > 0 do 
    Result := Result shl 1;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
        DEC     EAX
        JLE     @1
        BSR     ECX, EAX
        MOV     EAX, 2
        SHL     EAX, CL
        RET
@1:
        MOV     EAX, 1
{$ENDIF}
{$IFDEF TARGET_x64}
        MOV     EAX, Value
        DEC     EAX
        JLE     @1
        BSR     ECX, EAX
        MOV     EAX, 2
        SHL     EAX, CL
        RET
@1:
        MOV     EAX, 1
{$ENDIF}
{$ENDIF}
end;

function Average(A, B: Integer): Integer;
//fast average without overflow, useful e.g. for fixed point math
//(A + B)/2 = (A and B) + (A xor B)/2
{$IFDEF PUREPASCAL}
begin
  Result := (A and B) + (A xor B) div 2;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
        MOV     ECX, EDX
        XOR     EDX, EAX
        SAR     EDX, 1
        AND     EAX, ECX
        ADD     EAX, EDX
{$ENDIF}
{$IFDEF TARGET_x64}
        MOV     EAX, A
        MOV     ECX, EDX
        XOR     EDX, EAX
        SAR     EDX, 1
        AND     EAX, ECX
        ADD     EAX, EDX
{$ENDIF}
{$ENDIF}
end;

function Sign(Value: Integer): Integer;
{$IFDEF PUREPASCAL}
begin
  //Assumes 32 bit integer
  Result := (- Value) shr 31 - (Value shr 31);
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x64}
        MOV     EAX, Value
{$ENDIF}
        CDQ
        NEG     EAX
        ADC     EDX, EDX
        MOV     EAX, EDX
{$ENDIF}
end;

function FloatMod(x, y: Double): Double;
begin
  if (y = 0) then
    Result := X
  else
    Result := x - y * Floor(x / y);
end;

function DivMod(Dividend, Divisor: Integer; var Remainder: Integer): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := Dividend div Divisor;
  Remainder := Dividend mod Divisor;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
        PUSH    EDX
        CDQ
        IDIV    DWORD PTR [ESP]
        ADD     ESP, $04
        MOV     DWORD PTR [ECX], edx
{$ENDIF}
{$IFDEF TARGET_x64}
        MOV     RAX, RCX
        MOV     R9, RDX
        CDQ
        IDIV    R9
        MOV     DWORD PTR [R8], EDX
{$ENDIF}
{$ENDIF}
end;

procedure CumSum_Pas(Values: PSingleArray; Count: Integer);
var
  I: Integer;
  V: TFloat;
begin
  V := Values[0];
  for I := 1 to Count - 1 do
  begin
    if PInteger(@Values[I])^ <> 0 then
      V := V + Values[I];
    Values[I] := V;
  end;
end;

{$IFNDEF PUREPASCAL}
// Aligned SSE2 version -- Credits: Sanyin <prevodilac@hotmail.com>
procedure CumSum_SSE2(Values: PSingleArray; Count: Integer); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
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
{$ENDIF}
{$IFDEF TARGET_x64}
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
{$ENDIF}
@END:
end;
{$ENDIF}


initialization
{$IFNDEF PUREPASCAL}
  if HasInstructionSet(ciSSE2) then
    CumSum := CumSum_SSE2
  else
{$ENDIF}
    CumSum := CumSum_Pas;

end.
