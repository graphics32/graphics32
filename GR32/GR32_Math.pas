unit GR32_Math;

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
 * The Initial Developer of the Original Code is
 * Mattias Andersson <mattias@centaurix.com>
 * (parts of this unit were moved here from GR32_System.pas and GR32.pas by Alex A. Denisov)
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2007
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

procedure SinCos(const Theta: Single; var Sin, Cos: Single); overload;
procedure SinCos(const Theta, Radius: Single; var Sin, Cos: Single); overload;
function Hypot(const X, Y: TFloat): TFloat;


{ Misc. Routines }

{ MulDiv a faster implementation of Windows.MulDiv funtion }
function MulDiv(Multiplicand, Multiplier, Divisor: Integer): Integer;

// tells if X is a power of 2, returns true when X = 1,2,4,8,16 etc.
function IsPowerOf2(Value: Integer): Boolean;
// returns X rounded down to the nearest power of two
function PrevPowerOf2(Value: Integer): Integer;
// returns X rounded down to the nearest power of two, i.e. 5 -> 8, 7 -> 8, 15 -> 16
function NextPowerOf2(Value: Integer): Integer;

// fast average without overflow, useful for e.g. fixed point math
function Average(A, B: Integer): Integer;
// fast sign function
function Sign(Value: Integer): Integer;

implementation

{$IFNDEF TARGET_x86}
uses 
  Math;
{$ENDIF}

{ Fixed-point math }

function FixedFloor(A: TFixed): Integer;
{$IFNDEF TARGET_x86}
begin
  Result := A div 65536;
{$ELSE}
asm
        SAR     EAX, 16;
{$ENDIF}
end;

function FixedCeil(A: TFixed): Integer;
{$IFNDEF TARGET_x86}
begin
  Result := (A + $FFFF) div $10000;
{$ELSE}
asm
        ADD     EAX, $0000FFFF
        SAR     EAX, 16;
{$ENDIF}
end;

function FixedRound(A: TFixed): Integer;
{$IFNDEF TARGET_x86}
begin
  Result := (A + $7FFF) div $10000;
{$ELSE}
asm
        ADD     EAX, $00007FFF
        SAR     EAX, 16
{$ENDIF}
end;

function FixedMul(A, B: TFixed): TFixed;
{$IFNDEF TARGET_x86}
begin
  Result := Round(A * B * FixedToFloat);
{$ELSE}
asm
        IMUL    EDX
        SHRD    EAX, EDX, 16
{$ENDIF}
end;

function FixedDiv(A, B: TFixed): TFixed;
{$IFNDEF TARGET_x86}
begin
  Result := Round(A / B * FixedOne);
{$ELSE}
asm
        MOV     ECX, B
        CDQ
        SHLD    EDX, EAX, 16
        SHL     EAX, 16
        IDIV    ECX
{$ENDIF}
end;

function OneOver(Value: TFixed): TFixed;
{$IFNDEF TARGET_x86}
const
  Dividend: Single = 4294967296; // FixedOne * FixedOne
begin
  Result := Round(Dividend / Value);
{$ELSE}
asm
        MOV     ECX,EAX
        XOR     EAX,EAX
        MOV     EDX,1
        IDIV    ECX
{$ENDIF}
end;

function FixedSqr(Value: TFixed): TFixed;
{$IFNDEF TARGET_x86}
begin
  Result := Round(Sqr(Value) * FixedToFloat);
{$ELSE}
asm
          IMUL    EAX
          SHRD    EAX, EDX, 16
{$ENDIF}
end;

function FixedSqrtLP(Value: TFixed): TFixed;
{$IFNDEF TARGET_x86}
begin
  Result := Round(Sqrt(Value * FixedOne));
{$ELSE}
asm
          push    ebx
          mov     ecx, eax
          xor     eax, eax
          mov     ebx, $40000000
@sqrtLP1: mov     edx, ecx
          sub     edx, ebx
          jl      @sqrtLP2
          sub     edx, eax
          jl      @sqrtLP2
          mov     ecx,edx
          shr     eax, 1
          or      eax, ebx
          shr     ebx, 2
          jnz     @sqrtLP1
          shl     eax, 8
          jmp     @sqrtLP3
@sqrtLP2: shr     eax, 1
          shr     ebx, 2
          jnz     @sqrtLP1
          shl     eax, 8
@sqrtLP3: pop     ebx
{$ENDIF}
end;

function FixedSqrtHP(Value: TFixed): TFixed;
{$IFNDEF TARGET_x86}
begin
  Result := Round(Sqrt(Value * FixedOne));
{$ELSE}
asm
          push ebx
          mov ecx, eax
          xor eax, eax
          mov ebx, $40000000
@sqrtHP1: mov edx, ecx
          sub edx, ebx
          jb  @sqrtHP2
          sub edx, eax
          jb  @sqrtHP2
          mov ecx,edx
          shr eax, 1
          or  eax, ebx
          shr ebx, 2
          jnz @sqrtHP1
          jz  @sqrtHP5
@sqrtHP2: shr eax, 1
          shr ebx, 2
          jnz @sqrtHP1
@sqrtHP5: mov ebx, $00004000
          shl eax, 16
          shl ecx, 16
@sqrtHP3: mov edx, ecx
          sub edx, ebx
          jb  @sqrtHP4
          sub edx, eax
          jb  @sqrtHP4
          mov ecx, edx
          shr eax, 1
          or  eax, ebx
          shr ebx, 2
          jnz @sqrtHP3
          jmp @sqrtHP6
@sqrtHP4: shr eax, 1
          shr ebx, 2
          jnz @sqrtHP3
@sqrtHP6: pop ebx
{$ENDIF}
end;

function FixedCombine(W, X, Y: TFixed): TFixed;
// EAX <- W, EDX <- X, ECX <- Y
// combine fixed value X and fixed value Y with the weight of X given in W
// Result Z = W * X + (1 - W) * Y = Y + (X - Y) * W
// Fixed Point Version: Result Z = Y + (X - Y) * W / 65536
{$IFNDEF TARGET_x86}
begin
  Result := Round(Y + (X - Y) * W * FixedToFloat);
{$ELSE}
asm
      SUB  EDX,ECX
      IMUL EDX
      SHRD EAX,EDX,16
      ADD  EAX,ECX
{$ENDIF}
end;

{ Trigonometry }

procedure SinCos(const Theta: TFloat; var Sin, Cos: TFloat);
{$IFNDEF TARGET_x86}
begin
  Sin := System.Sin(Theta);
  Cos := System.Cos(Theta);
{$ELSE}
asm
   FLD  Theta
   FSINCOS
   FSTP DWORD PTR [EDX]    // cosine
   FSTP DWORD PTR [EAX]    // sine
{$ENDIF}
end;

procedure SinCos(const Theta, Radius : TFloat; var Sin, Cos: TFloat);
{$IFNDEF TARGET_x86}
begin
  Sin := System.Sin(Theta) * Radius;
  Cos := System.Cos(Theta) * Radius;
{$ELSE}
asm
   FLD  theta
   FSINCOS
   FMUL radius
   FSTP DWORD PTR [EDX]    // cosine
   FMUL radius
   FSTP DWORD PTR [EAX]    // sine
{$ENDIF}
end;

function Hypot(const X, Y: TFloat): TFloat;
{$IFNDEF TARGET_x86}
begin
  Result := Math.Hypot(X, Y);
{$ELSE}
asm
        FLD     X
        FMUL    ST,ST
        FLD     Y
        FMUL    ST,ST
        FADDP
        FSQRT
        FWAIT
{$ENDIF}
end;

{ Misc. }

function MulDiv(Multiplicand, Multiplier, Divisor: Integer): Integer;
{$IFNDEF TARGET_x86}
begin
  Result := Round(Multiplicand * Multiplier / Divisor);
{$ELSE}
asm
        PUSH    EBX             // Imperative save
        PUSH    ESI             // of EBX and ESI

        MOV     EBX,EAX         // Result will be negative or positive so set rounding direction
        XOR     EBX,EDX         //  Negative: substract 1 in case of rounding
        XOR     EBX,ECX         //  Positive: add 1

        OR      EAX,EAX         // Make all operands positive, ready for unsigned operations
        JNS     @m1Ok           // minimizing branching
        NEG     EAX
@m1Ok:
        OR      EDX,EDX
        JNS     @m2Ok
        NEG     EDX
@m2Ok:
        OR      ECX,ECX
        JNS     @DivOk
        NEG     ECX
@DivOK:
        MUL     EDX             // Unsigned multiply (Multiplicand*Multiplier)

        MOV     ESI,EDX         // Check for overflow, by comparing
        SHL     ESI,1           // 2 times the high-order 32 bits of the product (edx)
        CMP     ESI,ECX         // with the Divisor.
        JAE     @Overfl         // If equal or greater than overflow with division anticipated

        DIV     ECX             // Unsigned divide of product by Divisor

        SUB     ECX,EDX         // Check if the result must be adjusted by adding or substracting
        CMP     ECX,EDX         // 1 (*.5 -> nearest integer), by comparing the difference of
        JA      @NoAdd          // Divisor and remainder with the remainder. If it is greater then
        INC     EAX             // no rounding needed; add 1 to result otherwise
@NoAdd:
        OR      EBX,EDX         // From unsigned operations back the to original sign of the result
        JNS     @exit           // must be positive
        NEG     EAX             // must be negative
        JMP     @exit
@Overfl:
        OR      EAX,-1          //  3 bytes alternative for mov eax,-1. Windows.MulDiv "overflow"
                                //  and "zero-divide" return value
@exit:
        POP     ESI             // Restore
        POP     EBX             // esi and ebx
{$ENDIF}
end;

function IsPowerOf2(Value: Integer): Boolean;
//returns true when X = 1,2,4,8,16 etc.
{$IFNDEF TARGET_x86}
begin
  Result := Value and (Value - 1) <> 0;
{$ELSE}
asm
        LEA     EDX,[EAX-1]
        AND     EAX,EDX
        SETZ    AL
{$ENDIF}
end;

function PrevPowerOf2(Value: Integer): Integer;
//returns X rounded down to the power of two
{$IFNDEF TARGET_x86}
begin
  Result := 1;
  while Value shr 1 > 0 do
    Result := Result shl 1;
{$ELSE}
asm
        BSR     ECX,EAX
        SHR     EAX,CL
        SHL     EAX,CL
{$ENDIF}
end;

function NextPowerOf2(Value: Integer): Integer;
//returns X rounded up to the power of two, i.e. 5 -> 8, 7 -> 8, 15 -> 16
{$IFNDEF TARGET_x86}
begin
  Result := 2;
  while Value shr 1 > 0 do 
  	Result := Result shl 1;
{$ELSE}
asm
        DEC     EAX
        JLE     @1
        BSR     ECX,EAX
        MOV     EAX,2
        SHL     EAX,CL
        RET
@1:     MOV     EAX,1
{$ENDIF}
end;

function Average(A, B: Integer): Integer;
//fast average without overflow, useful e.g. for fixed point math
//(A + B)/2 = (A and B) + (A xor B)/2
{$IFNDEF TARGET_x86}
begin
  Result := (A and B) + (A xor B) div 2;
{$ELSE}
asm
        MOV     ECX,EDX
        XOR     EDX,EAX
        SAR     EDX,1
        AND     EAX,ECX
        ADD     EAX,EDX
{$ENDIF}
end;

function Sign(Value: Integer): Integer;
{$IFNDEF TARGET_x86}
begin
  if Value < 0 then
  	Result := -1
  else 
  	Result := 1;
{$ELSE}
asm
        CDQ
        NEG     EAX
        ADC     EDX,EDX
        MOV     EAX,EDX
{$ENDIF}
end;

end.
