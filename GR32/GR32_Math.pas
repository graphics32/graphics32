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
 * The Original Code is MicroTiles Repaint Optimizer Extension for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Mattias Andersson <mattias@centaurix.com>
 *
 * Portions created by the Initial Developer are Copyright (C) 2005
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

uses GR32;

{ Fixed point math routines }
function Fixed(S: Single): TFixed; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function Fixed(I: Integer): TFixed; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function FixedFloor(A: TFixed): Integer;
function FixedCeil(A: TFixed): Integer;
function FixedMul(A, B: TFixed): TFixed;
function FixedDiv(A, B: TFixed): TFixed;
function FixedRound(A: TFixed): Integer;
function FixedSqr(Value: TFixed): TFixed;
function FixedSqrtLP(Value: TFixed): TFixed;      // 8-bit precision
function FixedSqrtHP(Value: TFixed): TFixed;      // 16-bit precision

{ Trigonometric routines }
procedure SinCos(const Theta: Single; var Sin, Cos: Single); overload;
procedure SinCos(const Theta, Radius: Single; var Sin, Cos: Single); overload;

implementation

{ Fixed-point math }

function Fixed(S: Single): TFixed;
begin
  Result := Round(S * 65536);
end;

function Fixed(I: Integer): TFixed;
begin
  Result := I shl 16;
end;

function FixedFloor(A: TFixed): Integer;
asm
        SAR     EAX, 16;
end;

function FixedCeil(A: TFixed): Integer;
asm
        ADD     EAX, $0000FFFF
        SAR     EAX, 16;
end;

function FixedRound(A: TFixed): Integer;
asm
        ADD     EAX, $00007FFF
        SAR     EAX, 16
end;

function FixedMul(A, B: TFixed): TFixed;
asm
        IMUL    EDX
        SHRD    EAX, EDX, 16
end;

function FixedDiv(A, B: TFixed): TFixed;
asm
        MOV     ECX, B
        CDQ
        SHLD    EDX, EAX, 16
        SHL     EAX, 16
        IDIV    ECX
end;

function FixedSqr(Value: TFixed): TFixed;
asm
          IMUL    EAX
          SHRD    EAX, EDX, 16
end;

function FixedSqrtLP(Value: TFixed): TFixed;
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
end;

function FixedSqrtHP(Value: TFixed): TFixed;
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
end;

{ Trigonometry }

procedure SinCos(const Theta: Single; var Sin, Cos: Single);
asm
   FLD  Theta
   FSINCOS
   FSTP DWORD PTR [EDX]    // cosine
   FSTP DWORD PTR [EAX]    // sine
end;

procedure SinCos(const theta, radius : Single; var Sin, Cos: Single);
asm
   FLD  theta
   FSINCOS
   FMUL radius
   FSTP DWORD PTR [EDX]    // cosine
   FMUL radius
   FSTP DWORD PTR [EAX]    // sine
end;



end.
