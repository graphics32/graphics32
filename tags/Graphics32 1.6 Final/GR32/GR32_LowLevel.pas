unit GR32_LowLevel;

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
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2004
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)
// $Id: GR32_LowLevel.pas,v 1.1 2004/07/05 15:32:04 abeckedorf Exp $

interface

{$I GR32.inc}

uses
  {$IFDEF CLX}
  QGraphics,
  {$ELSE}
  Graphics,
  {$ENDIF}
  GR32;

{ Clamp function restricts Value to [0..255] range }
function Clamp(Value: Integer): TColor32;

{ An analogue of FillChar for 32 bit values }
procedure FillLongword(var X; Count: Integer; Value: Longword);

{ An analogue of Move for 32 bit values }
procedure MoveLongword(const Source; var Dest; Count: Integer);

{ Exchange two 32-bit values }
procedure Swap(var A, B: Integer);

{ Exhange A <-> B only if B < A }
procedure TestSwap(var A, B: Integer);

{ Exhange A <-> B only if B < A then restrict both to [0..Size-1] range }
{ returns true if resulting range has common points with [0..Size-1] range }
function TestClip(var A, B: Integer; Size: Integer): Boolean;

{ Returns Value constrained to [Lo..Hi] range}
function Constrain(Value, Lo, Hi: Integer): Integer;

{ shift right with sign conservation }
function SAR_4(Value: Integer): Integer;
function SAR_8(Value: Integer): Integer;
function SAR_9(Value: Integer): Integer;
function SAR_12(Value: Integer): Integer;
function SAR_13(Value: Integer): Integer;
function SAR_14(Value: Integer): Integer;
function SAR_16(Value: Integer): Integer;

{ ColorSwap exchanges ARGB <-> ABGR and fill A with $FF }
function ColorSwap(WinColor: TColor): TColor32;

{ MulDiv a faster implementation of Windows.MulDiv funtion }
function MulDiv(Multiplicand, Multiplier, Divisor: Integer): Integer;

implementation

{$R-}{$Q-}  // switch off overflow and range checking

function Clamp(Value: Integer): TColor32;
begin
  if Value < 0 then Result := 0
  else if Value > 255 then Result := 255
  else Result := Value;
end;

procedure FillLongword(var X; Count: Integer; Value: Longword);
asm
// EAX = X
// EDX = Count
// ECX = Value
        PUSH    EDI

        MOV     EDI,EAX  // Point EDI to destination
        MOV     EAX,ECX
        MOV     ECX,EDX
        TEST    ECX,ECX
        JS      @exit

        REP     STOSD    // Fill count dwords
@exit:
        POP     EDI
end;

procedure MoveLongword(const Source; var Dest; Count: Integer);
asm
// EAX = Source
// EDX = Dest
// ECX = Count
        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX
        MOV     EDI,EDX
        MOV     EAX,ECX
        CMP     EDI,ESI
        JE      @exit

        REP     MOVSD
@exit:
        POP     EDI
        POP     ESI
end;

procedure Swap(var A, B: Integer);
asm
// EAX = [A]
// EDX = [B]
        MOV     ECX,[EAX]     // ECX := [A]
        XCHG    ECX,[EDX]     // ECX <> [B];
        MOV     [EAX],ECX     // [A] := ECX
end;

procedure TestSwap(var A, B: Integer);
asm
// EAX = [A]
// EDX = [B]
        MOV     ECX,[EAX]     // ECX := [A]
        CMP     ECX,[EDX]     
        JLE     @exit        // ECX <= [B]? Exit
        XCHG    ECX,[EDX]     // ECX <-> [B];
        MOV     [EAX],ECX     // [A] := ECX
@exit:
end;

function TestClip(var A, B: Integer; Size: Integer): Boolean;
begin
  TestSwap(A, B); // now A = min(A,B) and B = max(A, B)
  if A < 0 then A := 0;
  if B >= Size then B := Size - 1;
  Result := B >= A;
end;

function Constrain(Value, Lo, Hi: Integer): Integer;
begin
  if Value < Lo then Result := Lo
  else if Value > Hi then Result := Hi
  else Result := Value;
end;

{ shift right with sign conservation }
function SAR_4(Value: Integer): Integer;
asm
        SAR EAX,4
end;

function SAR_8(Value: Integer): Integer;
asm
        SAR EAX,8
end;

function SAR_9(Value: Integer): Integer;
asm
        SAR EAX,9
end;

function SAR_12(Value: Integer): Integer;
asm
        SAR EAX,12
end;

function SAR_13(Value: Integer): Integer;
asm
        SAR EAX,13
end;

function SAR_14(Value: Integer): Integer;
asm
        SAR EAX,14
end;

function SAR_16(Value: Integer): Integer;
asm
        SAR EAX,16
end;

{ Colorswap exchanges ARGB <-> ABGR and fill A with $FF }
function ColorSwap(WinColor: TColor): TColor32;
asm
// EAX = WinColor
// this function swaps R and B bytes in ABGR
// and writes $FF into A component
        BSWAP   EAX
        MOV     AL, $FF
        ROR     EAX,8
end;

function MulDiv(Multiplicand, Multiplier, Divisor: Integer): Integer;
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

        SUB     ECX,EDX         // Check if the result must be corregized by adding or substracting
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
end;


end.
 
