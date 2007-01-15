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
 * Portions created by the Initial Developer are Copyright (C) 2000-2007
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Michael Hansen <dyster_tid@hotmail.com>
 *   Andre Beckedorf <Andre@metaException.de>
 *   Mattias Andersson <mattias@centaurix.com>
 *
 * ***** END LICENSE BLOCK ***** *)

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
function Clamp(const Value: Integer): Integer; overload; {$IFDEF USEINLINING} inline; {$ENDIF}

{ An analogue of FillChar for 32 bit values }
var
  FillLongword: procedure(var X; Count: Integer; Value: Longword);

procedure FillWord(var X; Count: Integer; Value: Longword);

{ An analogue of Move for 32 bit values }
{$IFDEF USEMOVE}
procedure MoveLongword(const Source; var Dest; Count: Integer); {$IFDEF USEINLINING} inline; {$ENDIF}
{$ELSE}
procedure MoveLongword(const Source; var Dest; Count: Integer);
{$ENDIF}
procedure MoveWord(const Source; var Dest; Count: Integer);

{ Allocates a 'small' block of memory on the stack }
function StackAlloc(Size: Integer): Pointer; register;

{ Pops memory allocated by StackAlloc }
procedure StackFree(P: Pointer); register;

{ Exchange two 32-bit values }
procedure Swap(var A, B: Integer);

{ Exchange A <-> B only if B < A }
procedure TestSwap(var A, B: Integer);

{ Exchange A <-> B only if B < A then restrict both to [0..Size-1] range }
{ returns true if resulting range has common points with [0..Size-1] range }
function TestClip(var A, B: Integer; const Size: Integer): Boolean; overload;
function TestClip(var A, B: Integer; const Start, Stop: Integer): Boolean; overload;

{ Returns Value constrained to [Lo..Hi] range}
function Constrain(const Value, Lo, Hi: Integer): Integer; {$IFDEF USEINLINING} inline; {$ENDIF} overload;
function Constrain(const Value, Lo, Hi: Single): Single; {$IFDEF USEINLINING} inline; {$ENDIF} overload;

{ Returns Value constrained to [min(Constrain1, Constrain2)..max(Constrain1, Constrain2] range}
function SwapConstrain(const Value: Integer; Constrain1, Constrain2: Integer): Integer;

{ Clamp integer Value to [0..Max] range }
function Clamp(Value, Max: Integer): Integer; overload;
{ Same but [Min..Max] range }
function Clamp(Value, Min, Max: Integer): Integer; overload;

{ Wrap integer Value to [0..Max] range }
function Wrap(Value, Max: Integer): Integer; overload;
{ Same but [Min..Max] range }
function Wrap(Value, Min, Max: Integer): Integer; overload;

{ Mirror integer Value in [0..Max] range }
function Mirror(Value, Max: Integer): Integer; overload;
{ Same but [Min..Max] range }
function Mirror(Value, Min, Max: Integer): Integer; overload;

type
  TWrapProc = function(Value, Max: Integer): Integer;
  TWrapProcEx = function(Value, Min, Max: Integer): Integer;

const
  WRAP_PROCS: array[TWrapMode] of TWrapProc = (Clamp, Wrap, Mirror);
  WRAP_PROCS_EX: array[TWrapMode] of TWrapProcEx = (Clamp, Wrap, Mirror);

{ shift right with sign conservation }
function SAR_4(Value: Integer): Integer;
function SAR_8(Value: Integer): Integer;
function SAR_9(Value: Integer): Integer;
function SAR_11(Value: Integer): Integer;
function SAR_12(Value: Integer): Integer;
function SAR_13(Value: Integer): Integer;
function SAR_14(Value: Integer): Integer;
function SAR_15(Value: Integer): Integer;
function SAR_16(Value: Integer): Integer;

{ ColorSwap exchanges ARGB <-> ABGR and fills A with $FF }
function ColorSwap(WinColor: TColor): TColor32;


implementation

uses
  Math, GR32_System;

{$R-}{$Q-}  // switch off overflow and range checking

function Clamp(const Value: Integer): Integer;
{$IFDEF USEINLINING}
begin
  if Value > 255 then Result := 255
  else if Value < 0 then Result := 0
  else Result := Value;
{$ELSE}
asm
        TEST    EAX,$FFFFFF00
        JNZ     @1
        RET
@1:     JS      @2
        MOV     EAX,$FF
        RET
@2:     XOR     EAX,EAX
{$ENDIF}
end;

procedure _FillLongword(var X; Count: Integer; Value: Longword);
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


procedure M_FillLongword(var X; Count: Integer; Value: Longword);
asm
// EAX = X
// EDX = Count
// ECX = Value
        CMP        EDX, 0
        JBE        @Exit

        PUSH       EDI
        PUSH       EBX
        MOV        EBX, EDX
        MOV        EDI, EDX

        SHR        EDI, 1
        SHL        EDI, 1
        SUB        EBX, EDI
        JE         @QLoopIni

        MOV        [EAX], ECX
        ADD        EAX, 4
        DEC        EDX
        JZ         @ExitPOP
    @QLoopIni:
        db $0F,$6E,$C9           /// MOVD       MM1, ECX
        db $0F,$62,$C9           /// PUNPCKLDQ  MM1, MM1
        SHR        EDX, 1
    @QLoop:
        db $0F,$7F,$08           /// MOVQ       [EAX], MM1
        ADD        EAX, 8
        DEC        EDX
        JNZ        @QLoop
        db $0F,$77               /// EMMS
    @ExitPOP:
        POP        EBX
        POP        EDI
    @Exit:
end;

procedure FillWord(var X; Count: Integer; Value: Longword);
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

        REP     STOSW    // Fill count words
@exit:
        POP     EDI
end;

{$IFDEF USEMOVE}
procedure MoveLongword(const Source; var Dest; Count: Integer);
begin
  Move(Source, Dest, Count shl 2);
end;
{$ELSE}
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
{$ENDIF}

procedure MoveWord(const Source; var Dest; Count: Integer);
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

        REP     MOVSW
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

function TestClip(var A, B: Integer; const Size: Integer): Boolean;
begin
  TestSwap(A, B); // now A = min(A,B) and B = max(A, B)
  if A < 0 then A := 0;
  if B >= Size then B := Size - 1;
  Result := B >= A;
end;

function TestClip(var A, B: Integer; const Start, Stop: Integer): Boolean;
begin
  TestSwap(A, B); // now A = min(A,B) and B = max(A, B)
  if A < Start then A := Start;
  if B >= Stop then B := Stop - 1;
  Result := B >= A;
end;

function Constrain(const Value, Lo, Hi: Integer): Integer;
{$IFDEF USEINLINING}
begin
  if Value < Lo then Result := Lo
  else if Value > Hi then Result := Hi
  else Result := Value;
{$ELSE}
asm
        CMP       EDX,EAX
        db $0F,$4F,$C2           /// CMOVG     EAX,EDX
        CMP       ECX,EAX
        db $0F,$4C,$C1           /// CMOVL     EAX,ECX
{$ENDIF}
end;

function Constrain(const Value, Lo, Hi: Single): Single; {$IFDEF USEINLINING} inline; {$ENDIF} overload;
begin
  if Value < Lo then Result := Lo
  else if Value > Hi then Result := Hi
  else Result := Value;
end;

function SwapConstrain(const Value: Integer; Constrain1, Constrain2: Integer): Integer;
begin
  TestSwap(Constrain1, Constrain2);
  if Value < Constrain1 then Result := Constrain1
  else if Value > Constrain2 then Result := Constrain2
  else Result := Value;
end;

function Clamp(Value, Max: Integer): Integer;
asm
        CMP     EAX,EDX
        JG      @@above
        TEST    EAX,EAX
        JL      @@below
        RET
@@above:
        MOV     EAX,EDX
        RET
@@below:
        MOV     EAX,0
        RET
end;

function Clamp(Value, Min, Max: Integer): Integer;
asm
        CMP       EDX,EAX
        db $0F,$4F,$C2           /// CMOVG     EAX,EDX
        CMP       ECX,EAX
        db $0F,$4C,$C1           /// CMOVL     EAX,ECX
end;

function Wrap(Value, Max: Integer): Integer;
asm
        LEA     ECX,[EDX+1]
        CDQ
        IDIV    ECX
        MOV     EAX,EDX
        TEST    EAX,EAX
        JNL     @@exit
        ADD     EAX,ECX
@@exit:
end;

function Wrap(Value, Min, Max: Integer): Integer;
begin
  if Value < Min then
    Result := Max + (Value - Max) mod (Max - Min + 1)
  else
    Result := Min + (Value - Min) mod (Max - Min + 1);
end;

(*
asm
        CMP     EAX,EDX
        JL      @@below

        SUB     EAX,ECX
        SUB     ECX,EDX
        DEC     ECX
        CDQ
        IDIV    ECX


        RET
@@below:

        SUB     EAX,EDX
        NEG     EDX
        LEA     ECX,[ECX-EDX+1]
        CDQ
        IDIV    ECX
        MOV     EAX,EDX
        TEST    EAX,EAX
        JNL     @@exit
        ADD     EAX,ECX
@@exit:
end;*)

function Mirror(Value, Max: Integer): Integer;
asm
        TEST    EAX,EAX
        JNL     @@1
        NEG     EAX
@@1:
        MOV     ECX,EDX
        CDQ
        IDIV    ECX
        TEST    EAX,1
        MOV     EAX,EDX
        JZ      @@exit
        NEG     EAX
        ADD     EAX,ECX
@@exit:
end;


function DivMod(Dividend, Divisor: Integer; out Remainder: Integer): Integer;
asm
        PUSH EBX
        MOV EBX,EDX
        CDQ
        IDIV EBX
        MOV [ECX],EDX
        POP EBX
end;

function Mirror(Value, Min, Max: Integer): Integer;
var
  DivResult: Integer;
begin
  if Value < Min then
  begin
    DivResult := DivMod(Value - Max, Max - Min + 1, Result);
    Inc(Result, Max);
  end
  else
  begin
    DivResult := DivMod(Value - Min, Max - Min + 1, Result);
    Inc(Result, Min);
  end;
  if Odd(DivResult) then Result := Max+Min-Result;
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

function SAR_11(Value: Integer): Integer;
asm
        SAR EAX,11
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

function SAR_15(Value: Integer): Integer;
asm
        SAR EAX,15
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

{ StackAlloc allocates a 'small' block of memory from the stack by
  decrementing SP.  This provides the allocation speed of a local variable,
  but the runtime size flexibility of heap allocated memory.  }
function StackAlloc(Size: Integer): Pointer; register;
asm
  POP   ECX          { return address }
  MOV   EDX, ESP
  ADD   EAX, 3
  AND   EAX, not 3   // round up to keep ESP dword aligned
  CMP   EAX, 4092
  JLE   @@2
@@1:
  SUB   ESP, 4092
  PUSH  EAX          { make sure we touch guard page, to grow stack }
  SUB   EAX, 4096
  JNS   @@1
  ADD   EAX, 4096
@@2:
  SUB   ESP, EAX
  MOV   EAX, ESP     { function result = low memory address of block }
  PUSH  EDX          { save original SP, for cleanup }
  MOV   EDX, ESP
  SUB   EDX, 4
  PUSH  EDX          { save current SP, for sanity check  (sp = [sp]) }
  PUSH  ECX          { return to caller }
end;

{ StackFree pops the memory allocated by StackAlloc off the stack.
- Calling StackFree is optional - SP will be restored when the calling routine
  exits, but it's a good idea to free the stack allocated memory ASAP anyway.
- StackFree must be called in the same stack context as StackAlloc - not in
  a subroutine or finally block.
- Multiple StackFree calls must occur in reverse order of their corresponding
  StackAlloc calls.
- Built-in sanity checks guarantee that an improper call to StackFree will not
  corrupt the stack. Worst case is that the stack block is not released until
  the calling routine exits. }
procedure StackFree(P: Pointer); register;
asm
  POP   ECX                     { return address }
  MOV   EDX, DWORD PTR [ESP]
  SUB   EAX, 8
  CMP   EDX, ESP                { sanity check #1 (SP = [SP]) }
  JNE   @@1
  CMP   EDX, EAX                { sanity check #2 (P = this stack block) }
  JNE   @@1
  MOV   ESP, DWORD PTR [ESP+4]  { restore previous SP  }
@@1:
  PUSH  ECX                     { return to caller }
end;

procedure SetupFunctions;
begin
  if HasMMX then
    FillLongword := M_FillLongword
  else
    FillLongword := _FillLongword;
end;

initialization
  SetupFunctions;

end.

