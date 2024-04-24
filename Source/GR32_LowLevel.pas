unit GR32_LowLevel;

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
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2009
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

{$IFDEF PUREPASCAL}
  {$DEFINE USENATIVECODE}
  {$DEFINE USEMOVE}
{$ENDIF}
{$IFDEF USEINLINING}
  {$DEFINE USENATIVECODE}
{$ENDIF}

// Define WRAP_USEFLOATMOD to have Wrap(Single, Single) forward to FloatMod().
// If WRAP_USEFLOATMOD is not defined then an iterative algorithm is used which is
// very ineffective when the value is much out of bounds.
{$define WRAP_USEFLOATMOD}

uses
{$if defined(FRAMEWORK_FMX)}
  FMX.Graphics,
{$elseif defined(FRAMEWORK_VCL)}
  VCL.Graphics,
{$else}
  Graphics,
{$ifend}
  System.UITypes,
  GR32,
  GR32_Math,
  GR32_Bindings;


//------------------------------------------------------------------------------
//
//      Clamp function restricts value to [0..255] range
//
//------------------------------------------------------------------------------
function Clamp(const Value: Integer): Integer; overload; {$IFDEF USEINLINING} inline; {$ENDIF}


//------------------------------------------------------------------------------
//
//      FillLongword: An analogue of FillChar for 32 bit values
//
//------------------------------------------------------------------------------
var FillLongword: procedure(var X; Count: Cardinal; Value: Longword);

procedure FillWord(var X; Count: Cardinal; Value: Longword);


//------------------------------------------------------------------------------
//
//      MoveLongword: An analogue of Move optimized for 32 bit values
//      MoveWord: An analogue of Move optimized for 16 bit values
//
//------------------------------------------------------------------------------
{$IFDEF USEMOVE}
procedure MoveLongword(const Source; var Dest; Count: Integer); {$IFDEF USEINLINING} inline; {$ENDIF}
{$ELSE}
procedure MoveLongword(const Source; var Dest; Count: Integer);
{$ENDIF}
procedure MoveWord(const Source; var Dest; Count: Integer);


//------------------------------------------------------------------------------
//
//      StackAlloc: Allocates a 'small' block of memory on the stack
//
//------------------------------------------------------------------------------
{$IFDEF USESTACKALLOC}
function StackAlloc(Size: Integer): Pointer; register;

// Pops memory allocated by StackAlloc
procedure StackFree(P: Pointer); register;
{$ENDIF}


//------------------------------------------------------------------------------
//
//      Swap: Exchange values
//
//------------------------------------------------------------------------------
// Exchange two 32-bit values (except Swap(pointer, pointer))
procedure Swap(var A, B: Pointer); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
procedure Swap(var A, B: Integer); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
procedure Swap(var A, B: TFixed); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
procedure Swap(var A, B: TColor32); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
procedure Swap32(var A, B); overload; {$IFDEF USEINLINING} inline; {$ENDIF}

// Convert little-endian <-> big-endian
function Swap16(Value: Word): Word; {$IFDEF USENATIVECODE} inline; {$ENDIF}
function Swap32(Value: Cardinal): Cardinal; overload; {$IFDEF PUREPASCAL} inline; {$ENDIF}
function Swap64(Value: Int64): Int64; {$IFDEF USEINLINING} inline; {$ENDIF}

// Exchange A <-> B only if B < A
procedure TestSwap(var A, B: Integer); overload; {$IFDEF USEINLINING} inline; {$ENDIF}
procedure TestSwap(var A, B: TFixed); overload; {$IFDEF USEINLINING} inline; {$ENDIF}

// Exchange A <-> B only if B < A then restrict both to [0..Size-1] range.
// Returns true if resulting range has common points with [0..Size-1] range.
function TestClip(var A, B: Integer; const Size: Integer): Boolean; overload;
function TestClip(var A, B: Integer; const Start, Stop: Integer): Boolean; overload;


//------------------------------------------------------------------------------
//
//      Min/Max: Returns min./max. value of A, B and C
//
//------------------------------------------------------------------------------
function Min(const A, B, C: Integer): Integer; overload; {$IFDEF USENATIVECODE} inline; {$ENDIF}
function Max(const A, B, C: Integer): Integer; overload; {$IFDEF USENATIVECODE} inline; {$ENDIF}


//------------------------------------------------------------------------------
//
//      Constrain, Clamp: Constrain value to range
//
//------------------------------------------------------------------------------
// Return value constrained to [Lo..Hi] range
function Constrain(const Value, Lo, Hi: Integer): Integer; overload; {$IFDEF USENATIVECODE} inline; {$ENDIF}
function Constrain(const Value, Lo, Hi: Single): Single; overload; {$IFDEF USEINLINING} inline; {$ENDIF}

// Returns value constrained to [min(Constrain1, Constrain2)..max(Constrain1, Constrain2] range
function SwapConstrain(const Value: Integer; Constrain1, Constrain2: Integer): Integer;

// Clamp integer value to [0..Max] range
function Clamp(Value, Max: Integer): Integer; overload; {$IFDEF USENATIVECODE} inline; {$ENDIF}
// Clamp integer value to [Min..Max] range. Same as Constrain with same parameters.
function Clamp(Value, Min, Max: Integer): Integer; overload; {$IFDEF USENATIVECODE} inline; {$ENDIF}


//------------------------------------------------------------------------------
//
//      Wrap: Constrain value to range with wrap around
//
//------------------------------------------------------------------------------
// Wrap integer value to [0..Max] range
function Wrap(Value, Max: Integer): Integer; overload; {$IFDEF USENATIVECODE} inline; {$ENDIF}
// Same but [Min..Max] range. Min is assumed to be <= Max
function Wrap(Value, Min, Max: Integer): Integer; overload; {$IFDEF USEINLINING} inline; {$ENDIF}

{ Wrap single value to [0..Max) range.
  Basically the same as FloatMod except:
  - The upper limit is expected to always be positive.
  - If Max=0,, then 0 is returned.
  Unlike the integer version of Wrap, the upper limit is exclusive.
  NAN is not checked. If Max=0, Zero is returned. }
function Wrap(Value, Max: Single): Single; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
// Same as Wrap above but Value is by ref and Max is an integer
procedure WrapMem(var Value: Single; Max: Cardinal); {$IFDEF USEINLINING} inline; {$ENDIF}

// Fast Wrap alternatives for cases where range + 1 is a power of two
function WrapPow2(Value, Max: Integer): Integer; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function WrapPow2(Value, Min, Max: Integer): Integer; overload; {$IFDEF USEINLINING} inline; {$ENDIF}


//------------------------------------------------------------------------------
//
//      Mirror: Constrain value to range with mirroring
//
//------------------------------------------------------------------------------
// Mirror integer value in [0..Max] range
function Mirror(Value, Max: Integer): Integer; overload;
// Mirror integer value in [Min..Max] range
function Mirror(Value, Min, Max: Integer): Integer; overload;

// Fast Mirror alternatives for cases where range + 1 is a power of two
function MirrorPow2(Value, Max: Integer): Integer; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function MirrorPow2(Value, Min, Max: Integer): Integer; overload; {$IFDEF USEINLINING} inline; {$ENDIF}


//------------------------------------------------------------------------------
//
//      Clamp/Wrap/Mirror
//
//------------------------------------------------------------------------------
// Functions to determine appropiate wrap procs (normal or power of 2 optimized)
function GetOptimalWrap(Max: Integer): TWrapProc; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function GetOptimalWrap(Min, Max: Integer): TWrapProcEx; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function GetOptimalMirror(Max: Integer): TWrapProc; overload; {$IFDEF USEINLINING} inline; {$ENDIF}
function GetOptimalMirror(Min, Max: Integer): TWrapProcEx; overload; {$IFDEF USEINLINING} inline; {$ENDIF}

// Functions to retrieve correct WrapProc given WrapMode (and range) }
function GetWrapProc(WrapMode: TWrapMode): TWrapProc; overload;
function GetWrapProc(WrapMode: TWrapMode; Max: Integer): TWrapProc; overload;
function GetWrapProcEx(WrapMode: TWrapMode): TWrapProcEx; overload;
function GetWrapProcEx(WrapMode: TWrapMode; Min, Max: Integer): TWrapProcEx; overload;


const
  WRAP_PROCS: array[TWrapMode] of TWrapProc = (Clamp, Wrap, Mirror);
  WRAP_PROCS_EX: array[TWrapMode] of TWrapProcEx = (Clamp, Wrap, Mirror);


//------------------------------------------------------------------------------
//
//      Div255: Fast integer division by 255 with limited range
//
//------------------------------------------------------------------------------
// Fast integer division by 255.
// Valid for the range [0..$ffff]
function Div255(Value: Word): Word; {$IFDEF USEINLINING} inline; {$ENDIF}

// Possibly even faster integer division by 255.
// Valid for the range [0..255*255] }
function FastDiv255(Value: Word): Word; experimental; {$IFDEF USEINLINING} inline; {$ENDIF}

// Fast rounded integer division by 255.
// Valid for the range [0..255*255]
function Div255Round(Value: Word): Word; experimental; {$IFDEF USEINLINING} inline; {$ENDIF}


//------------------------------------------------------------------------------
//
//      FastRound & FastTrunc: Fast alternatives to the RTL Round & Trunc
//
//------------------------------------------------------------------------------
type
  TFastRoundProc = function(Value: TFloat): Integer;

var
  // Trunc and Round using SSE
  FastTrunc: TFastRoundProc experimental;

  FastRound: TFastRoundProc experimental;


//------------------------------------------------------------------------------
//
//      SAR: Shift right with sign conservation
//
//------------------------------------------------------------------------------
// Note that for PUREPASCAL SAR_n(x) is implemented as (x div 2^n).
// This works for positive values but not for negative values as both Delphi and FPC
// compiles (x div 2^n) to:
//
//   ADD EAX, $00007FFF
//   TEST EAX, EAX
//   JNS :positive
//   ADD EAX, $0000FFFF
//   :positive
//   SAR EAX, n
//
function SAR_3(Value: Integer): Integer; {$IFDEF PUREPASCAL} inline; {$ENDIF}
function SAR_4(Value: Integer): Integer; {$IFDEF PUREPASCAL} inline; {$ENDIF}
function SAR_6(Value: Integer): Integer; {$IFDEF PUREPASCAL} inline; {$ENDIF}
function SAR_8(Value: Integer): Integer; {$IFDEF PUREPASCAL} inline; {$ENDIF}
function SAR_9(Value: Integer): Integer; {$IFDEF PUREPASCAL} inline; {$ENDIF}
function SAR_11(Value: Integer): Integer; {$IFDEF PUREPASCAL} inline; {$ENDIF}
function SAR_12(Value: Integer): Integer; {$IFDEF PUREPASCAL} inline; {$ENDIF}
function SAR_13(Value: Integer): Integer; {$IFDEF PUREPASCAL} inline; {$ENDIF}
function SAR_14(Value: Integer): Integer; {$IFDEF PUREPASCAL} inline; {$ENDIF}
function SAR_15(Value: Integer): Integer; {$IFDEF PUREPASCAL} inline; {$ENDIF}
function SAR_16(Value: Integer): Integer; {$IFDEF PUREPASCAL} inline; {$ENDIF}


//------------------------------------------------------------------------------
//
//      ColorSwap exchanges ARGB <-> ABGR and fills A with $FF
//
//------------------------------------------------------------------------------
function ColorSwap(WinColor: TColor): TColor32;


//------------------------------------------------------------------------------
//
//      Bindings
//
//------------------------------------------------------------------------------
var
  LowLevelRegistry: TFunctionRegistry;

const
  FID_FILLLONGWORD      = 0;
  FID_FAST_TRUNC        = 1;
  FID_FAST_ROUND        = 2;

const
  LowLevelBindingFlagPascal = $0001;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
{$IFDEF FPC}
  SysUtils,
{$ENDIF}
  GR32_System;

{$R-}{$Q-}  // switch off overflow and range checking

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

const
  // SSE MXCSR rounding modes
  MXCSR_ROUND_MASK    = $FFFF9FFF;
  MXCSR_ROUND_NEAREST = $00000000;
  MXCSR_ROUND_DOWN    = $00002000;
  MXCSR_ROUND_UP      = $00004000;
  MXCSR_ROUND_TRUNC   = $00006000;
{$ENDIF}

//------------------------------------------------------------------------------
//
//      Clamp
//
//------------------------------------------------------------------------------
function Clamp(const Value: Integer): Integer;
{$IFDEF USENATIVECODE}
begin
 Result := Value;
 if Result > 255 then
   Result := 255
 else
 if Result < 0 then
   Result := 0;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x64}
        // in x64 calling convention parameters are passed in ECX, EDX, R8 & R9
        MOV     EAX,ECX
{$ENDIF}
        TEST    EAX,$FFFFFF00
        JNZ     @1
        RET
@1:     JS      @2
        MOV     EAX,$FF
        RET
@2:     XOR     EAX,EAX
{$ENDIF}
end;


//------------------------------------------------------------------------------
//
//      FillLongword
//
//------------------------------------------------------------------------------
procedure FillLongword_Pas(var X; Count: Cardinal; Value: Longword);
var
  I: Integer;
  P: PIntegerArray;
begin
  P := PIntegerArray(@X);
  for I := Count - 1 downto 0 do
    P[I] := Integer(Value);
end;

{$IFNDEF PUREPASCAL}
procedure FillLongword_ASM(var X; Count: Cardinal; Value: Longword); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
        // EAX = X;   EDX = Count;   ECX = Value
        PUSH    EDI

        MOV     EDI,EAX  // Point EDI to destination
        MOV     EAX,ECX
        MOV     ECX,EDX

        REP     STOSD    // Fill count dwords
@Exit:
        POP     EDI
{$ENDIF}
{$IFDEF TARGET_x64}
        // ECX = X;   EDX = Count;   R8 = Value
        PUSH    RDI

        MOV     RDI,RCX  // Point EDI to destination
        MOV     RAX,R8   // copy value from R8 to RAX (EAX)
        MOV     ECX,EDX  // copy count to ECX
        TEST    ECX,ECX
        JS      @Exit

        REP     STOSD    // Fill count dwords
@Exit:
        POP     RDI
{$ENDIF}
end;

procedure FillLongword_MMX(var X; Count: Cardinal; Value: Longword); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
        // EAX = X;   EDX = Count;   ECX = Value
        TEST       EDX, EDX   // if Count = 0 then
        JZ         @Exit      //   Exit

        PUSH       EDI
        MOV        EDI, EAX
        MOV        EAX, EDX

        SHR        EAX, 1
        SHL        EAX, 1
        SUB        EAX, EDX
        JE         @QLoopIni

        MOV        [EDI], ECX
        ADD        EDI, 4
        DEC        EDX
        JZ         @ExitPOP
    @QLoopIni:
        MOVD       MM1, ECX
        PUNPCKLDQ  MM1, MM1
        SHR        EDX, 1
    @QLoop:
        MOVQ       [EDI], MM1
        ADD        EDI, 8
        DEC        EDX
        JNZ        @QLoop
        EMMS
    @ExitPOP:
        POP        EDI
    @Exit:
{$ENDIF}
{$IFDEF TARGET_x64}
        // RCX = X;   RDX = Count;   R8 = Value
        TEST       RDX, RDX   // if Count = 0 then
        JZ         @Exit      //   Exit
        MOV        RAX, RCX   // RAX = X

        PUSH       RDI        // store RDI on stack
        MOV        R9, RDX    // R9 = Count
        MOV        RDI, RDX   // RDI = Count

        SHR        RDI, 1     // RDI = RDI SHR 1
        SHL        RDI, 1     // RDI = RDI SHL 1
        SUB        R9, RDI    // check if extra fill is necessary
        JE         @QLoopIni

        MOV        [RAX], R8D // eventually perform extra fill
        ADD        RAX, 4     // Inc(X, 4)
        DEC        RDX        // Dec(Count)
        JZ         @ExitPOP   // if (Count = 0) then Exit
@QLoopIni:
        MOVD       MM0, R8D   // MM0 = R8D
        PUNPCKLDQ  MM0, MM0   // unpack MM0 register
        SHR        RDX, 1     // RDX = RDX div 2
@QLoop:
        MOVQ       QWORD PTR [RAX], MM0 // perform fill
        ADD        RAX, 8     // Inc(X, 8)
        DEC        RDX        // Dec(X);
        JNZ        @QLoop
        EMMS
@ExitPOP:
        POP        RDI
@Exit:
{$ENDIF}
end;

procedure FillLongword_SSE2(var X; Count: Integer; Value: Longword); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
        // EAX = X;   EDX = Count;   ECX = Value

        TEST       EDX, EDX        // if Count = 0 then
        JZ         @Exit           //   Exit

        PUSH       EDI             // push EDI on stack
        MOV        EDI, EAX        // Point EDI to destination

        CMP        EDX, 32
        JL         @SmallLoop

        AND        EAX, 3          // get aligned count
        TEST       EAX, EAX        // check if X is not dividable by 4
        JNZ        @SmallLoop      // otherwise perform slow small loop

        MOV        EAX, EDI
        SHR        EAX, 2          // bytes to count
        AND        EAX, 3          // get aligned count
        ADD        EAX,-4
        NEG        EAX             // get count to advance
        JZ         @SetupMain
        SUB        EDX, EAX        // subtract aligning start from total count

@AligningLoop:
        MOV        [EDI], ECX
        ADD        EDI, 4
        DEC        EAX
        JNZ        @AligningLoop

@SetupMain:
        MOV        EAX, EDX        // EAX = remaining count
        SHR        EAX, 2
        SHL        EAX, 2
        SUB        EDX, EAX        // EDX = remaining count
        SHR        EAX, 2

        MOVD       XMM0, ECX
        PUNPCKLDQ  XMM0, XMM0
        PUNPCKLDQ  XMM0, XMM0
@SSE2Loop:
        MOVDQA     [EDI], XMM0
        ADD        EDI, 16
        DEC        EAX
        JNZ        @SSE2Loop

@SmallLoop:
        MOV        EAX,ECX
        MOV        ECX,EDX

        REP        STOSD           // Fill count dwords

@ExitPOP:
        POP        EDI

@Exit:
{$ENDIF}

{$IFDEF TARGET_x64}
        // RCX = X;   RDX = Count;   R8 = Value

        TEST       RDX, RDX        // if Count = 0 then
        JZ         @Exit           //   Exit

        MOV        R9, RCX         // Point R9 to destination

        CMP        RDX, 32
        JL         @SmallLoop

        AND        RCX, 3          // get aligned count
        TEST       RCX, RCX        // check if X is not dividable by 4
        JNZ        @SmallLoop      // otherwise perform slow small loop

        MOV        RCX, R9
        SHR        RCX, 2          // bytes to count
        AND        RCX, 3          // get aligned count
        ADD        RCX,-4
        NEG        RCX             // get count to advance
        JZ         @SetupMain
        SUB        RDX, RCX        // subtract aligning start from total count

@AligningLoop:
        MOV        [R9], R8D
        ADD        R9, 4
        DEC        RCX
        JNZ        @AligningLoop

@SetupMain:
        MOV        RCX, RDX        // RCX = remaining count
        SHR        RCX, 2
        SHL        RCX, 2
        SUB        RDX, RCX        // RDX = remaining count
        SHR        RCX, 2

        MOVD       XMM0, R8D
        PUNPCKLDQ  XMM0, XMM0
        PUNPCKLDQ  XMM0, XMM0
@SSE2Loop:
        MOVDQA     [R9], XMM0
        ADD        R9, 16
        DEC        RCX
        JNZ        @SSE2Loop

        TEST       RDX, RDX
        JZ         @Exit
@SmallLoop:
        MOV        [R9], R8D
        ADD        R9, 4
        DEC        RDX
        JNZ        @SmallLoop
@Exit:
{$ENDIF}
end;
{$ENDIF}


//------------------------------------------------------------------------------
//
//      FillWord
//
//------------------------------------------------------------------------------
procedure FillWord(var X; Count: Cardinal; Value: LongWord);
{$IFDEF USENATIVECODE}
var
  I: Integer;
  P: PWordArray;
begin
  P := PWordArray(@X);
  for I := Count - 1 downto 0 do
    P[I] := Value;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
        // EAX = X;   EDX = Count;   ECX = Value
        PUSH    EDI

        MOV     EDI,EAX  // Point EDI to destination
        MOV     EAX,ECX
        MOV     ECX,EDX
        TEST    ECX,ECX
        JZ      @exit

        REP     STOSW    // Fill count words
@exit:
        POP     EDI
{$ENDIF}

{$IFDEF TARGET_x64}
        // ECX = X;   EDX = Count;   R8D = Value
        PUSH    RDI

        MOV     RDI,RCX  // Point EDI to destination
        MOV     EAX,R8D
        MOV     ECX,EDX
        TEST    ECX,ECX
        JZ      @exit

        REP     STOSW    // Fill count words
@exit:
        POP     RDI
{$ENDIF}
{$ENDIF}
end;


//------------------------------------------------------------------------------
//
//      MoveLongword
//
//------------------------------------------------------------------------------
procedure MoveLongword(const Source; var Dest; Count: Integer);
{$IFDEF USEMOVE}
begin
  Move(Source, Dest, Count shl 2);
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
        // EAX = Source;   EDX = Dest;   ECX = Count
        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX
        MOV     EDI,EDX
        CMP     EDI,ESI
        JE      @exit

        REP     MOVSD
@exit:
        POP     EDI
        POP     ESI
{$ENDIF}

{$IFDEF TARGET_x64}
        // RCX = Source;   RDX = Dest;   R8 = Count
        PUSH    RSI
        PUSH    RDI

        MOV     RSI,RCX
        MOV     RDI,RDX
        MOV     RCX,R8
        CMP     RDI,RSI
        JE      @exit

        REP     MOVSD
@exit:
        POP     RDI
        POP     RSI
{$ENDIF}
{$ENDIF}
end;


//------------------------------------------------------------------------------
//
//      MoveWord
//
//------------------------------------------------------------------------------
procedure MoveWord(const Source; var Dest; Count: Integer);
{$IFDEF USEMOVE}
begin
  Move(Source, Dest, Count shl 1);
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
        // EAX = X;   EDX = Count;   ECX = Value
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
{$ENDIF}

{$IFDEF TARGET_x64}
        // RCX = Source;   RDX = Dest;   R8 = Count
        CMP     RCX,RDX
        JE      @exit

        TEST    R8,R8
        JZ      @exit

        PUSH    RSI
        PUSH    RDI

        MOV     RSI,RCX
        MOV     RDI,RDX
        MOV     RCX,R8

        REP     MOVSW

        POP     RDI
        POP     RSI
@exit:
{$ENDIF}
{$ENDIF}
end;


//------------------------------------------------------------------------------
//
//      Swap
//
//------------------------------------------------------------------------------
procedure Swap(var A, B: Pointer);
var
  T: Pointer;
begin
  T := A;
  A := B;
  B := T;
end;

//------------------------------------------------------------------------------

procedure Swap(var A, B: Integer);
var
  T: Integer;
begin
  T := A;
  A := B;
  B := T;
end;

//------------------------------------------------------------------------------

procedure Swap(var A, B: TFixed);
var
  T: TFixed;
begin
  T := A;
  A := B;
  B := T;
end;

//------------------------------------------------------------------------------

procedure Swap(var A, B: TColor32);
var
  T: TColor32;
begin
  T := A;
  A := B;
  B := T;
end;

//------------------------------------------------------------------------------

procedure Swap32(var A, B);
var
  T: Integer;
begin
  T := Integer(A);
  Integer(A) := Integer(B);
  Integer(B) := T;
end;

//------------------------------------------------------------------------------

function Swap16(Value: Word): Word;
{$IFDEF USENATIVECODE}
begin
  Result := System.Swap(Value);
{$ELSE}
asm
  {$IFDEF TARGET_x64}
  MOV     EAX, ECX
  {$ENDIF}
  XCHG    AL, AH
{$ENDIF}
end;

//------------------------------------------------------------------------------

function Swap32(Value: Cardinal): Cardinal;
{$IFDEF PUREPASCAL}
type
  TTwoWords = array [0..1] of Word;
begin
  TTwoWords(Result)[1] := System.Swap(TTwoWords(Value)[0]);
  TTwoWords(Result)[0] := System.Swap(TTwoWords(Value)[1]);
{$ELSE}
asm
  {$IFDEF TARGET_x64}
  MOV     EAX, ECX
  {$ENDIF}
  BSWAP   EAX
{$ENDIF}
end;

//------------------------------------------------------------------------------

function Swap64(Value: Int64): Int64;
type
  TFourWords = array [0..3] of Word;
begin
  TFourWords(Result)[3] := System.Swap(TFourWords(Value)[0]);
  TFourWords(Result)[2] := System.Swap(TFourWords(Value)[1]);
  TFourWords(Result)[1] := System.Swap(TFourWords(Value)[2]);
  TFourWords(Result)[0] := System.Swap(TFourWords(Value)[3]);
end;

//------------------------------------------------------------------------------

procedure TestSwap(var A, B: Integer);
var
  T: Integer;
begin
  if B < A then
  begin
    T := A;
    A := B;
    B := T;
  end;
end;

//------------------------------------------------------------------------------

procedure TestSwap(var A, B: TFixed);
var
  T: TFixed;
begin
  if B < A then
  begin
    T := A;
    A := B;
    B := T;
  end;
end;

//------------------------------------------------------------------------------

function TestClip(var A, B: Integer; const Size: Integer): Boolean;
begin
  TestSwap(A, B); // now A = min(A,B) and B = max(A, B)
  if A < 0 then
    A := 0;
  if B >= Size then
    B := Size - 1;
  Result := B >= A;
end;

//------------------------------------------------------------------------------

function TestClip(var A, B: Integer; const Start, Stop: Integer): Boolean;
begin
  TestSwap(A, B); // now A = min(A,B) and B = max(A, B)
  if A < Start then
    A := Start;
  if B >= Stop then
    B := Stop - 1;
  Result := B >= A;
end;


//------------------------------------------------------------------------------
//
//      Min/Max
//
//------------------------------------------------------------------------------
function Max(const A, B, C: Integer): Integer;
{$IFDEF USENATIVECODE}
begin
  if A > B then
    Result := A
  else
    Result := B;

  if C > Result then
    Result := C;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x64}
        MOV       RAX,RCX
        MOV       RCX,R8
{$ENDIF}
        CMP       EDX,EAX
        CMOVG     EAX,EDX
        CMP       ECX,EAX
        CMOVG     EAX,ECX
{$ENDIF}
end;

//------------------------------------------------------------------------------

function Min(const A, B, C: Integer): Integer;
{$IFDEF USENATIVECODE}
begin
  if A < B then
    Result := A
  else
    Result := B;

  if C < Result then
    Result := C;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x64}
        MOV       RAX,RCX
        MOV       RCX,R8
{$ENDIF}
        CMP       EDX,EAX
        CMOVL     EAX,EDX
        CMP       ECX,EAX
        CMOVL     EAX,ECX
{$ENDIF}
end;

//------------------------------------------------------------------------------
//
//      Constrain
//
//------------------------------------------------------------------------------
function Constrain(const Value, Lo, Hi: Integer): Integer;
{$IFDEF USENATIVECODE}
begin
  Result := Value;
  if Result < Lo then
    Result := Lo
  else
  if Result > Hi then
    Result := Hi;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x64}
        MOV       EAX,ECX
        MOV       ECX,R8D
{$ENDIF}
        CMP       EDX,EAX
        CMOVG     EAX,EDX
        CMP       ECX,EAX
        CMOVL     EAX,ECX
{$ENDIF}
end;

//------------------------------------------------------------------------------

function Constrain(const Value, Lo, Hi: Single): Single; overload;
begin
  Result := Value;
  if Result < Lo then
    Result := Lo
  else
  if Result > Hi then
    Result := Hi;
end;

//------------------------------------------------------------------------------

function SwapConstrain(const Value: Integer; Constrain1, Constrain2: Integer): Integer;
begin
  TestSwap(Constrain1, Constrain2);
  Result := Value;
  if Result < Constrain1 then
    Result := Constrain1
  else
  if Result > Constrain2 then
    Result := Constrain2;
end;


//------------------------------------------------------------------------------
//
//      Clamp
//
//------------------------------------------------------------------------------
function Clamp(Value, Max: Integer): Integer;
{$IFDEF USENATIVECODE}
begin
  Result := Value;
  if Result > Max then
    Result := Max
  else
  if Result < 0 then
    Result := 0;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x64}
        MOV     EAX,ECX
        MOV     ECX,R8D
{$ENDIF}
        CMP     EAX,EDX
        JG      @Above
        TEST    EAX,EAX
        JL      @Below
        RET
@Above:
        MOV     EAX,EDX
        RET
@Below:
        MOV     EAX,0
        RET
{$ENDIF}
end;

//------------------------------------------------------------------------------

function Clamp(Value, Min, Max: Integer): Integer;
{$IFDEF USENATIVECODE}
begin
  Result := Value;
  if Result > Max then
    Result := Max
  else
  if Result < Min then
    Result := Min;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x64}
        MOV     EAX,ECX
        MOV     ECX,R8D
{$ENDIF}
        CMP     EDX,EAX
        CMOVG   EAX,EDX
        CMP     ECX,EAX
        CMOVL   EAX,ECX
{$ENDIF}
end;


//------------------------------------------------------------------------------
//
//      Wrap
//
//------------------------------------------------------------------------------
function Wrap(Value, Max: Integer): Integer;
{$IFDEF USENATIVECODE}
begin
  Inc(Max);

  if (Value < 0) then
    Value := Value + Max * (-Value div Max + 1);

  Result := Value mod Max;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x64}
        MOV     EAX,ECX
        MOV     ECX,R8D
        LEA     ECX,[RDX+1]
{$ELSE}
        LEA     ECX,[EDX+1]
{$ENDIF}
        CDQ
        IDIV    ECX
        MOV     EAX,EDX
        TEST    EAX,EAX
        JNL     @Exit
        ADD     EAX,ECX
@Exit:
{$ENDIF}
end;

//------------------------------------------------------------------------------

function Wrap(Value, Min, Max: Integer): Integer;
var
  Range: integer;
begin
  Range := Max - Min + 1;

  if (Value < Min) then
    Value := Value + Range * ((Min - Value) div Range + 1);

  Result := Min + (Value - Min) mod Range;
end;

//------------------------------------------------------------------------------

function Wrap(Value, Max: Single): Single;
var
  Maxbin: Cardinal absolute Max;
begin
{$if defined(WRAP_USEFLOATMOD)}
  if (Maxbin shl 1 <> 0) then // Single=0 test trick
    Result := FloatMod(Value, Max)
  else
    Result := 0;
{$else}
  if Max = 0 then
  begin
    Result := 0;
    Exit;
  end;

  Result := Value;
  while Result >= Max do
    Result := Result - Max;
  while Result < 0 do
    Result := Result + Max;
{$ifend}
end;

//------------------------------------------------------------------------------

procedure WrapMem(var Value: Single; Max: Cardinal);
begin
{$if defined(WRAP_USEFLOATMOD)}
  if (Max <> 0) then
    Value := FloatMod(Value, Max)
  else
    Value := 0;
{$else}
  if Max = 0 then
  begin
    Value := 0;
    Exit;
  end;

  while Value >= Max do
    Value := Value - Max;
  while Value < 0 do
    Value := Value + Max;
{$ifend}
end;

//------------------------------------------------------------------------------

function DivMod(Dividend, Divisor: Integer; out Remainder: Integer): Integer;
{$IFDEF USENATIVECODE}
begin
  Remainder := Dividend mod Divisor;
  Result := Dividend div Divisor;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
        PUSH      EBX
        MOV       EBX,EDX
        CDQ
        IDIV      EBX
        MOV       [ECX],EDX
        POP       EBX
{$ENDIF}
{$IFDEF TARGET_x64}
        PUSH      RBX
        MOV       EAX,ECX
        MOV       ECX,R8D
        MOV       EBX,EDX
        CDQ
        IDIV      EBX
        MOV       [RCX],EDX
        POP       RBX
{$ENDIF}
{$ENDIF}
end;

//------------------------------------------------------------------------------

function WrapPow2(Value, Max: Integer): Integer; overload;
begin
  Result := Value and Max;
end;

//------------------------------------------------------------------------------

function WrapPow2(Value, Min, Max: Integer): Integer; overload;
begin
  Result := (Value - Min) and (Max - Min) + Min;
end;


//------------------------------------------------------------------------------
//
//      Mirror
//
//------------------------------------------------------------------------------
function Mirror(Value, Max: Integer): Integer;
{$IFDEF USENATIVECODE}
var
  DivResult: Integer;
begin
  if Value < 0 then
  begin
    DivResult := DivMod(Value - Max, Max + 1, Result);
    Inc(Result, Max);
  end
  else
    DivResult := DivMod(Value, Max + 1, Result);

  if Odd(DivResult) then
    Result := Max - Result;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x64}
        MOV       EAX,ECX
        MOV       ECX,R8D
{$ENDIF}
        TEST      EAX,EAX
        JNL       @@1
        NEG       EAX
@@1:
        MOV       ECX,EDX
        CDQ
        IDIV      ECX
        TEST      EAX,1
        MOV       EAX,EDX
        JZ        @Exit
        NEG       EAX
        ADD       EAX,ECX
@Exit:
{$ENDIF}
end;

//------------------------------------------------------------------------------

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
  if Odd(DivResult) then Result := Max + Min - Result;
end;

//------------------------------------------------------------------------------

function MirrorPow2(Value, Max: Integer): Integer; overload;
begin
  if Value and (Max + 1) = 0 then
    Result := Value and Max
  else
    Result := Max - Value and Max;
end;

//------------------------------------------------------------------------------

function MirrorPow2(Value, Min, Max: Integer): Integer; overload;
begin
  Value := Value - Min;
  Result := Max - Min;

  if Value and (Result + 1) = 0 then
    Result := Min + Value and Result
  else
    Result := Max - Value and Result;
end;


//------------------------------------------------------------------------------
//
//      Clamp/Wrap/Mirror
//
//------------------------------------------------------------------------------
function GetOptimalWrap(Max: Integer): TWrapProc; overload;
begin
  if (Max >= 0) and IsPowerOf2(Max + 1) then
    Result := WrapPow2
  else
    Result := Wrap;
end;

//------------------------------------------------------------------------------

function GetOptimalWrap(Min, Max: Integer): TWrapProcEx; overload;
begin
  if (Min >= 0) and (Max >= Min) and IsPowerOf2(Max - Min + 1) then
    Result := WrapPow2
  else
    Result := Wrap;
end;

//------------------------------------------------------------------------------

function GetOptimalMirror(Max: Integer): TWrapProc; overload;
begin
  if (Max >= 0) and IsPowerOf2(Max + 1) then
    Result := MirrorPow2
  else
    Result := Mirror;
end;

//------------------------------------------------------------------------------

function GetOptimalMirror(Min, Max: Integer): TWrapProcEx; overload;
begin
  if (Min >= 0) and (Max >= Min) and IsPowerOf2(Max - Min + 1) then
    Result := MirrorPow2
  else
    Result := Mirror;
end;

//------------------------------------------------------------------------------

function GetWrapProc(WrapMode: TWrapMode): TWrapProc; overload;
begin
  case WrapMode of
    wmRepeat:
      Result := Wrap;
    wmMirror:
      Result := Mirror;
    else //wmClamp:
      Result := Clamp;
  end;
end;

//------------------------------------------------------------------------------

function GetWrapProc(WrapMode: TWrapMode; Max: Integer): TWrapProc; overload;
begin
  case WrapMode of
    wmRepeat:
      Result := GetOptimalWrap(Max);
    wmMirror:
      Result := GetOptimalMirror(Max);
    else //wmClamp:
      Result := Clamp;
  end;
end;

//------------------------------------------------------------------------------

function GetWrapProcEx(WrapMode: TWrapMode): TWrapProcEx; overload;
begin
  case WrapMode of
    wmRepeat:
      Result := Wrap;
    wmMirror:
      Result := Mirror;
    else //wmClamp:
      Result := Clamp;
  end;
end;

//------------------------------------------------------------------------------

function GetWrapProcEx(WrapMode: TWrapMode; Min, Max: Integer): TWrapProcEx; overload;
begin
  case WrapMode of
    wmRepeat:
      Result := GetOptimalWrap(Min, Max);
    wmMirror:
      Result := GetOptimalMirror(Min, Max);
    else //wmClamp:
      Result := Clamp;
  end;
end;


//------------------------------------------------------------------------------
//
//      Div255: Fast integer division by 255 with limited range
//
//------------------------------------------------------------------------------
function Div255(Value: Word): Word;
begin
{$if (defined(FPC)) or ((CompilerVersion >= 36.0) and (defined(TARGET_x86)))}
  // Delphi 12, 32-bit, already knows how to optimize division by 255.
  // Unfortunately it always optimizes as if the argument is a signed 32-bit.
  Result := Value div 255;
{$else}
  // Input is 16 bit, intermediate result is 32-bit, result is 8 bit
  // Note: Algorithm doesn't take sign into account!
  Result := (Value * $8081) shr 23;
{$ifend}
end;

//------------------------------------------------------------------------------

function FastDiv255(Value: Word): Word;
begin
  // Input is 16 bit, intermediate result is 32-bit (25 used), result is 8 bit
  // Note: Algorithm doesn't take sign into account!
  Result := (Value + ((Value + 257) shr 8)) shr 8;
end;

//------------------------------------------------------------------------------

function Div255Round(Value: Word): Word;
begin
  // Input is 16 bit, intermediate result is 24-bit, result is 8 bit
  // Note: Algorithm doesn't take sign into account!
  Result := ((Value + 128) * 257) shr 16;
end;


//------------------------------------------------------------------------------
//
//      FastRound
//
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// FastRound_Pas
//------------------------------------------------------------------------------
function FastRound_Pas(Value: TFloat): Integer;
begin
  Result := Round(Value);
end;

{$IFNDEF PUREPASCAL}
//------------------------------------------------------------------------------
// FastRound_SSE41
//------------------------------------------------------------------------------
function FastRound_SSE41(Value: TFloat): Integer; {$IFDEF FPC} assembler; {$IFDEF TARGET_X64} nostackframe; {$ENDIF}{$ENDIF}
asm
{$if defined(TARGET_x86)}
        MOVSS   xmm0, Value
{$ifend}

        ROUNDSS xmm0, xmm0, ROUND_TO_NEAREST_INT or ROUND_NO_EXC

        CVTSS2SI eax, xmm0
end;
{$ENDIF}

//------------------------------------------------------------------------------
//
//      FastTrunc
//
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// FastTrunc_Pas
//------------------------------------------------------------------------------
//
// Just defer to RTL Trunc
//
function FastTrunc_Pas(Value: TFloat): Integer;
begin
  Result := Trunc(Value);
end;

{$IFNDEF PUREPASCAL}
//------------------------------------------------------------------------------
// FastTrunc_SSE2
//------------------------------------------------------------------------------
//
// Faster that RTL Trunc on x86 and x64
//
{$IFNDEF OMIT_SSE2}
function FastTrunc_SSE2(Value: TFloat): Integer; {$IFDEF FPC} assembler; {$IFDEF TARGET_X64} nostackframe; {$ENDIF}{$ENDIF}
asm
{$if defined(TARGET_x86)}
        MOVSS      XMM0, Value
{$ifend}

        CVTTSS2SI  EAX, XMM0
end;
{$ENDIF}

//------------------------------------------------------------------------------
// SlowTrunc_SSE2
//------------------------------------------------------------------------------
//
// Faster that RTL Trunc on x64 (and sometimes on x86).
//
{$IFNDEF OMIT_SSE2}
function SlowTrunc_SSE2(Value: TFloat): Integer; {$IFDEF FPC} assembler; {$ENDIF}
var
  SaveMXCSR: Cardinal;
  NewMXCSR: Cardinal;
asm
        XOR     ECX, ECX

        // Save current rounding mode
        STMXCSR SaveMXCSR
        // Load rounding mode
        MOV     EAX, SaveMXCSR
        // Do we need to change anything?
        MOV     ECX, EAX
        NOT     ECX
        AND     ECX, MXCSR_ROUND_TRUNC
        JZ      @SkipSetMXCSR // Skip expensive LDMXCSR
@SetMXCSR:
        // Save current rounding mode in ECX and flag that we need to restore it
        MOV     ECX, EAX
        // Set rounding mode to truncation
        AND     EAX, MXCSR_ROUND_MASK
        OR      EAX, MXCSR_ROUND_TRUNC
        // Set new rounding mode
        MOV     NewMXCSR, EAX
        LDMXCSR NewMXCSR
@SkipSetMXCSR:

{$if defined(TARGET_x86)}
        MOVSS   XMM0, Value
{$ifend}
        // Round/Trunc
        CVTSS2SI EAX, XMM0

        // Restore rounding mode
        // Did we modify it?
        TEST    ECX, ECX
        JZ      @SkipRestoreMXCSR // Skip expensive LDMXCSR
        // Restore old rounding mode
        LDMXCSR SaveMXCSR
@SkipRestoreMXCSR:
end;
{$ENDIF}

//------------------------------------------------------------------------------
// FastTrunc_SSE41
//------------------------------------------------------------------------------
//
// Faster that RTL Trunc on x86
//
{$IFNDEF OMIT_SSE2}
function FastTrunc_SSE41(Value: TFloat): Integer; {$IFDEF FPC} assembler; {$IFDEF TARGET_X64} nostackframe; {$ENDIF}{$ENDIF}
asm
{$if defined(TARGET_x86)}
        MOVSS   xmm0, Value
{$ifend}

        ROUNDSS xmm0, xmm0, ROUND_TO_ZERO or ROUND_NO_EXC
        CVTSS2SI eax, xmm0
end;
{$ENDIF}
{$ENDIF}

//------------------------------------------------------------------------------
//
//      SAR: Shift right with sign conservation
//
//------------------------------------------------------------------------------
function SAR_3(Value: Integer): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := Value div 8;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x64}
        MOV       EAX,ECX
{$ENDIF}
        SAR       EAX,3
{$ENDIF}
end;

//------------------------------------------------------------------------------

function SAR_4(Value: Integer): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := Value div 16;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x64}
        MOV       EAX,ECX
{$ENDIF}
        SAR       EAX,4
{$ENDIF}
end;

//------------------------------------------------------------------------------

function SAR_6(Value: Integer): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := Value div 64;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x64}
        MOV       EAX,ECX
{$ENDIF}
        SAR       EAX,6
{$ENDIF}
end;

//------------------------------------------------------------------------------

function SAR_8(Value: Integer): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := Value div 256;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x64}
        MOV       EAX,ECX
{$ENDIF}
        SAR       EAX,8
{$ENDIF}
end;

//------------------------------------------------------------------------------

function SAR_9(Value: Integer): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := Value div 512;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x64}
        MOV       EAX,ECX
{$ENDIF}
        SAR       EAX,9
{$ENDIF}
end;

//------------------------------------------------------------------------------

function SAR_11(Value: Integer): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := Value div 2048;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x64}
        MOV       EAX,ECX
{$ENDIF}
        SAR       EAX,11
{$ENDIF}
end;

//------------------------------------------------------------------------------

function SAR_12(Value: Integer): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := Value div 4096;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x64}
        MOV       EAX,ECX
{$ENDIF}
        SAR       EAX,12
{$ENDIF}
end;

//------------------------------------------------------------------------------

function SAR_13(Value: Integer): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := Value div 8192;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x64}
        MOV       EAX,ECX
{$ENDIF}
        SAR       EAX,13
{$ENDIF}
end;

//------------------------------------------------------------------------------

function SAR_14(Value: Integer): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := Value div 16384;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x64}
        MOV       EAX,ECX
{$ENDIF}
        SAR       EAX,14
{$ENDIF}
end;

//------------------------------------------------------------------------------

function SAR_15(Value: Integer): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := Value div 32768;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x64}
        MOV       EAX,ECX
{$ENDIF}
        SAR       EAX,15
{$ENDIF}
end;

//------------------------------------------------------------------------------

function SAR_16(Value: Integer): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := Value div 65536;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x64}
        MOV       EAX,ECX
{$ENDIF}
        SAR       EAX,16
{$ENDIF}
end;


//------------------------------------------------------------------------------
//
//      ColorSwap
//
//------------------------------------------------------------------------------
function ColorSwap(WinColor: TColor): TColor32;
{$IFDEF USENATIVECODE}
var
  WCEn: TColor32Entry absolute WinColor;
  REn : TColor32Entry absolute Result;
begin
  Result := WCEn.ARGB;
  REn.A := $FF;
  REn.R := WCEn.B;
  REn.B := WCEn.R;
{$ELSE}
{$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
// EAX = WinColor
// this function swaps R and B bytes in ABGR
// and writes $FF into A component
{$IFDEF TARGET_x64}
        MOV       EAX,ECX
{$ENDIF}
        BSWAP     EAX
        MOV       AL, $FF
        ROR       EAX,8
{$ENDIF}
end;


//------------------------------------------------------------------------------
//
//      StackAlloc
//
//------------------------------------------------------------------------------
{$IFDEF USESTACKALLOC}
{$IFDEF PUREPASCAL}

function StackAlloc(Size: Integer): Pointer;
begin
  GetMem(Result, Size);
end;

procedure StackFree(P: Pointer);
begin
  FreeMem(P);
end;

{$ELSE}

{ StackAlloc allocates a 'small' block of memory from the stack by
  decrementing SP.  This provides the allocation speed of a local variable,
  but the runtime size flexibility of heap allocated memory.

  x64 implementation by Jameel Halabi
  }
function StackAlloc(Size: Integer): Pointer; register; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
        POP       ECX          // return address
        MOV       EDX, ESP
        ADD       EAX, 3
        AND       EAX, not 3   // round up to keep ESP dword aligned
        CMP       EAX, 4092
        JLE       @@2
@@1:
        SUB       ESP, 4092
        PUSH      EAX          // make sure we touch guard page, to grow stack
        SUB       EAX, 4096
        JNS       @@1
        ADD       EAX, 4096
@@2:
        SUB       ESP, EAX
        MOV       EAX, ESP     // function result = low memory address of block
        PUSH      EDX          // save original SP, for cleanup
        MOV       EDX, ESP
        SUB       EDX, 4
        PUSH      EDX          // save current SP, for sanity check  (sp = [sp])
        PUSH      ECX          // return to caller
{$ENDIF}
{$IFDEF TARGET_x64}
        {$IFNDEF FPC}
        .NOFRAME
        {$ENDIF}
        POP       R8           // return address
        MOV       RDX, RSP     // original SP
        ADD       ECX, 15
        AND       ECX, NOT 15  // round up to keep SP dqword aligned
        CMP       ECX, 4088
        JLE       @@2
@@1:
        SUB       RSP, 4088
        PUSH      RCX          // make sure we touch guard page, to grow stack
        SUB       ECX, 4096
        JNS       @@1
        ADD       ECX, 4096
@@2:
        SUB       RSP, RCX
        MOV       RAX, RSP     // function result = low memory address of block
        PUSH      RDX          // save original SP, for cleanup
        MOV       RDX, RSP
        SUB       RDX, 8
        PUSH      RDX          // save current SP, for sanity check  (sp = [sp])
        PUSH      R8           // return to caller
{$ENDIF}
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
procedure StackFree(P: Pointer); register; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
        POP       ECX                     // return address
        MOV       EDX, DWORD PTR [ESP]
        SUB       EAX, 8
        CMP       EDX, ESP                // sanity check #1 (SP = [SP])
        JNE       @Exit
        CMP       EDX, EAX                // sanity check #2 (P = this stack block)
        JNE       @Exit
        MOV       ESP, DWORD PTR [ESP+4]  // restore previous SP
@Exit:
        PUSH      ECX                     // return to caller
{$ENDIF}
{$IFDEF TARGET_x64}
        {$IFNDEF FPC}
        .NOFRAME
        {$ENDIF}
        POP       R8                       // return address
        MOV       RDX, QWORD PTR [RSP]
        SUB       RCX, 16
        CMP       RDX, RSP                 // sanity check #1 (SP = [SP])
        JNE       @Exit
        CMP       RDX, RCX                 // sanity check #2 (P = this stack block)
        JNE       @Exit
        MOV       RSP, QWORD PTR [RSP + 8] // restore previous SP
 @Exit:
        PUSH      R8                       // return to caller
{$ENDIF}
end;
{$ENDIF}

{$ENDIF}


//------------------------------------------------------------------------------
//
//      Bindings
//
//------------------------------------------------------------------------------
procedure RegisterBindings;
begin
  {$WARN SYMBOL_EXPERIMENTAL OFF}

  LowLevelRegistry := NewRegistry('GR32_LowLevel bindings');
  LowLevelRegistry.RegisterBinding(FID_FILLLONGWORD, @@FillLongWord);
  LowLevelRegistry.RegisterBinding(FID_FAST_TRUNC, @@FastTrunc);
  LowLevelRegistry.RegisterBinding(FID_FAST_ROUND, @@FastRound);

  LowLevelRegistry.Add(FID_FILLLONGWORD, @FillLongWord_Pas, LowLevelBindingFlagPascal);
  LowLevelRegistry.Add(FID_FAST_TRUNC, @FastTrunc_Pas, LowLevelBindingFlagPascal);
  LowLevelRegistry.Add(FID_FAST_ROUND, @FastRound_Pas, LowLevelBindingFlagPascal);

{$IFNDEF PUREPASCAL}
  LowLevelRegistry.Add(FID_FILLLONGWORD, @FillLongWord_ASM);
{$IFNDEF OMIT_MMX}
  LowLevelRegistry.Add(FID_FILLLONGWORD, @FillLongWord_MMX, [isMMX]);
{$ENDIF}
{$IFNDEF OMIT_SSE2}
  LowLevelRegistry.Add(FID_FILLLONGWORD, @FillLongword_SSE2, [isSSE2]);
{$ENDIF}

{$IFNDEF OMIT_SSE2}
  LowLevelRegistry.Add(FID_FAST_TRUNC, @FastTrunc_SSE2, [isSSE2]);
  LowLevelRegistry.Add(FID_FAST_ROUND, @FastRound_SSE41, [isSSE41]);
{$ENDIF}
{$ENDIF}

  LowLevelRegistry.RebindAll;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
  RegisterBindings;

end.
