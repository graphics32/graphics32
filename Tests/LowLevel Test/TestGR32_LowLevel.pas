unit TestGR32_LowLevel;

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

{-$DEFINE CheckNegativeInteger}
{$define FAIL_NOT_IMPLEMENTED} // Fail test if function isn't implemented

// VERIFY_WIN_MULDIV: Validate MulDiv against Windows' MulDiv. Otherwise validates against a floating point emulation.
// Note though that there's a bug in Windows' MulDiv: https://devblogs.microsoft.com/oldnewthing/20120514-00/?p=7633
{$ifdef Windows}
{$define VERIFY_WIN_MULDIV}
{$endif}

uses
{$IFDEF FPC}
  fpcunit, testregistry,
{$ELSE}
  TestFramework,
{$ENDIF}
{$ifdef Windows}
  Windows,
{$endif}
  GR32_Bindings;

// ----------------------------------------------------------------------------
//
// TBindingTestCase
//
// ----------------------------------------------------------------------------
type
  TBindingTestCase = class abstract(TTestCase)
  private
  protected
    class function FunctionRegistry: TFunctionRegistry; virtual; abstract;
    class function PriorityProc: TFunctionPriority; virtual; abstract;

    class function PriorityProcPas(Info: PFunctionInfo): Integer; static;
    class function PriorityProcAsm(Info: PFunctionInfo): Integer; static;
    class function PriorityProcMMX(Info: PFunctionInfo): Integer; static;
    class function PriorityProcSSE2(Info: PFunctionInfo): Integer; static;
    class function PriorityProcSSE41(Info: PFunctionInfo): Integer; static;

    function Rebind(FunctionID: Integer; RequireImplementation: boolean = True): boolean;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

// ----------------------------------------------------------------------------
//
// TTestLowLevel
//
// ----------------------------------------------------------------------------
type
  TTestLowLevel = class abstract(TBindingTestCase)
  private
  protected
    class function FunctionRegistry: TFunctionRegistry; override;
  public
  published
    procedure TestDiv255;
    procedure TestFastDiv255;
    procedure TestDiv255Round;

    procedure TestFastTrunc;
    procedure TestFastRound;

    procedure TestClamp1;
    procedure TestClampMax;

    procedure TestClamp2;
    procedure TestFillWord;
    procedure TestFillLongWord;
    procedure TestMoveLongword;
    procedure TestMoveWord;
{$IFDEF USESTACKALLOC}
    procedure TestStackMemory;
{$ENDIF USESTACKALLOC}
    procedure TestConstrain;
    procedure TestMinMax;
    procedure TestWrapInteger;
    procedure TestWrapFloat;
    procedure TestWrapMinMax;
    procedure TestWrapPow2;
    procedure TestMirror;
    procedure TestSAR;
  end;

  TTestLowLevelPas = class(TTestLowLevel)
  protected
    class function PriorityProc: TFunctionPriority; override;
  end;

  TTestLowLevelAsm = class(TTestLowLevel)
  protected
    class function PriorityProc: TFunctionPriority; override;
  end;

  TTestLowLevelMMX = class(TTestLowLevel)
  protected
    class function PriorityProc: TFunctionPriority; override;
  end;

  TTestLowLevelSSE2 = class(TTestLowLevel)
  protected
    class function PriorityProc: TFunctionPriority; override;
  end;

  TTestLowLevelSSE41 = class(TTestLowLevel)
  protected
    class function PriorityProc: TFunctionPriority; override;
  end;

// ----------------------------------------------------------------------------
//
// TTestMath
//
// ----------------------------------------------------------------------------
type
  TTestMath = class(TTestCase)
  published
    procedure TestFixedFloor;
    procedure TestFixedCeil;
    procedure TestFixedMul;
    procedure TestFixedDiv;
    procedure TestOneOver;
    procedure TestFixedRound;
    procedure TestFixedSqr;
    procedure TestFixedSqrtLP;
    procedure TestFixedSqrtHP;
    procedure TestFixedCombine;
    procedure TestSinCos;
    procedure TestHypotFloat;
    procedure TestHypotInt;
    procedure TestFastSqrt;
    procedure TestFastSqrtBab1;
    procedure TestFastSqrtBab2;
    procedure TestFastInvSqrt;
    procedure TestMulDiv;
    procedure TestIsPowerOf2;
    procedure TestNextPowerOf2;
    procedure TestPrevPowerOf2;
    procedure TestAverage;
    procedure TestSign;
    procedure TestFModDouble;
    procedure TestFModSingle;
    procedure TestFloatModDouble;
    procedure TestFloatModSingle;
    procedure TestFloatRemainderDouble;
    procedure TestFloatRemainderSingle;
  end;


implementation

uses
  Controls, Types, Classes, SysUtils, Messages, Graphics,
  Math,
  GR32,
  GR32_System,
  GR32_LowLevel,
  GR32_Math;

// ----------------------------------------------------------------------------
//
// TBindingTestCase
//
// ----------------------------------------------------------------------------
class function TBindingTestCase.PriorityProcPas(Info: PFunctionInfo): Integer;
begin
  if (Info^.Flags and LowLevelBindingFlagPascal <> 0) then
    Result := 0
  else
    Result := MaxInt-1;//INVALID_PRIORITY;
end;

class function TBindingTestCase.PriorityProcAsm(Info: PFunctionInfo): Integer;
begin
  if (Info^.InstructionSupport = []) then
    Result := 0
  else
    Result := INVALID_PRIORITY;
end;

class function TBindingTestCase.PriorityProcMMX(Info: PFunctionInfo): Integer;
begin
{$if not defined(PUREPASCAL)}
  if (isMMX in Info^.InstructionSupport) then
    Result := 0
  else
    Result := INVALID_PRIORITY;
{$ifend}
end;

class function TBindingTestCase.PriorityProcSSE2(Info: PFunctionInfo): Integer;
begin
{$if not defined(PUREPASCAL)}
  if (isSSE2 in Info^.InstructionSupport) then
    Result := 0
  else
    Result := INVALID_PRIORITY;
{$ifend}
end;

class function TBindingTestCase.PriorityProcSSE41(Info: PFunctionInfo): Integer;
begin
{$if not defined(PUREPASCAL)}
  if (isSSE41 in Info^.InstructionSupport) then
    Result := 0
  else
    Result := INVALID_PRIORITY;
{$ifend}
end;

function TBindingTestCase.Rebind(FunctionID: Integer; RequireImplementation: boolean): boolean;
var
  Proc: TFunctionPriority;
begin
{$ifndef FPC}
  Proc := pointer(PriorityProc);
{$else}
  Proc := PriorityProc;
{$endif}

  Result := FunctionRegistry.Rebind(FunctionID, pointer(@Proc));

  if (RequireImplementation) and (not Result) then
  begin
{$ifndef FPC}
    Enabled := False;
{$endif}
{$ifdef FAIL_NOT_IMPLEMENTED}
{$ifndef FPC}
    // Not really an error but we need to indicate that nothing was tested
    Fail('Not implemented');
{$else}
    Ignore('Not implemented');
{$endif}
{$endif}
  end;
end;

procedure TBindingTestCase.SetUp;
var
  Proc: TFunctionPriority;
begin
  inherited;
  RandSeed := 0;

{$ifndef FPC}
  Proc := pointer(PriorityProc);
{$else}
  Proc := PriorityProc;
{$endif}

  FunctionRegistry.RebindAll(True, pointer(@Proc));
end;

procedure TBindingTestCase.TearDown;
begin
  inherited;
  FunctionRegistry.RebindAll(True);
end;

// ----------------------------------------------------------------------------
//
// TTestLowLevel
//
// ----------------------------------------------------------------------------
class function TTestLowLevelPas.PriorityProc: TFunctionPriority;
begin
  Result := PriorityProcPas;
end;

class function TTestLowLevelAsm.PriorityProc: TFunctionPriority;
begin
  Result := PriorityProcAsm;
end;

class function TTestLowLevelMMX.PriorityProc: TFunctionPriority;
begin
  Result := PriorityProcMMX;
end;

class function TTestLowLevelSSE2.PriorityProc: TFunctionPriority;
begin
  Result := PriorityProcSSE2;
end;

class function TTestLowLevelSSE41.PriorityProc: TFunctionPriority;
begin
  Result := PriorityProcSSE41;
end;

// ----------------------------------------------------------------------------
//
// TTestLowLevel
//
// ----------------------------------------------------------------------------
class function TTestLowLevel.FunctionRegistry: TFunctionRegistry;
begin
  Result := LowLevelRegistry;
end;

procedure TTestLowLevel.TestClamp1;
var
  Value: integer;
  Step: integer;
  Result: integer;
begin
  Check(True);
  Step := High(Value) div 8192;
  Value := Low(Value);
  while (Value <= High(Value)-Step) do
  begin
    Result := Clamp(Value);

    if (Result < 0) or (Result > 255) then
      Fail(Format('Clamp(%d) = %d', [Value, Result]));

    Inc(Value, Step);
  end;
end;

procedure TTestLowLevel.TestClampMax;
var
  Value: integer;
  Step: integer;
  MaxValue: integer;
  Result: integer;
begin
  Check(True);
  Step := High(Value) div 8192;
  Value := Low(Value);
  while (Value <= High(Value)-Step) do
  begin
    MaxValue := Random(High(Value));

    Result := Clamp(Value, MaxValue);

    if (Result < 0) or (Result > MaxValue) then
      Fail(Format('Clamp(%d, %d) = %d', [Value, MaxValue, Result]));

    Inc(Value, Step);
  end;
end;

procedure TTestLowLevel.TestDiv255;
var
  Value: Cardinal;
begin
  // Edge cases
  CheckEquals(0, Byte(Div255(0)));
  CheckEquals(1, Byte(Div255(255)));
  CheckEquals(255, Byte(Div255(255*255)));

  // Verify documented range
  for Value := 0 to 255*255 do
    CheckEquals(Trunc(Value/255), Byte(Div255(Value)));
end;

procedure TTestLowLevel.TestFastDiv255;
var
  Value: Cardinal;
begin
  // Edge cases
  CheckEquals(0, Byte(FastDiv255(0)));
  CheckEquals(1, Byte(FastDiv255(255)));
  CheckEquals(255, Byte(FastDiv255(255*255)));

  // Verify documented range
  for Value := 0 to 255*255 do
    CheckEquals(Value div 255, FastDiv255(Value));
end;

procedure TTestLowLevel.TestDiv255Round;
var
  Value: Cardinal;
begin
  // Edge cases
  CheckEquals(0, Byte(Div255Round(0)));
  CheckEquals(1, Byte(Div255Round(255)));
  CheckEquals(255, Byte(Div255Round(255*255)));

  // Verify documented range
  for Value := 0 to 255*255 do
    CheckEquals(Round(Value/255), Div255Round(Value));
end;

procedure TTestLowLevel.TestFastRound;
var
  i: integer;
  Value: Single;
  Expected, Actual: Integer;
begin
  if (not Rebind(FID_FAST_ROUND)) then
  begin
    Check(True);
    Exit;
  end;

  for i := 1 to 1000 do
  begin
    Value := (Random(10000)-5000) / i;

    Expected := Round(Value);
    Actual := FastRound(Value);

    if (Expected <> Actual) then
      CheckEquals(Expected, Actual, Format('FastRound(%g)', [Value]));
  end;
end;

procedure TTestLowLevel.TestFastTrunc;

  procedure DoTest;
  var
    i: integer;
    Value: TFloat;
    Expected, Actual: Integer;
  begin
    Value := 0.0;
    Expected := Trunc(Value);
    Actual := FastTrunc(Value);
    CheckEquals(Expected, Actual, Format('FastTrunc(%g)', [Value]));

    Value := -0.0;
    Expected := Trunc(Value);
    Actual := FastTrunc(Value);
    CheckEquals(Expected, Actual, Format('FastTrunc(%g)', [Value]));

    for i := 1 to 1000 do
    begin
      Value := (Random(10000)-5000) / i;

      Expected := Trunc(Value);
      Actual := FastTrunc(Value);

      if (Expected <> Actual) then
        CheckEquals(Expected, Actual, Format('FastTrunc(%g)', [Value]));
    end;
  end;

var
  SaveMXCSR: DWORD;
  NewMXCSR: DWORD;
begin
  if (not Rebind(FID_FAST_TRUNC)) then
  begin
    Check(True);
    Exit;
  end;

  SaveMXCSR := GetMXCSR;
  try
    // Set Mode=Truncation
    NewMXCSR := SaveMXCSR or $00006000;
    SetMXCSR(NewMXCSR);

    DoTest;

    // Verify that MXCSR is preserved
    CheckEquals(NewMXCSR and $0000FFC0, GetMXCSR and $0000FFC0, 'MXCSR not preserved');


    // Set Mode=Floor
    NewMXCSR := (SaveMXCSR and (not $00006000)) or $00002000;
    SetMXCSR(NewMXCSR);

    DoTest;

    // Verify that MXCSR is preserved
    CheckEquals(NewMXCSR and $0000FFC0, GetMXCSR and $0000FFC0, 'MXCSR not preserved');



    // Set Mode=Round
    NewMXCSR := SaveMXCSR and (not $00006000);
    SetMXCSR(NewMXCSR);

    DoTest;

    // Verify that MXCSR is preserved
    CheckEquals(NewMXCSR and $0000FFC0, GetMXCSR and $0000FFC0, 'MXCSR not preserved');
  finally
    SetMXCSR(SaveMXCSR);
  end;
end;

procedure TTestLowLevel.TestClamp2;
var
  Index : Integer;
begin
  // check clamp without arguments
  for Index := -(1 shl 16) to (1 shl 16) do
    CheckEquals(Clamp(Index), (Abs(Index) + 255 - Abs(Index - 255)) shr 1);

  // check clamp with max argument
  for Index := -(1 shl 16) to (1 shl 16) do
    CheckEquals(Clamp(Index, 100), (Abs(Index) + 100 - Abs(Index - 100)) shr 1);

  // check clamp with min/max argument
  for Index := -(1 shl 16) to (1 shl 16) do
  begin
    CheckEquals(Clamp(Index, 20, 108), (Abs(Index - 20) + 128 - Abs(Index - 108)) shr 1);
    {$IFDEF CheckNegativeInteger}
    CheckEquals(Clamp(Index, -20, 20), (Abs(Index + 20) - Abs(Index - 20)) shr 1);
    {$ENDIF}
  end;
end;

procedure TTestLowLevel.TestConstrain;
var
  Index : Integer;
begin
  // check constrain with min/max argument
  for Index := -(1 shl 16) to (1 shl 16) do
  begin
    CheckEquals(Constrain(Index, 20, 108), (Abs(Index - 20) + 128 - Abs(Index - 108)) shr 1);
    {$IFDEF CheckNegativeInteger}
    CheckEquals(Constrain(Index, -20, 20), (Abs(Index + 20) - Abs(Index - 20)) shr 1);
    {$ENDIF}
  end;

  // check constrain with min/max argument
  for Index := -(1 shl 16) to (1 shl 16) do
  begin
    CheckEquals(Constrain(Index, 0.1, 0.9), (Abs(Index - 0.1) + 1 - Abs(Index - 0.9)) * 0.5, 1E-6);
    CheckEquals(Constrain(Index, -0.5, 0.5), (Abs(Index + 0.5) - Abs(Index - 0.5)) * 0.5, 1E-6);
  end;
end;

procedure TTestLowLevel.TestFillLongWord;
var
  Data  : PColor32Array;
  Index : Integer;
const
  CColor32Count: Integer = 1024;  // must be larger than 32!
  CFillValue: array [0..1] of Cardinal = ($12345678, $1337DEAD);
begin
  if (not Rebind(FID_FILLLONGWORD)) then
  begin
    Check(True);
    Exit;
  end;

  GetMem(Data, CColor32Count * SizeOf(TColor32));
  try
    // check zero count
    Data^[0] := CFillValue[0];
    FillLongWord(Data^, 0, CFillValue[1]);
    CheckEquals(CFillValue[0], Data^[0]);

    // check large count
    FillLongWord(Data^, CColor32Count, not CFillValue[1]);
    for Index := 0 to CColor32Count - 1 do
      CheckEquals(not CFillValue[1], Data^[Index], 'Error at index ' +
        IntToStr(Index));

    // check odd small count
    FillLongWord(Data^, 31, CFillValue[0]);
    for Index := 0 to 30 do
      CheckEquals(CFillValue[0], Data^[Index], 'Error at index ' +
        IntToStr(Index));
    for Index := 31 to 64 do
      CheckEquals(not CFillValue[1], Data^[Index], 'Error at index ' +
        IntToStr(Index));

    // check odd large count
    FillLongWord(Data^, CColor32Count - 1, CFillValue[1]);
    for Index := 0 to CColor32Count - 2 do
      CheckEquals(CFillValue[1], Data^[Index], 'Error at index ' +
        IntToStr(Index));
    CheckEquals(not CFillValue[1], Data^[CColor32Count - 1],
      'Error at index ' + IntToStr(CColor32Count - 1));

    // test odd start address
    Inc(Data);
    try
      FillLongWord(Data^, CColor32Count div 2, not CFillValue[0]);
      for Index := 0 to (CColor32Count div 2) - 1 do
        CheckEquals(not CFillValue[0], Data^[Index]);
    finally
      Dec(Data);
    end;
  finally
    FreeMem(Data);
  end;
end;

procedure TTestLowLevel.TestFillWord;
var
  Data  : PWordArray;
  Index : Integer;
const
  CWordCount: Integer = 1024;
  CFillValue: Word = $1337;
begin
  GetMem(Data, CWordCount * SizeOf(Word));
  try
    // check zero count
    Data^[0] := $1234;
    FillWord(Data^, 0, CFillValue);
    CheckEquals($1234, Data^[0]);

    FillWord(Data^, CWordCount, CFillValue);
    for Index := 0 to CWordCount - 1 do
      CheckEquals(CFillValue, Data^[Index]);

    // test odd start address
    Inc(Data);
    try
      FillWord(Data^, CWordCount div 2, not CFillValue);
      for Index := 0 to (CWordCount div 2) - 1 do
        CheckEquals(not CFillValue, Data^[Index]);
    finally
      Dec(Data);
    end;
  finally
    FreeMem(Data);
  end;
end;

procedure TTestLowLevel.TestMinMax;
var
  Index : Integer;
begin
  // check min/max in one go
  // Yes, very clever... but completely obfuscates what's going on. Pfft!
  for Index := -(1 shl 16) to (1 shl 16) do
  begin
    // TODO : Originally tested Min(a, b) & Max(a, b). Now modified to test Min(a, b, c) & Max(a, b, c). Needs to be rewritten.
    CheckEquals(Max(20, Min(108, Index, 108), 20), (Abs(Index - 20) + 128 - Abs(Index - 108)) shr 1);
    {$IFDEF CheckNegativeInteger}
    CheckEquals(Max(Min(Index, 20), -20), (Abs(Index + 20) - Abs(Index - 20)) shr 1);
    {$ENDIF}
  end;
end;

procedure TTestLowLevel.TestMirror;
begin
  CheckEquals(50, Mirror(50, 100));
  CheckEquals(51, Mirror(150, 100));
  CheckEquals(50, Mirror(149, 99));
  CheckEquals(64, MirrorPow2(64, 127));
  CheckEquals(63, MirrorPow2(192, 127));
  CheckEquals(95, MirrorPow2(160, 127));
end;

procedure TTestLowLevel.TestMoveLongword;
var
  Data  : array [0..1] of PColor32Array;
  Index : Integer;
const
  CDataCount: Integer = 1024;
  CFillValue: TColor32 = $DEADBEEF;
begin
  GetMem(Data[0], CDataCount * SizeOf(Cardinal));
  GetMem(Data[1], CDataCount * SizeOf(Cardinal));
  try
    for Index := 0 to CDataCount - 1 do
      Data[0]^[Index] := CFillValue;

    MoveLongWord(Data[0]^, Data[1]^, CDataCount);

    for Index := 0 to CDataCount - 1 do
      CheckEquals(CFillValue, Data[1]^[Index]);

    // test odd start address
    Inc(Data[0]);
    Inc(Data[1]);
    try
      for Index := 0 to (CDataCount div 2) - 1 do
        Data[0]^[Index] := not CFillValue;

      MoveLongWord(Data[0]^, Data[1]^, (CDataCount div 2));

      for Index := 0 to (CDataCount div 2) - 1 do
        CheckEquals(not CFillValue, Data[1]^[Index]);
    finally
      Dec(Data[0]);
      Dec(Data[1]);
    end;
  finally
    FreeMem(Data[0]);
    FreeMem(Data[1]);
  end;
end;

procedure TTestLowLevel.TestMoveWord;
var
  Data  : array [0..1] of PWordArray;
  Index : Integer;
const
  CWordCount: Integer = 1024;
  CFillValue: Word = $1337;
begin
  GetMem(Data[0], CWordCount * SizeOf(Word));
  GetMem(Data[1], CWordCount * SizeOf(Word));
  try
    for Index := 0 to CWordCount - 1 do
      Data[0]^[Index] := CFillValue;

    MoveWord(Data[0]^, Data[1]^, CWordCount);

    for Index := 0 to CWordCount - 1 do
      CheckEquals(CFillValue, Data[1]^[Index]);

    // test odd start address
    Inc(Data[0]);
    Inc(Data[1]);
    try
      for Index := 0 to (CWordCount div 2) - 1 do
        Data[0]^[Index] := not CFillValue;

      MoveWord(Data[0]^, Data[1]^, (CWordCount div 2));

      for Index := 0 to (CWordCount div 2) - 1 do
        CheckEquals(not CFillValue, Data[1]^[Index]);
    finally
      Dec(Data[0]);
      Dec(Data[1]);
    end;

    // Source=Dest: Should be a no-op
    MoveWord(Data[0]^, Data[0]^, MaxInt);

  finally
    FreeMem(Data[0]);
    FreeMem(Data[1]);
  end;

  Data[0] := pointer(0);
  Data[1] := pointer(1);

  // Count=0: Should be a no-op
  MoveWord(Data[0]^, Data[1]^, 0);
end;

procedure TTestLowLevel.TestSAR;
const
  CValue: Integer = $7ADE4BE1;
begin
  CheckEquals(SAR_3(CValue), CValue div 8, 'Error in function SAR_3');
  CheckEquals(SAR_4(CValue), CValue div 16, 'Error in function SAR_4');
  CheckEquals(SAR_8(CValue), CValue div 256, 'Error in function SAR_8');
  CheckEquals(SAR_9(CValue), CValue div 512, 'Error in function SAR_9');
  CheckEquals(SAR_11(CValue), CValue div 2048, 'Error in function SAR_11');
  CheckEquals(SAR_12(CValue), CValue div 4096, 'Error in function SAR_12');
  CheckEquals(SAR_13(CValue), CValue div 8192, 'Error in function SAR_13');
  CheckEquals(SAR_14(CValue), CValue div 16384, 'Error in function SAR_14');
  CheckEquals(SAR_15(CValue), CValue div 32768, 'Error in function SAR_15');
  CheckEquals(SAR_16(CValue), CValue div 65536, 'Error in function SAR_16');
end;

{$IFDEF USESTACKALLOC}
procedure TTestLowLevel.TestStackMemory;
var
  Data  : PColor32Array;
  Index : Integer;
const
  CDataCount: Integer = 16;
  CFillValue: TColor32 = $DEADBEEF;
begin
  Data := StackAlloc(CDataCount * SizeOf(TColor32));
  try
    for Index := 0 to CDataCount - 1 do
      Data^[Index] := CFillValue
  finally
    StackFree(Data);
  end;
end;
{$ENDIF USESTACKALLOC}

procedure TTestLowLevel.TestWrapInteger;
var
  i: integer;
  Expected, Actual: integer;
begin
  CheckEquals(50, Wrap(50, 100));
  CheckEquals(49, Wrap(150, 100));
  CheckEquals(50, Wrap(150, 99));

  // Negative values
  CheckEquals(50, Wrap(-150, 99));

  // Edge cases
  CheckEquals(0, Wrap(0, 0));
  CheckEquals(0, Wrap(50, 0));
  CheckEquals(0, Wrap(0, 50));
  CheckEquals(50, Wrap(50, 50));

  // Function should produce a saw-tooth
  Expected := 5;
  for i := -100 to 100 do
  begin
    Actual := Wrap(i, 20);

    CheckEquals(Expected, Actual);

    if (Expected = 20) then
      Expected := 0
    else
      Inc(Expected);
  end;
end;

procedure TTestLowLevel.TestWrapFloat;
var
  i: integer;
  Expected, Actual: Single;
const
  Epsilon = 1e-10;
begin
  CheckEquals(50.0, Wrap(50.0, 100.0), Epsilon);
  CheckEquals(50, Wrap(150.0, 100.0), Epsilon);
  CheckEquals(51, Wrap(150.0, 99.0), Epsilon);

  // Negative values
  CheckEquals(48.0, Wrap(-150.0, 99.0), Epsilon);

  // Edge cases
  CheckEquals(0.0, Wrap(0.0, 0.0), Epsilon);
  CheckEquals(0.0, Wrap(50.0, 0.0), Epsilon);
  CheckEquals(0.0, Wrap(0.0, 50.5), Epsilon);
  CheckEquals(0.0, Wrap(50.5, 50.5), Epsilon);

  // Function should produce a saw-tooth
  Expected := 0;
  for i := -100 to 100 do
  begin
    Actual := Wrap(i, 20.0);

    CheckEquals(Expected, Actual, Epsilon);

    Expected := Expected + 1.0;
    if (Expected = 20) then
      Expected := 0;
  end;
end;

procedure TTestLowLevel.TestWrapMinMax;
var
  i: integer;
  Expected, Actual : integer;
begin
  // Inside range
  CheckEquals(50, Wrap(50, 25, 100));

  // Outside range
  CheckEquals(77, Wrap(1, 25, 100));
  CheckEquals(74, Wrap(150, 25, 100));

  CheckEquals(74, Wrap(150, 25, 100));
  CheckEquals(75, Wrap(150, 25, 99));

  // Negative values
  CheckEquals(78, Wrap(-150, 25, 100));
  CheckEquals(75, Wrap(-150, 25, 99));

  // Edge cases
  CheckEquals(10, Wrap(0, 10, 10));
  CheckEquals(10, Wrap(50, 10, 10));
  CheckEquals(10, Wrap(10, 10, 50));
  CheckEquals(50, Wrap(50, 10, 50));

  // Function should produce a saw-tooth
  Expected := 11;
  for i := 0 to 100 do
  begin
    Actual := Wrap(i, 10, 20);

    CheckEquals(Expected, Actual);

    if (Expected = 20) then
      Expected := 10
    else
      Inc(Expected);
  end;
end;

procedure TTestLowLevel.TestWrapPow2;
begin
  CheckEquals(64, WrapPow2(64, 127));
  CheckEquals(64, WrapPow2(192, 127));
  CheckEquals(32, WrapPow2(160, 127));
end;

// ----------------------------------------------------------------------------
//
// TTestMath
//
// ----------------------------------------------------------------------------
procedure TTestMath.TestAverage;
var
  X, Y : Integer;
begin
  for X := 0 to (1 shl 10) do
    for Y := 0 to (1 shl 10) do
      CheckEquals((X + Y) div 2, Average(X, Y));
end;

// ----------------------------------------------------------------------------
// GR32_Math.FMod
// ----------------------------------------------------------------------------

function FMod_Reference(ANumerator, ADenominator: Double): Double; overload;
begin
  Result := ANumerator - ADenominator * Trunc(ANumerator / ADenominator);
end;

function FMod_Reference(ANumerator, ADenominator: Single): Single; overload;
begin
  Result := ANumerator - ADenominator * Trunc(ANumerator / ADenominator);
end;

procedure TTestMath.TestFModDouble;
var
  Numerator, Denominator: Double;
  Expected, Actual: Double;
  i: integer;
const
  Epsilon = 1e-10;
begin
  Denominator := 10;
  while (Denominator >= -10) do
  begin
    Denominator := Denominator - 1.3;

    Numerator := 3 * Denominator;
    while (Numerator >= -3 * Denominator) do
    begin
      Numerator := Numerator - 0.3;

      Actual := GR32_Math.FMod(Numerator, Denominator);
      Expected := FMod_Reference(Numerator, Denominator);

      CheckEquals(Expected, Actual, Epsilon, Format('FMod(%n, %n)', [Numerator, Denominator]));
    end;
  end;

  // Common test cases
  CheckEquals(5.0, GR32_Math.FMod(5.0, -10.0), Epsilon);
  CheckEquals(4.5, GR32_Math.FMod(4.5, -10.0), Epsilon);
  CheckEquals(-5.0, GR32_Math.FMod(-5.0, 10.0), Epsilon);
  CheckEquals(-4.5, GR32_Math.FMod(-4.5, 10.0), Epsilon);

  // Edge cases
  CheckEquals(0, GR32_Math.FMod(0, 50), Epsilon);
  CheckEquals(0.0, GR32_Math.FMod(0.0, 50.5), Epsilon);

  CheckEquals(0, GR32_Math.FMod(50, 50), Epsilon);
  CheckEquals(0.0, GR32_Math.FMod(50.5, 50.5), Epsilon);

  // Function should produce a saw-tooth
  Expected := -10;
  for i := -100 to 100 do
  begin
    Actual := GR32_Math.FMod(i * 0.5, 20.0);

    CheckEquals(Expected, Actual, Epsilon, Format('FMod(%n, %n)', [i * 0.5, 20.0]));

    if (i < 0) then
    begin
      if (Expected = 0) then
        Expected := -20;
      Expected := Expected + 0.5;
    end else
    begin
      Expected := Expected + 0.5;
      if (Expected = 20) then
        Expected := 0;
    end;

  end;
end;

procedure TTestMath.TestFModSingle;
var
  Numerator, Denominator: Single;
  Expected, Actual: Single;
  i: integer;
const
  Epsilon = 1e-5;
begin
  Denominator := 10;
  while (Denominator >= -10) do
  begin
    Denominator := Denominator - 1.3;

    Numerator := 3 * Denominator;
    while (Numerator >= -3 * Denominator) do
    begin
      Numerator := Numerator - 0.3;

      Actual := GR32_Math.FMod(Numerator, Denominator);
      Expected := FMod_Reference(Numerator, Denominator);

      CheckEquals(Expected, Actual, Epsilon);
    end;
  end;

  // Common test cases
  CheckEquals(5.0, GR32_Math.FMod(5.0, -10.0), Epsilon);
  CheckEquals(4.5, GR32_Math.FMod(4.5, -10.0), Epsilon);
  CheckEquals(-5.0, GR32_Math.FMod(-5.0, 10.0), Epsilon);
  CheckEquals(-4.5, GR32_Math.FMod(-4.5, 10.0), Epsilon);

  // Edge cases
  CheckEquals(0, GR32_Math.FMod(0, 50), Epsilon);
  CheckEquals(0.0, GR32_Math.FMod(0.0, 50.5), Epsilon);

  CheckEquals(0, GR32_Math.FMod(50, 50), Epsilon);
  CheckEquals(0.0, GR32_Math.FMod(50.5, 50.5), Epsilon);

  // Function should produce a saw-tooth
  Expected := -10;
  for i := -100 to 100 do
  begin
    Actual := GR32_Math.FMod(i * 0.5, 20.0);

    CheckEquals(Expected, Actual, Epsilon, Format('FMod(%n, %n)', [i * 0.5, 20.0]));

    if (i < 0) then
    begin
      if (Expected = 0) then
        Expected := -20;
      Expected := Expected + 0.5;
    end else
    begin
      Expected := Expected + 0.5;
      if (Expected = 20) then
        Expected := 0;
    end;

  end;
end;

// ----------------------------------------------------------------------------
// FloatMod
// ----------------------------------------------------------------------------

function FloatMod_Reference(ANumerator, ADenominator: Double): Double; overload;
begin
  if ((ANumerator >= 0) and (ANumerator < ADenominator)) or (ADenominator = 0) then
    Result := ANumerator
  else
    Result := ANumerator - ADenominator * Floor(ANumerator / ADenominator);
end;

function FloatMod_Reference(ANumerator, ADenominator: Single): Single; overload;
begin
  if ((ANumerator >= 0) and (ANumerator < ADenominator)) or (ADenominator = 0) then
    Result := ANumerator
  else
    Result := ANumerator - ADenominator * Floor(ANumerator / ADenominator);
end;

procedure TTestMath.TestFloatModDouble;
var
  Numerator, Denominator: Double;
  Expected, Actual: Double;
  i: integer;
const
  Epsilon = 1e-10;
begin
  Denominator := 10;
  while (Denominator >= -10) do
  begin
    Denominator := Denominator - 1.3;

    Numerator := 3 * Denominator;
    while (Numerator >= -3 * Denominator) do
    begin
      Numerator := Numerator - 0.3;

      Actual := FloatMod(Numerator, Denominator);
      Expected := FloatMod_Reference(Numerator, Denominator);

      CheckEquals(Expected, Actual, Epsilon);
    end;
  end;

  // Common test cases
  CheckEquals(-5.0, FloatMod(5.0, -10.0), Epsilon);
  CheckEquals(-5.5, FloatMod(4.5, -10.0), Epsilon);
  CheckEquals(5.0, FloatMod(-5.0, 10.0), Epsilon);
  CheckEquals(5.5, FloatMod(-4.5, 10.0), Epsilon);

  // Edge cases
  CheckEquals(0, FloatMod(0, 0), Epsilon);
  CheckEquals(0.0, FloatMod(0.0, 0.0), Epsilon);

  CheckEquals(50, FloatMod(50, 0), Epsilon);
  CheckEquals(50.0, FloatMod(50.0, 0.0), Epsilon);

  CheckEquals(0, FloatMod(0, 50), Epsilon);
  CheckEquals(0.0, FloatMod(0.0, 50.5), Epsilon);

  CheckEquals(0, FloatMod(50, 50), Epsilon);
  CheckEquals(0.0, FloatMod(50.5, 50.5), Epsilon);

  // Function should produce a saw-tooth
  Expected := 10;
  for i := -100 to 100 do
  begin
    Actual := FloatMod(i * 0.5, 20.0);

    CheckEquals(Expected, Actual, Epsilon);

    Expected := Expected + 0.5;
    if (Expected = 20) then
      Expected := 0;
  end;
end;

procedure TTestMath.TestFloatModSingle;
var
  Numerator, Denominator: Single;
  Expected, Actual: Single;
  i: integer;
const
  Epsilon = 1e-5;
begin
  Denominator := 10;
  while (Denominator >= -10) do
  begin
    Denominator := Denominator - 1.3;

    Numerator := 3 * Denominator;
    while (Numerator >= -3 * Denominator) do
    begin
      Numerator := Numerator - 0.3;

      Actual := FloatMod(Numerator, Denominator);
      Expected := FloatMod_Reference(Numerator, Denominator);

      CheckEquals(Expected, Actual, Epsilon);
    end;
  end;

  // Common test cases
  CheckEquals(-5.0, FloatMod(5.0, -10.0), Epsilon);
  CheckEquals(-5.5, FloatMod(4.5, -10.0), Epsilon);
  CheckEquals(5.0, FloatMod(-5.0, 10.0), Epsilon);
  CheckEquals(5.5, FloatMod(-4.5, 10.0), Epsilon);

  // Edge cases
  CheckEquals(0, FloatMod(0, 0), Epsilon);
  CheckEquals(0.0, FloatMod(0.0, 0.0), Epsilon);

  CheckEquals(50, FloatMod(50, 0), Epsilon);
  CheckEquals(50.0, FloatMod(50.0, 0.0), Epsilon);

  CheckEquals(0, FloatMod(0, 50), Epsilon);
  CheckEquals(0.0, FloatMod(0.0, 50.5), Epsilon);

  CheckEquals(0, FloatMod(50, 50), Epsilon);
  CheckEquals(0.0, FloatMod(50.5, 50.5), Epsilon);

  // Function should produce a saw-tooth
  Expected := 10;
  for i := -100 to 100 do
  begin
    Actual := FloatMod(i * 0.5, 20.0);

    CheckEquals(Expected, Actual, Epsilon);

    Expected := Expected + 0.5;
    if (Expected = 20) then
      Expected := 0;
  end;
end;

// ----------------------------------------------------------------------------
// FloatRemainder
// ----------------------------------------------------------------------------

function FloatRemainder_Reference(ANumerator, ADenominator: Double): Double; overload;
begin
  if ((ANumerator >= 0) and (ANumerator < ADenominator)) or (ADenominator = 0) then
    Result := ANumerator
  else
    Result := ANumerator - ADenominator * Round(ANumerator / ADenominator);
end;

function FloatRemainder_Reference(ANumerator, ADenominator: Single): Single; overload;
begin
  if ((ANumerator >= 0) and (ANumerator < ADenominator)) or (ADenominator = 0) then
    Result := ANumerator
  else
    Result := ANumerator - ADenominator * Round(ANumerator / ADenominator);
end;

procedure TTestMath.TestFloatRemainderDouble;
var
  Numerator, Denominator: Double;
  Expected, Actual: Double;
const
  Epsilon = 1e-10;
begin
  Denominator := 10;
  while (Denominator >= -10) do
  begin
    Denominator := Denominator - 1.3;

    Numerator := 3 * Denominator;
    while (Numerator >= -3 * Denominator) do
    begin
      Numerator := Numerator - 0.3;

      Actual := FloatRemainder(Numerator, Denominator);
      Expected := FloatRemainder_Reference(Numerator, Denominator);

      CheckEquals(Expected, Actual, Epsilon);
    end;
  end;

  // Common test cases
  CheckEquals(5.0, FloatRemainder(5.0, -10.0), Epsilon);
  CheckEquals(4.5, FloatRemainder(4.5, -10.0), Epsilon);
  CheckEquals(-5.0, FloatRemainder(-5.0, 10.0), Epsilon);
  CheckEquals(-4.5, FloatRemainder(-4.5, 10.0), Epsilon);

  // Test values from https://en.cppreference.com/w/c/numeric/math/remainder
  // The results are completely bonkers
  (*
  CheckEquals(-0.9, FloatRemainder(+5.1, +3.0), Epsilon);
  CheckEquals(+0.9, FloatRemainder(-5.1, +3.0), Epsilon);
  CheckEquals(-0.9, FloatRemainder(+5.1, -3.0), Epsilon);
  CheckEquals(+0.9, FloatRemainder(-5.1, -3.0), Epsilon);
  CheckEquals(+0.0, FloatRemainder(+0.0, +1.0), Epsilon);
  CheckEquals(-0.0, FloatRemainder(-0.0, +1.0), Epsilon);
  *)

  // Edge cases
  CheckEquals(0.0, FloatRemainder(0.0, 0.0), Epsilon);
  CheckEquals(50.0, FloatRemainder(50.0, 0.0), Epsilon);
  CheckEquals(0.0, FloatRemainder(0.0, 50.0), Epsilon);
  CheckEquals(0.0, FloatRemainder(50.0, 50.0), Epsilon);

  // Function should produce a saw-tooth - and it does but I can't figure out how to verify it :-(
end;

procedure TTestMath.TestFloatRemainderSingle;
var
  Numerator, Denominator: Double;
  Expected, Actual: Double;
const
  Epsilon = 1e-10;
begin
  Denominator := 10;
  while (Denominator >= -10) do
  begin
    Denominator := Denominator - 1.3;

    Numerator := 3 * Denominator;
    while (Numerator >= -3 * Denominator) do
    begin
      Numerator := Numerator - 0.3;

      Actual := FloatRemainder(Numerator, Denominator);
      Expected := FloatRemainder_Reference(Numerator, Denominator);

      CheckEquals(Expected, Actual, Epsilon);
    end;
  end;

  // Common test cases
  CheckEquals(5.0, FloatRemainder(5.0, -10.0), Epsilon);
  CheckEquals(4.5, FloatRemainder(4.5, -10.0), Epsilon);
  CheckEquals(-5.0, FloatRemainder(-5.0, 10.0), Epsilon);
  CheckEquals(-4.5, FloatRemainder(-4.5, 10.0), Epsilon);

  // Edge cases
  CheckEquals(0.0, FloatRemainder(0.0, 0.0), Epsilon);
  CheckEquals(50.0, FloatRemainder(50.0, 0.0), Epsilon);
  CheckEquals(0.0, FloatRemainder(0.0, 50.0), Epsilon);
  CheckEquals(0.0, FloatRemainder(50.0, 50.0), Epsilon);
end;

// ----------------------------------------------------------------------------
// Hypot
// ----------------------------------------------------------------------------
procedure TTestMath.TestHypotFloat;
var
  X, Y : Integer;
  Expected, Actual: TFloat;
begin
  for X := 0 to (1 shl 11) do
    for Y := 0 to (1 shl 11) do
    begin
      Expected := Math.Hypot(X, 1E-4 * Y);
      Actual := GR32_Math.Hypot(X, 1E-4 * Y);
      if (not SameValue(Expected, Actual, 7E-5)) then
        CheckEquals(Expected, Actual, 7E-5, Format('Hypot(%.0n, %.4n)', [1.0 * X, 1E-4 * Y]));
    end;
end;

procedure TTestMath.TestHypotInt;
var
  X, Y : Integer;
  Expected, Actual: integer;
begin
  for X := 0 to (1 shl 11) do
    for Y := 0 to (1 shl 11) do
    begin
      Expected := Round(Math.Hypot(X, Y));
      Actual := GR32_Math.Hypot(X, Y);
      if (Expected <> Actual) then
        CheckEquals(Expected, Actual, Format('Hypot(%.0n, %.4n)', [1.0 * X, 1.0 * Y]));
    end;
end;

procedure TTestMath.TestIsPowerOf2;
var
  Index : Integer;
begin
  CheckFalse(IsPowerOf2(0), Format('IsPowerOf2($%.8X))', [0]));

  for Index := 0 to 31 do
  begin
    CheckTrue(IsPowerOf2(1 shl Index), Format('IsPowerOf2($%.8X))', [1 shl Index]));

    if Index > 0 then
    begin
      if Index > 1 then
        CheckTrue(not IsPowerOf2((1 shl Index) - 1), Format('IsPowerOf2($%.8X))', [(1 shl Index) - 1]));

      if Index < 31 then
        CheckTrue(not IsPowerOf2((1 shl Index) + 1), Format('IsPowerOf2($%.8X))', [(1 shl Index) + 1]));
    end;
  end;
end;

procedure TTestMath.TestNextPowerOf2;
var
  Index : Integer;
begin
  CheckEquals(1, NextPowerOf2(0), Format('NextPowerOf2($%.8X))', [0]));

  for Index := 0 to 30 do
  begin
    CheckEquals(1 shl Index, NextPowerOf2(1 shl Index), Format('NextPowerOf2($%.8X))', [1 shl Index]));

    if Index > 0 then
    begin
      if Index > 1 then
        CheckEquals(1 shl Index, NextPowerOf2((1 shl Index) - 1), Format('NextPowerOf2($%.8X))', [1 shl Index]));
      if Index < 30 then
        CheckEquals(1 shl Index, NextPowerOf2((1 shl Index) + 1) shr 1, Format('NextPowerOf2($%.8X))', [1 shl Index]));
    end;
  end;
end;

procedure TTestMath.TestPrevPowerOf2;
var
  Index : Integer;
begin
  CheckEquals(0, PrevPowerOf2(0), Format('PrevPowerOf2($%.8X))', [0]));

  for Index := 0 to 30 do
  begin
    CheckEquals(1 shl Index, PrevPowerOf2(1 shl Index), Format('PrevPowerOf2($%.8X))', [1 shl Index]));

    if Index > 0 then
    begin
      if Index > 1 then
        CheckEquals(1 shl Index, PrevPowerOf2((1 shl Index) - 1) shl 1, Format('PrevPowerOf2($%.8X))', [(1 shl Index)-1]));

      if Index < 30 then
        CheckEquals(1 shl Index, PrevPowerOf2((1 shl Index) + 1), Format('PrevPowerOf2($%.8X))', [(1 shl Index)+1]));
    end;
  end;
end;

procedure TTestMath.TestSign;
var
  Index : Integer;
begin
  CheckEquals(0, GR32_Math.Sign(0));
  CheckEquals(1, GR32_Math.Sign($7FFFFFFF));
  CheckEquals(-1, GR32_Math.Sign(Integer($80000000)));
  for Index := 1 to (1 shl 10) do
  begin
    CheckEquals(1, GR32_Math.Sign(Index));
    CheckEquals(-1, GR32_Math.Sign(-Index));
  end;
end;

procedure TTestMath.TestFastInvSqrt;
var
  Index : Integer;
begin
  for Index := 1 to (1 shl 8) do
    CheckEquals(1 / Sqrt(Index), FastInvSqrt(Index), 3E-3);
end;

procedure TTestMath.TestFastSqrt;
var
  Index : Integer;
  Expected, Actual: TFloat;
const
{$ifndef PUREPASCAL}
  Tolerance = 1E-1;
{$else}
  Tolerance = 7E-1; // Eeew!
{$endif }
begin
  for Index := 10 to (1 shl 8) do
  begin
    Expected := Sqrt(Index);
    Actual := FastSqrt(Index);
    if (not SameValue(Expected, Actual, Tolerance)) then
      CheckEquals(Expected, Actual, Tolerance, Format('FastSqrt(%d)', [Index]));
  end;
end;

procedure TTestMath.TestFastSqrtBab1;
var
  Index : Integer;
begin
  for Index := 1 to (1 shl 8) do
    CheckEquals(Sqrt(Index), FastSqrtBab1(Index), 1E-1);
end;

procedure TTestMath.TestFastSqrtBab2;
var
  Index : Integer;
begin
  for Index := 1 to (1 shl 8) do
    CheckEquals(Sqrt(Index), FastSqrtBab2(Index), 1E-1);
end;

procedure TTestMath.TestMulDiv;
var
  Multiplicand : Integer;
  Multiplier   : Integer;
  Divisor      : Integer;
  Expected     : Integer;
  Actual       : Integer;
{$ifndef VERIFY_WIN_MULDIV}
  SaveRoundMode: TRoundingMode;
{$endif}
begin
{$ifndef VERIFY_WIN_MULDIV}
  SaveRoundMode := SetRoundMode(rmTruncate);
  try
{$endif}
    for Multiplicand := Low(Byte) to High(Byte) do
      for Multiplier := Low(Byte) to High(Byte) do
        for Divisor := 1 to High(Byte) do
        begin
          Actual := MulDiv(Multiplicand, Multiplier, Divisor);
{$ifdef VERIFY_WIN_MULDIV}
          // Check against Windows' MulDiv since that is the one we're emulating
          Expected := Windows.MulDiv(Multiplicand, Multiplier, Divisor);
{$else}
          Expected := Round(Multiplicand * Multiplier / Divisor + 0.5);
{$endif}

          if (Actual <> Expected) then // Avoid costly Format evaluation unless faillure
            CheckEquals(Expected, Actual, Format('MulDiv(%d, %d, %d)', [Multiplicand, Multiplier, Divisor]));
        end;

    // Same again but in Word range
    Multiplicand := Low(Byte);
    while (Multiplicand <= High(SmallInt)) do
    begin
      Multiplier := Low(Byte);
      while (Multiplier <= High(SmallInt)) do
      begin
        Divisor := 1;
        while (Divisor <= High(SmallInt)) do
        begin
          Actual := MulDiv(Multiplicand, Multiplier, Divisor);
{$ifdef VERIFY_WIN_MULDIV}
          // Check against Windows' MulDiv since that is the one we're emulating.
          Expected := Windows.MulDiv(Multiplicand, Multiplier, Divisor);
{$else}
          Expected := Round(Multiplicand * Multiplier / Divisor + 0.5);
{$endif}

          if (Actual <> Expected) then // Avoid costly Format evaluation unless faillure
            CheckEquals(Expected, Actual, Format('MulDiv(%d, %d, %d)', [Multiplicand, Multiplier, Divisor]));

          Inc(Divisor, 67); // Odd step
        end;

        Inc(Multiplier, 257); // Odd step
      end;

      Inc(Multiplicand, 129); // Odd step
    end;

{$ifndef VERIFY_WIN_MULDIV}
  finally
    SetRoundMode(SaveRoundMode);
  end;
{$endif}
end;

procedure TTestMath.TestSinCos;
var
  Value : Single;
  Data  : array [0..3] of Single;
begin
  Value := 0;
  repeat
    Math.SinCos(Value, Data[0], Data[1]);
    GR32_Math.SinCos(Value, Data[2], Data[3]);
    CheckEquals(Data[0], Data[2], 1E-8);
    CheckEquals(Data[1], Data[3], 1E-8);
    Value := Value + 1E-5;
  until Value > 1;
end;

procedure TTestMath.TestFixedCeil;
var
  Value: TFixedRec;
  Index : Integer;
begin
  for Index := 1 to (1 shl 7) do
    begin
      Value.Int := Index;
      Value.Frac := Index;
      CheckEquals(Index + 1, FixedCeil(Value.Fixed));
    end;
end;

procedure TTestMath.TestFixedFloor;
var
  Value: TFixedRec;
  Index : Integer;
begin
  for Index := 1 to (1 shl 7) do
  begin
    Value.Int := Index;
    Value.Frac := Index;
    CheckEquals(Index, FixedFloor(Value.Fixed));
  end;
end;

procedure TTestMath.TestFixedRound;
var
  Value: TFixedRec;
  Index : Integer;
begin
  // Positive values
  for Index := -(1 shl 7) to (1 shl 7) do
  begin
    Value.Int := Abs(Index);
    Value.Frac := Index;
    if Index < 0 then
      CheckEquals(Value.Int + 1, FixedRound(Value.Fixed), 'Positive round up')
    else
      CheckEquals(Value.Int, FixedRound(Value.Fixed), 'Positive round down');
  end;

  // Negative values
  for Index := -(1 shl 7) to (1 shl 7) do
  begin
    Value.Int := -Abs(Index);
    Value.Frac := Index;
    if Index < 0 then
      CheckEquals(Value.Int + 1, FixedRound(Value.Fixed), 'Negative round up')
    else
      CheckEquals(Value.Int, FixedRound(Value.Fixed), 'Negative round down');
  end;
end;

procedure TTestMath.TestFixedMul;
var
  A, B : TFixed;
  X, Y : Integer;
begin
  for X := 0 to (1 shl 9) do
    for Y := 0 to (1 shl 9) do
    begin
      A := Fixed(1E-2 * X);
      B := Fixed(1E-1 * Y);
      CheckEquals(1E-3 * X * Y, FixedMul(A, B) / (1 shl 16), 1E-3);
    end;
end;

procedure TTestMath.TestFixedDiv;
var
  A, B : TFixed;
  X, Y : Integer;
begin
  for X := 0 to (1 shl 7) do
    for Y := 1 to (1 shl 9) do
    begin
      A := Fixed(1E-2 * X);
      B := Fixed(1E-1 * Y);
      CheckEquals(1E-1 * X / Y, FixedDiv(A, B) / (1 shl 16), 1E-3);
    end;
end;

procedure TTestMath.TestFixedCombine;
var
  A, B, C : TFixed;
  W, X, Y : Integer;
begin
  for W := 0 to (1 shl 8) do
    for X := 0 to (1 shl 8) do
      for Y := 0 to (1 shl 8) do
      begin
        A := Fixed(1E-2 * X);
        B := Fixed(1E-1 * Y);
        C := Fixed(W / (1 shl 8));
        CheckEquals(W / (1 shl 8) * 1E-2 * X + (1 - W / (1 shl 8)) * 1E-1 * Y,
          FixedCombine(C, A, B) / (1 shl 16), 1E-4);
      end;
end;

procedure TTestMath.TestOneOver;
var
  Value : TFixed;
  Index : Integer;
begin
  for Index := 1 to (1 shl 12) do
  begin
    Value := Fixed(1E-1 * Index);
    CheckEquals(10 / Index, OneOver(Value) / (1 shl 16), 1E-3);
  end;
end;

procedure TTestMath.TestFixedSqr;
var
  Value : TFixed;
  Index : Integer;
begin
  for Index := 0 to (1 shl 12) do
  begin
    Value := Fixed(1E-2 * Index);
    CheckEquals(Sqr(1E-2 * Index), FixedSqr(Value) / (1 shl 16), 1E-3);
  end;
end;

procedure TTestMath.TestFixedSqrtLP;
var
  Value : TFixed;
  Index : Integer;
begin
  for Index := 0 to (1 shl 12) do
  begin
    Value := Fixed(1E-2 * Index);
    CheckEquals(Sqrt(1E-2 * Index), FixedSqrtLP(Value) / (1 shl 16), 1E-2);
  end;
end;

procedure TTestMath.TestFixedSqrtHP;
var
  Value : TFixed;
  Index : Integer;
begin
  for Index := 0 to (1 shl 12) do
  begin
    Value := Fixed(1E-2 * Index);
    CheckEquals(Sqrt(1E-2 * Index), FixedSqrtHP(Value) / (1 shl 16), 1E-3);
  end;
end;


// ----------------------------------------------------------------------------
//
// DUnit compatibility for FPC
//
// ----------------------------------------------------------------------------
{$IFDEF FPC}
procedure RegisterTest(ATest: TTest);
begin
  testregistry.RegisterTest('', ATest);
end;
{$ENDIF}

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------

initialization
//  RegisterTest(TTestLowLevel.Suite);

  RegisterTest(TTestLowLevelPas.Suite);
{$if not defined(PUREPASCAL)}
  RegisterTest(TTestLowLevelAsm.Suite);
  if isMMX in GR32_System.CPU.InstructionSupport then
    RegisterTest(TTestLowLevelMMX.Suite);
  if isSSE2 in GR32_System.CPU.InstructionSupport then
    RegisterTest(TTestLowLevelSSE2.Suite);
  if isSSE41 in GR32_System.CPU.InstructionSupport then
    RegisterTest(TTestLowLevelSSE41.Suite);
{$ifend}

  RegisterTest(TTestMath.Suite);
end.

