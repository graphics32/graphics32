unit TestLowLevel;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Version: MPL 1.1 or LGPL 2.1 with linking exception                       //
//                                                                            //
//  The contents of this file are subject to the Mozilla Public License       //
//  Version 1.1 (the "License"); you may not use this file except in          //
//  compliance with the License. You may obtain a copy of the License at      //
//  http://www.mozilla.org/MPL/                                               //
//                                                                            //
//  Software distributed under the License is distributed on an "AS IS"       //
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   //
//  License for the specific language governing rights and limitations under  //
//  the License.                                                              //
//                                                                            //
//  Alternatively, the contents of this file may be used under the terms of   //
//  the Free Pascal modified version of the GNU Lesser General Public         //
//  License Version 2.1 (the "FPC modified LGPL License"), in which case the  //
//  provisions of this license are applicable instead of those above.         //
//  Please see the file LICENSE.txt for additional information concerning     //
//  this license.                                                             //
//                                                                            //
//  The code is part of the Delphi ASIO & VST Project                         //
//                                                                            //
//  The initial developer of this code is Christian-W. Budde                  //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2010-2011        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\..\Source\GR32.inc}

{-$DEFINE CheckNegativeInteger}

uses
  {$IFDEF FPC} fpcunit, testregistry, {$ELSE} TestFramework, Windows,
  {$ENDIF} Controls, Types, Classes, SysUtils, Messages, Graphics,
  GR32, GR32_LowLevel, GR32_Math, GR32_Bindings;

type
  TTestLowLevel = class(TTestCase)
  published
    procedure TestClamp;
    procedure TestFillWord;
    procedure TestFillLongWord;
    procedure TestMoveLongword;
    procedure TestMoveWord;
    procedure TestStackMemory;
    procedure TestConstrain;
    procedure TestMinMax;
    procedure TestWrap;
    procedure TestMirror;
    procedure TestSAR;
  end;

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
    procedure TestHypot;
    procedure TestFastSqrt;
    procedure TestFastSqrtBab1;
    procedure TestFastSqrtBab2;
    procedure TestFastInvSqrt;
    procedure TestMulDiv;
    procedure TestPowerOf2;
    procedure TestAverage;
    procedure TestSign;
  end;

implementation

uses
  Math, GR32_System;

{ TTestLowLevel }

procedure TTestLowLevel.TestClamp;
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
  for Index := -(1 shl 16) to (1 shl 16) do
  begin
    CheckEquals(Max(Min(Index, 108), 20), (Abs(Index - 20) + 128 - Abs(Index - 108)) shr 1);
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
  finally
    FreeMem(Data[0]);
    FreeMem(Data[1]);
  end;
end;

procedure TTestLowLevel.TestSAR;
const
  CValue: Integer = $7ADE4BE1;
begin
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

procedure TTestLowLevel.TestWrap;
begin
  CheckEquals(50, Wrap(50, 100));
  CheckEquals(49, Wrap(150, 100));
  CheckEquals(50, Wrap(150, 99));
  CheckEquals(64, WrapPow2(64, 127));
  CheckEquals(64, WrapPow2(192, 127));
  CheckEquals(32, WrapPow2(160, 127));
end;


{ TTestMath }

procedure TTestMath.TestAverage;
var
  X, Y : Integer;
begin
  for X := 0 to (1 shl 10) do
    for Y := 0 to (1 shl 10) do
      CheckEquals((X + Y) div 2, Average(X, Y));
end;

procedure TTestMath.TestHypot;
var
  X, Y : Integer;
begin
  for X := 0 to (1 shl 11) do
    for Y := 0 to (1 shl 11) do
    begin
      CheckEquals(Round(Math.Hypot(X, Y)), GR32_Math.Hypot(X, Y),
        'Error at X = ' + IntToStr(X) + ' and Y = ' + IntToStr(Y));
      CheckEquals(Math.Hypot(X, 1E-4 * Y), GR32_Math.Hypot(X, 1E-4 * Y), 1E-8,
        'Error at X = ' + IntToStr(X) + ' and Y = ' + IntToStr(Y));
    end;
end;

procedure TTestMath.TestPowerOf2;
var
  Index : Integer;
begin
  // check IsPowerOf
  for Index := 0 to 31 do
  begin
    CheckTrue(IsPowerOf2(1 shl Index));
    if Index > 0 then
    begin
      if Index > 1 then
        CheckTrue(not IsPowerOf2((1 shl Index) - 1));
      if Index < 31 then
        CheckTrue(not IsPowerOf2((1 shl Index) + 1));
    end;
  end;

  // check NextPowerOf
  for Index := 0 to 30 do
  begin
    CheckEquals(1 shl Index, NextPowerOf2(1 shl Index));
    if Index > 0 then
    begin
      if Index > 1 then
        CheckEquals(1 shl Index, NextPowerOf2((1 shl Index) - 1));
      if Index < 30 then
        CheckEquals(1 shl Index, NextPowerOf2((1 shl Index) + 1) shr 1);
    end;
  end;

  // check PrevPowerOf
  for Index := 0 to 30 do
  begin
    CheckEquals(1 shl Index, PrevPowerOf2(1 shl Index));
    if Index > 0 then
    begin
      if Index > 1 then
        CheckEquals(1 shl Index, PrevPowerOf2((1 shl Index) - 1) shl 1);
      if Index < 30 then
        CheckEquals(1 shl Index, PrevPowerOf2((1 shl Index) + 1));
    end;
  end;
end;

procedure TTestMath.TestSign;
var
  Index : Integer;
begin
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
begin
  for Index := 1 to (1 shl 8) do
    CheckEquals(Sqrt(Index), FastSqrt(Index), 1E-1);
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
begin
  for Multiplicand := Low(Byte) to High(Byte) do
    for Multiplier := Low(Byte) to High(Byte) do
      for Divisor := 1 to High(Byte) do
        CheckEquals(Round(Multiplicand * Multiplier / Divisor),
          MulDiv(Multiplicand, Multiplier, Divisor));
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
  for Index := -(1 shl 7) to (1 shl 7) do
  begin
    Value.Int := Abs(Index);
    Value.Frac := Index;
    if Index < 0 then
      CheckEquals(Abs(Index) + 1, FixedRound(Value.Fixed))
    else
      CheckEquals(Abs(Index), FixedRound(Value.Fixed))
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

initialization
  RegisterTest(TTestLowLevel.Suite);
  RegisterTest(TTestMath.Suite);

end.
