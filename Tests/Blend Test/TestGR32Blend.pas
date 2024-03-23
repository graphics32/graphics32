unit TestGR32Blend;

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

{$define FAIL_NOT_IMPLEMENTED} // Fail test if function isn't implemented

uses
{$IFDEF FPC}
  fpcunit, testregistry,
{$ELSE}
  TestFramework,
{$ENDIF}
{$ifdef Windows}
  Windows,
{$endif}
  Controls, Types, Classes, SysUtils, Messages, Graphics,
  System.Rtti,
  System.TypInfo,
  GR32,
  GR32_Blend,
  GR32_Bindings;

{$M+}

type
  MaxErrorAttribute = class(TCustomAttribute)
  private
    FValue: integer;
  published
    constructor Create(AValue: integer);
  public
    property Value: integer read FValue;
  end;

  MaxErrorCountAttribute = class(TCustomAttribute)
  private
    FValue: integer;
  published
    constructor Create(AValue: integer);
  public
    property Value: integer read FValue;
  end;

type
  TTestBlendTables = class(TTestCase)
  published
    procedure TestAlphaTableAlignment;
    procedure TestAlphaTable;
    procedure TestDivisionTable;
    procedure TestMultiplicationTable;
  end;

type
  TCheckCombine = reference to procedure (ForeGround, Background: TColor32; Weight: Cardinal);

  TCustomTestBlendModes = class abstract(TTestCase)
  strict private
    FForeground : PColor32Array;
    FBackground : PColor32Array;
    FReference: PColor32Array;
  protected
    FMaxDifferenceLimit: Byte;
    FTestCount: integer;
    FErrorCount: integer;
    FErrorCountLimit: integer;
    FMaxAbsoluteDifference: integer;
    FDifferenceCount: integer;
    FDifferenceSum: integer;

    procedure DoCheckColor(ExpectedColor32, ActualColor32: TColor32Entry; MaxDifferenceLimit: Byte; const AExtra: string; const AExtraParams: array of const); overload;
    procedure CheckColor(ExpectedColor32, ActualColor32: TColor32Entry; MaxDifferenceLimit: Byte = 1); overload;
    procedure CheckColor(ExpectedColor32, ActualColor32: TColor32Entry; MaxDifferenceLimit: Byte; const AExtra: string; const AExtraParams: array of const); overload;

    function Rebind(FunctionID: Integer; RequireImplementation: boolean = True): boolean;
    class function PriorityProc: TFunctionPriority; virtual; abstract;

    procedure TestBlendReg; virtual;
    procedure TestBlendRegEx; virtual;
    procedure TestBlendMem; virtual;
    procedure TestBlendMemEx; virtual;
    procedure TestBlendLine; virtual;
    procedure TestBlendLineEx; virtual;
    procedure TestBlendLine1; virtual;
    procedure TestCombineReg; virtual;
    procedure TestCombineMem; virtual;
    procedure TestCombineLine; virtual;
    procedure TestMergeReg; virtual;
    procedure TestMergeRegEx; virtual;
    procedure TestMergeMem; virtual;
    procedure TestMergeMemEx; virtual;
    procedure TestMergeLine; virtual;
    procedure TestMergeLineEx; virtual;

    procedure DoCheckCombine(CheckCombineProc: TCheckCombine);
  public
    procedure SetUp; override;
    procedure TearDown; override;
//  published
{$IFNDEF FPC}
    procedure PerformanceTest; virtual;
{$ENDIF}
  end;

  TTestBlendModesPas = class(TCustomTestBlendModes)
  private
    class function PriorityProcPas(Info: PFunctionInfo): Integer; static;
  protected
    class function PriorityProc: TFunctionPriority; override;
  published
    [MaxError(1)]
    procedure TestBlendReg; override;
    [MaxError(2)]
    procedure TestBlendRegEx; override;
    [MaxError(1)]
    procedure TestBlendMem; override;
    [MaxError(2)]
    procedure TestBlendMemEx; override;
    [MaxError(1)]
    procedure TestBlendLine; override;
    [MaxError(1)]
    procedure TestBlendLineEx; override;
    [MaxError(1)]
    procedure TestBlendLine1; override;
    [MaxError(1)]
    procedure TestCombineReg; override;
    [MaxError(1), MaxErrorCount(-1)]
    procedure TestCombineMem; override;
    [MaxError(1)]
    procedure TestCombineLine; override;
    [MaxError(4)]
    procedure TestMergeReg; override;
    [MaxError(4)]
    procedure TestMergeRegEx; override;
    [MaxError(4)]
    procedure TestMergeMem; override;
    [MaxError(4)]
    procedure TestMergeMemEx; override;
    [MaxError(4)]
    procedure TestMergeLine; override;
    [MaxError(4)]
    procedure TestMergeLineEx; override;
  end;

  TTestBlendModesAsm = class(TCustomTestBlendModes)
  private
    class function PriorityProcAsm(Info: PFunctionInfo): Integer; static;
  protected
    class function PriorityProc: TFunctionPriority; override;
  published
    [MaxError(1)]
    procedure TestBlendReg; override;
    [MaxError(2)]
    procedure TestBlendRegEx; override;
    [MaxError(1)]
    procedure TestBlendMem; override;
    [MaxError(2)]
    procedure TestBlendMemEx; override;
    [MaxError(1)]
    procedure TestBlendLine; override;
    [MaxError(0)]
    procedure TestBlendLineEx; override;
    [MaxError(1)]
    procedure TestBlendLine1; override;
    [MaxError(2)]
    procedure TestCombineReg; override;
    [MaxError(2)]
    procedure TestCombineMem; override;
    [MaxError(0)]
    procedure TestCombineLine; override;
    [MaxError(9)] // Pretty bad :-(
    procedure TestMergeReg; override;
    [MaxError(0)]
    procedure TestMergeRegEx; override;
    [MaxError(0)]
    procedure TestMergeMem; override;
    [MaxError(0)]
    procedure TestMergeMemEx; override;
    [MaxError(0)]
    procedure TestMergeLine; override;
    [MaxError(0)]
    procedure TestMergeLineEx; override;
  end;

  TTestBlendModesMMX = class(TCustomTestBlendModes)
  private
    class function PriorityProcMMX(Info: PFunctionInfo): Integer; static;
  protected
    class function PriorityProc: TFunctionPriority; override;
  published
    [MaxError(1)]
    procedure TestBlendReg; override;
    [MaxError(2)]
    procedure TestBlendRegEx; override;
    [MaxError(1)]
    procedure TestBlendMem; override;
    [MaxError(2)]
    procedure TestBlendMemEx; override;
    [MaxError(1)]
    procedure TestBlendLine; override;
    [MaxError(2)]
    procedure TestBlendLineEx; override;
    [MaxError(2)]
    procedure TestBlendLine1; override;
    [MaxError(1)]
    procedure TestCombineReg; override;
    [MaxError(1)]
    procedure TestCombineMem; override;
    [MaxError(1)]
    procedure TestCombineLine; override;
    [MaxError(6)]
    procedure TestMergeReg; override;
    [MaxError(6)]
    procedure TestMergeRegEx; override;
    [MaxError(6)]
    procedure TestMergeMem; override;
    [MaxError(6)]
    procedure TestMergeMemEx; override;
    procedure TestMergeLine; override;
    procedure TestMergeLineEx; override;
  end;

  TTestBlendModesSSE2 = class(TCustomTestBlendModes)
  private
    class function PriorityProcSSE2(Info: PFunctionInfo): Integer; static;
  protected
    class function PriorityProc: TFunctionPriority; override;
  published
    [MaxError(1)]
    procedure TestBlendReg; override;
    [MaxError(2)]
    procedure TestBlendRegEx; override;
    [MaxError(1)]
    procedure TestBlendMem; override;
    [MaxError(2)]
    procedure TestBlendMemEx; override;
    [MaxError(1)]
    procedure TestBlendLine; override;
    [MaxError(2)]
    procedure TestBlendLineEx; override;
    procedure TestBlendLine1; override;
    [MaxError(1)]
    procedure TestCombineReg; override;
    [MaxError(1)]
    procedure TestCombineMem; override;
    procedure TestCombineLine; override;
    [MaxError(1)]
    procedure TestMergeReg; override;
    procedure TestMergeRegEx; override;
    procedure TestMergeMem; override;
    procedure TestMergeMemEx; override;
    procedure TestMergeLine; override;
    procedure TestMergeLineEx; override;
  end;

  TTestBlendModesSSE41 = class(TCustomTestBlendModes)
  private
    class function PriorityProcSSE41(Info: PFunctionInfo): Integer; static;
  protected
    class function PriorityProc: TFunctionPriority; override;
  published
    procedure TestBlendReg; override;
    procedure TestBlendRegEx; override;
    procedure TestBlendMem; override;
    procedure TestBlendMemEx; override;
    procedure TestBlendLine; override;
    procedure TestBlendLineEx; override;
    procedure TestBlendLine1; override;
    procedure TestCombineReg; override;
    [MaxError(1), MaxErrorCount(20)]
    procedure TestCombineMem; override;
    procedure TestCombineLine; override;
    procedure TestMergeReg; override;
    procedure TestMergeRegEx; override;
    procedure TestMergeMem; override;
    procedure TestMergeMemEx; override;
    procedure TestMergeLine; override;
    procedure TestMergeLineEx; override;
  end;

implementation

uses
  Math,
  GR32_System,
  GR32_LowLevel,
  GR32_BlendReference;

constructor MaxErrorAttribute.Create(AValue: integer);
begin
  inherited Create;
  FValue := AValue;
end;

constructor MaxErrorCountAttribute.Create(AValue: integer);
begin
  inherited Create;
  FValue := AValue;
end;

{ TTestBlendModes }

procedure TCustomTestBlendModes.SetUp;
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  RttiMethod: TRttimethod;
  MaxAbsoluteError: MaxErrorAttribute;
  MaxErrorCount: MaxErrorCountAttribute;
begin
  inherited;

  BlendRegistry.RebindAll(True, pointer(PriorityProc));

  FErrorCountLimit := -1;
  FMaxDifferenceLimit := 0;

  RttiContext := TRttiContext.Create;
  try
    RttiType := RttiContext.GetType(ClassType);
    RttiMethod := RttiType.GetMethod(GetName);

    MaxAbsoluteError := RttiMethod.GetAttribute<MaxErrorAttribute>;
    if (MaxAbsoluteError <> nil) then
      FMaxDifferenceLimit := MaxAbsoluteError.Value;

    MaxErrorCount := RttiMethod.GetAttribute<MaxErrorCountAttribute>;
    if (MaxErrorCount <> nil) then
      FErrorCountLimit := MaxErrorCount.Value;
  finally
    RttiContext.Free
  end;

  FTestCount := 0;
  FErrorCount := 0;
  FMaxAbsoluteDifference := 0;
  FDifferenceCount := 0;
  FDifferenceSum := 0;
  GetMem(FForeground, 256 * SizeOf(TColor32));
  GetMem(FBackground, 256 * SizeOf(TColor32));
  GetMem(FReference, 256 * SizeOf(TColor32));
end;

procedure TCustomTestBlendModes.TearDown;
begin
  if (FDifferenceCount > 0) then
    Status(Format(
      'Errors: %.0n = %.1n %% (Limit: %d)'#13+
      'Differences: %.0n'#13+
      'Average difference: %.2n'#13+
      'Max difference: %d (Limit: %d)',
      [FErrorCount*1.0, FErrorCount/FTestCount*100, FErrorCountLimit, FDifferenceCount*1.0, FDifferenceSum/FDifferenceCount, FMaxAbsoluteDifference, FMaxDifferenceLimit]));

  (* This is just for verification that we're testing against the strictest possible criteria
  if (FDifferenceCount < FMaxAbsoluteDifference) then
    Fail(Format('Expected max difference: %d, Actual: %d', [FMaxAbsoluteDifference, FDifferenceCount]));

  if (FErrorCountLimit > 0) and (FErrorCount < FErrorCountLimit) then
    Fail(Format('Expected errors: %d, Actual: %d', [FErrorCountLimit, FErrorCount]));
  *)

  inherited;
  Dispose(FForeground);
  Dispose(FBackground);
  Dispose(FReference);
end;

{$IFNDEF FPC}
procedure TCustomTestBlendModes.DoCheckColor(ExpectedColor32, ActualColor32: TColor32Entry; MaxDifferenceLimit: Byte; const AExtra: string; const AExtraParams: array of const);
var
  Msg, MsgExtra: string;
  DifferenceA: integer;
  DifferenceR: integer;
  DifferenceG: integer;
  DifferenceB: integer;
  MaxAbsoluteDifference: integer;
begin
  Inc(FTestCount);
  FCheckCalled := True;

  DifferenceA := ActualColor32.A - ExpectedColor32.A;
  DifferenceR := ActualColor32.R - ExpectedColor32.R;
  DifferenceG := ActualColor32.G - ExpectedColor32.G;
  DifferenceB := ActualColor32.B - ExpectedColor32.B;

  MaxAbsoluteDifference := Max(Abs(DifferenceA), Abs(DifferenceR));
  MaxAbsoluteDifference := Max(MaxAbsoluteDifference, Abs(DifferenceG));
  MaxAbsoluteDifference := Max(MaxAbsoluteDifference, Abs(DifferenceB));

  FDifferenceSum := FDifferenceSum + DifferenceA + DifferenceR + DifferenceG + DifferenceB;

  if (MaxAbsoluteDifference > 0) then
  begin
    Inc(FErrorCount);

    FMaxAbsoluteDifference := Max(FMaxAbsoluteDifference, MaxAbsoluteDifference);

    if (DifferenceA <> 0) then
      Inc(FDifferenceCount);
    if (DifferenceR <> 0) then
      Inc(FDifferenceCount);
    if (DifferenceG <> 0) then
      Inc(FDifferenceCount);
    if (DifferenceB <> 0) then
      Inc(FDifferenceCount);

    if (MaxAbsoluteDifference > MaxDifferenceLimit) or ((FErrorCountLimit <> -1) and (FErrorCount > FErrorCountLimit)) then
    begin
      if (AExtra <> '') then
        MsgExtra := Format(AExtra, AExtraParams)
      else
        MsgExtra := '';

      Msg := Format('Expected:%.8X, Actual:%.8X, Dif:%.2X%.2X%.2X%.2X %s',
        [ExpectedColor32.ARGB, ActualColor32.ARGB, Abs(DifferenceA), Abs(DifferenceR), Abs(DifferenceG), Abs(DifferenceB), MsgExtra]);
      Fail(Msg, ReturnAddress);
    end else
      ;//Status(Format('Dif:%.8X', [DifColor.ARGB]));
  end;
end;

procedure TCustomTestBlendModes.CheckColor(ExpectedColor32, ActualColor32: TColor32Entry; MaxDifferenceLimit: Byte);
begin
  DoCheckColor(ExpectedColor32, ActualColor32, MaxDifferenceLimit, '', []);
end;

procedure TCustomTestBlendModes.CheckColor(ExpectedColor32, ActualColor32: TColor32Entry; MaxDifferenceLimit: Byte; const AExtra: string; const AExtraParams: array of const);
begin
  DoCheckColor(ExpectedColor32, ActualColor32, MaxDifferenceLimit, AExtra, AExtraParams);
end;

procedure TCustomTestBlendModes.PerformanceTest;
var
  Start, Stop, Freq : Int64;
  BlendColor32      : TColor32Entry;
  Index             : Integer;
begin
  BlendRegistry.RebindAll(pointer(PriorityProc));

  BlendColor32.ARGB := clWhite32;
  BlendColor32.A := $5A;

  QueryPerformanceFrequency(Freq);
  QueryPerformanceCounter(Start);

  for Index := 0 to $7FFFFFF do
    BlendReg(BlendColor32.ARGB, clBlack32);

  EMMS;

  for Index := 0 to $7FFFFFF do
  begin
    BlendReg(BlendColor32.ARGB, clBlack32);
    EMMS;
  end;

  QueryPerformanceCounter(Stop);

  Status(Format('Performance: %.3n', [1000 * (Stop - Start) / Freq]));
  Check(True);
end;

function TCustomTestBlendModes.Rebind(FunctionID: Integer; RequireImplementation: boolean): boolean;
begin
  Result := BlendRegistry.Rebind(FunctionID, pointer(PriorityProc));
  if (RequireImplementation) and (not Result) then
{$ifdef FAIL_NOT_IMPLEMENTED}
    // Not really an error but we need to indicate that nothing was tested
    Fail('Not implemented');
{$else}
    Enabled := False;
{$endif}
end;

{$ENDIF}


procedure TCustomTestBlendModes.TestBlendReg;
var
  BlendColor32    : TColor32Entry;
  CombinedColor32 : TColor32Entry;
  ExpectedColor32 : TColor32Entry;
  RefIndex, Index : Integer;
begin
  Rebind(FID_EMMS, False);
  if (not Rebind(FID_BLENDREG)) then
  begin
    Check(True);
    Exit;
  end;

  // static test
  BlendColor32.A := $1A;
  BlendColor32.B := $2B;
  BlendColor32.G := $3C;
  BlendColor32.R := $4D;

  ExpectedColor32.ARGB := BlendReg_Reference(BlendColor32.ARGB, BlendColor32.ARGB);
  CombinedColor32.ARGB := BlendReg(BlendColor32.ARGB, BlendColor32.ARGB);

  EMMS;

  CombinedColor32.A := $FF;
  ExpectedColor32.A := $FF;
  CheckColor(ExpectedColor32, CombinedColor32, FMaxDifferenceLimit);

  for RefIndex := 0 to High(Byte) do
  begin
    BlendColor32.B := RefIndex;
    BlendColor32.G := RefIndex shr 1;
    BlendColor32.R := RefIndex shr 2;
    for Index := 0 to High(Byte) do
    begin
      BlendColor32.A := Index;
      ExpectedColor32.ARGB := BlendReg_Reference(BlendColor32.ARGB, clBlack32);
      CombinedColor32.ARGB := BlendReg(BlendColor32.ARGB, clBlack32);
      CombinedColor32.A := $FF;
      ExpectedColor32.A := $FF;

      EMMS;

      CheckColor(ExpectedColor32, CombinedColor32, FMaxDifferenceLimit);
    end;
  end;
end;

procedure TCustomTestBlendModes.TestBlendRegEx;
var
  BlendColor32    : TColor32Entry;
  CombinedColor32 : TColor32Entry;
  ExpectedColor32 : TColor32Entry;
  RefIndex, Index : Integer;
  MasterIndex     : Integer;
begin
  Rebind(FID_EMMS, False);
  if (not Rebind(FID_BLENDREGEX)) then
  begin
    Check(True);
    Exit;
  end;

  // static test
  BlendColor32.A := $F8;
  BlendColor32.B := $A1;
  BlendColor32.G := $50;
  BlendColor32.R := $28;

  ExpectedColor32.ARGB := BlendRegEx_Reference(BlendColor32.ARGB, clBlack32, TColor32(7 shl 5));
  CombinedColor32.ARGB := BlendRegEx(BlendColor32.ARGB, clBlack32, TColor32(7 shl 5));
  CombinedColor32.A := $FF;
  ExpectedColor32.A := $FF;

  EMMS;

  CheckColor(ExpectedColor32, CombinedColor32, FMaxDifferenceLimit);

  for RefIndex := 0 to High(Byte) do
  begin
    BlendColor32.B := RefIndex;
    BlendColor32.G := RefIndex shr 1;
    BlendColor32.R := RefIndex shr 2;
    for Index := 0 to High(Byte) do
    begin
      for MasterIndex := 0 to 7 do
      begin
        BlendColor32.A := Index;
        ExpectedColor32.ARGB := BlendRegEx_Reference(BlendColor32.ARGB, clBlack32, TColor32(MasterIndex shl 5));
        CombinedColor32.ARGB := BlendRegEx(BlendColor32.ARGB, clBlack32, TColor32(MasterIndex shl 5));
        CombinedColor32.A := $FF;
        ExpectedColor32.A := $FF;

        EMMS;

        CheckColor(ExpectedColor32, CombinedColor32, FMaxDifferenceLimit);
      end;
    end;
  end;
end;

procedure TCustomTestBlendModes.TestBlendMem;
var
  BlendColor32    : TColor32Entry;
  CombinedColor32 : TColor32Entry;
  ExpectedColor32 : TColor32Entry;
  RefIndex, Index : Integer;
begin
  Rebind(FID_EMMS, False);
  if (not Rebind(FID_BLENDMEM)) then
  begin
    Check(True);
    Exit;
  end;

  for RefIndex := 0 to High(Byte) do
  begin
    BlendColor32.B := RefIndex;
    BlendColor32.G := RefIndex shr 1;
    BlendColor32.R := RefIndex shr 2;

    for Index := 0 to High(Byte) do
    begin
      BlendColor32.A := Index;

      ExpectedColor32.ARGB := clBlack32;
      BlendMem_Reference(BlendColor32.ARGB, ExpectedColor32.ARGB);

      CombinedColor32.ARGB := clBlack32;
      BlendMem(BlendColor32.ARGB, CombinedColor32.ARGB);

      EMMS;

      // Documentation states that background is assumed to be opaque but then continues
      // to explain what happens when it's not. This is probably a doc bug.
      // BlendMem_Pas/Asm forces the Alpha to 255 but BlendMem_Reference doesn't.
      ExpectedColor32.A := $FF;
      CombinedColor32.A := $FF;

      CheckColor(ExpectedColor32, CombinedColor32, FMaxDifferenceLimit);
    end;
  end;
end;

procedure TCustomTestBlendModes.TestBlendMemEx;
var
  BlendColor32    : TColor32Entry;
  CombinedColor32 : TColor32Entry;
  ExpectedColor32 : TColor32Entry;
  RefIndex, Index : Integer;
  MasterIndex     : Integer;
begin
  Rebind(FID_EMMS, False);
  if (not Rebind(FID_BLENDMEMEX)) then
  begin
    Check(True);
    Exit;
  end;

  for RefIndex := 0 to High(Byte) do
  begin
    BlendColor32.B := RefIndex;
    BlendColor32.G := RefIndex shr 1;
    BlendColor32.R := RefIndex shr 2;
    for Index := 0 to High(Byte) do
    begin
      for MasterIndex := 0 to 7 do
      begin
        BlendColor32.A := Index;

        ExpectedColor32.ARGB := clBlack32;
        BlendMemEx_Reference(BlendColor32.ARGB, ExpectedColor32.ARGB, TColor32(MasterIndex shl 5));

        CombinedColor32.ARGB := clBlack32;
        BlendMemEx(BlendColor32.ARGB, CombinedColor32.ARGB, TColor32(MasterIndex shl 5));

        EMMS;

        // Documentation states that background is assumed to be opaque but then continues
        // to explain what happens when it's not. This is probably a doc bug.
        // BlendMemEx_Pas/Asm forces the Alpha to 255 but BlendMemEx_Reference doesn't.
        ExpectedColor32.A := $FF;
        CombinedColor32.A := $FF;

        CheckColor(ExpectedColor32, CombinedColor32, FMaxDifferenceLimit,
          '(RefIndex: %d, Index: %d, Master: %d, BlendMemEx(FG:%.8X, BG:%.8X, Master:%d)',
          [RefIndex, Index, MasterIndex shl 5, BlendColor32.ARGB, clBlack32, MasterIndex shl 5]);
      end;
    end;
  end;
end;

procedure TCustomTestBlendModes.TestBlendLine;
var
  CombinedColor32 : TColor32Entry;
  ExpectedColor32 : TColor32Entry;
  Index           : Integer;
begin
  Rebind(FID_EMMS, False);
  if (not Rebind(FID_BLENDLINE)) then
  begin
    Check(True);
    Exit;
  end;

  for Index := 0 to High(Byte) do
  begin
    FBackground^[Index] := clBlack32;
    FForeground^[Index] := clWhite32;
    TColor32Entry(FForeground^[Index]).A := Index;
  end;

  BlendLine(PColor32(FForeground), PColor32(FBackground), 256);
  EMMS;

  for Index := 0 to High(Byte) do
  begin
    ExpectedColor32.ARGB := clBlack32;

    BlendMem_Reference(FForeground^[Index], ExpectedColor32.ARGB);

    CombinedColor32.ARGB := FBackground^[Index];
    // Ignore alpha for Blend
    ExpectedColor32.A := 0;
    CombinedColor32.A := 0;

    CheckColor(ExpectedColor32, CombinedColor32, FMaxDifferenceLimit);
  end;
end;

procedure TCustomTestBlendModes.TestBlendLine1;
var
  CombinedColor32 : TColor32Entry;
  ExpectedColor32 : TColor32Entry;
  Index           : Integer;
begin
  Rebind(FID_EMMS, False);
  if (not Rebind(FID_BLENDLINE1)) then
  begin
    Check(True);
    Exit;
  end;

  for Index := 0 to High(Byte) do
  begin
    FBackground^[Index] := clBlack32;
    TColor32Entry(FBackground^[Index]).R := Index;
    TColor32Entry(FBackground^[Index]).G := High(Byte) - Index;
  end;

  BlendLine1(clTrWhite32, PColor32(FBackground), 256);

  EMMS;

  for Index := 0 to High(Byte) do
  begin
    ExpectedColor32.ARGB := clBlack32;
    TColor32Entry(ExpectedColor32).R := Index;
    TColor32Entry(ExpectedColor32).G := High(Byte) - Index;

    BlendMem_Reference(clTrWhite32, ExpectedColor32.ARGB);

    CombinedColor32.ARGB := FBackground^[Index];
    CombinedColor32.A := $FF;
    ExpectedColor32.A := $FF;


    CheckColor(ExpectedColor32, CombinedColor32, FMaxDifferenceLimit);
  end;
end;

procedure TCustomTestBlendModes.TestBlendLineEx;
var
  ActualColor32      : TColor32Entry;
  ExpectedColor32    : TColor32Entry;
  Index, MasterIndex : Integer;
begin
  Rebind(FID_EMMS, False);
  if (not Rebind(FID_BLENDLINEEX)) then
  begin
    Check(True);
    Exit;
  end;

  for Index := 0 to High(Byte) do
  begin
    FForeground^[Index] := clWhite32;
    TColor32Entry(FForeground^[Index]).A := Index;
  end;

  for MasterIndex := 0 to 7 do
  begin
    for Index := 0 to High(Byte) do
    begin
      FBackground^[Index] := clBlack32;
      FReference^[Index] := clBlack32;
    end;

    BlendLineEx_Reference(PColor32(FForeground), PColor32(FReference), 256, TColor32(MasterIndex shl 5));
    BlendLineEx(PColor32(FForeground), PColor32(FBackground), 256, TColor32(MasterIndex shl 5));

    EMMS;

    for Index := 0 to High(Byte) do
    begin
      ExpectedColor32.ARGB := FReference^[Index];
      ExpectedColor32.A := 0;

      ActualColor32.ARGB := FBackground^[Index];
      ActualColor32.A := 0;

      CheckColor(ExpectedColor32, ActualColor32, FMaxDifferenceLimit, 'Index: %d, BlendMemEx(FG:%.8X, BG:%.8X, Master:%d)', [Index, FForeground^[Index], clBlack32, MasterIndex shl 5]);
    end;
  end;
end;

procedure TCustomTestBlendModes.DoCheckCombine(CheckCombineProc: TCheckCombine);
var
  BlendColor32    : TColor32Entry;
  RefIndex, Index : Integer;
begin
  // Edge cases
  CheckCombineProc($FF010101, clBlack32, 127);
  CheckCombineProc($FF010101, clBlack32, 128);
  CheckCombineProc($FF010101, clWhite32, 127);
  CheckCombineProc($FF010101, clWhite32, 128);

  CheckCombineProc(clBlack32, clBlack32, 0);
  CheckCombineProc(clBlack32, clBlack32, 255);
  CheckCombineProc(clBlack32, clBlack32, 1);
  CheckCombineProc(clBlack32, clBlack32, 254);

  CheckCombineProc(clWhite32, clWhite32, 0);
  CheckCombineProc(clWhite32, clWhite32, 255);
  CheckCombineProc(clWhite32, clWhite32, 1);
  CheckCombineProc(clWhite32, clWhite32, 254);

  CheckCombineProc(clWhite32, clBlack32, 0);
  CheckCombineProc(clWhite32, clBlack32, 255);
  CheckCombineProc(clWhite32, clBlack32, 1);
  CheckCombineProc(clWhite32, clBlack32, 254);

  CheckCombineProc(clBlack32, clWhite32, 0);
  CheckCombineProc(clBlack32, clWhite32, 255);
  CheckCombineProc(clBlack32, clWhite32, 1);
  CheckCombineProc(clBlack32, clWhite32, 254);


  for RefIndex := 0 to High(Byte) do
  begin
    BlendColor32.A := $FF;
    BlendColor32.B := RefIndex;
    BlendColor32.G := RefIndex shr 1;
    BlendColor32.R := RefIndex shr 2;
    for Index := 0 to High(Byte) do
      CheckCombineProc(BlendColor32.ARGB, clBlack32, Index);
  end;

  BlendColor32.A := 0;
  BlendColor32.B := 0;
  BlendColor32.G := High(Byte);
  BlendColor32.R := High(Byte);
  for RefIndex := 0 to High(Byte) do
  begin
    Inc(BlendColor32.A);
    Inc(BlendColor32.B);
    Dec(BlendColor32.G);
    Dec(BlendColor32.R);
    for Index := 0 to High(Byte) do
      CheckCombineProc(BlendColor32.ARGB, not(BlendColor32.ARGB), Index);
  end;
end;

procedure TCustomTestBlendModes.TestCombineReg;
begin
  Rebind(FID_EMMS, False);
  if (not Rebind(FID_COMBINEREG)) then
  begin
    Check(True);
    Exit;
  end;

  DoCheckCombine(
    procedure(ForeGround, Background: TColor32; Weight: Cardinal)
    var
      ForegroundColor : TColor32Entry absolute Foreground;
      BackgroundColor : TColor32Entry absolute Background;
      ActualColor32   : TColor32Entry;
      ExpectedColor32 : TColor32Entry;
    begin
      ExpectedColor32.ARGB := CombineReg_Reference(ForeGround, Background, Weight);
      ActualColor32.ARGB := CombineReg(ForeGround, Background, Weight);
      EMMS;

      CheckColor(ExpectedColor32, ActualColor32, FMaxDifferenceLimit, 'CombineReg(FG:%.8X, BG:%.8X, Weight:%d)', [ForeGround, Background, Weight]);
    end
  );
end;

procedure TCustomTestBlendModes.TestCombineMem;
begin
  Rebind(FID_EMMS, False);
  if (not Rebind(FID_COMBINEMEM)) then
  begin
    Check(True);
    Exit;
  end;

  DoCheckCombine(
    procedure(ForeGround, Background: TColor32; Weight: Cardinal)
    var
      ForegroundColor : TColor32Entry absolute Foreground;
      ActualColor32   : TColor32Entry;
      ExpectedColor32 : TColor32Entry;
    begin
      ExpectedColor32.ARGB := Background;
      CombineMem_Reference(ForeGround, ExpectedColor32.ARGB, Weight);
      ActualColor32.ARGB := Background;
      CombineMem(ForeGround, ActualColor32.ARGB, Weight);

      EMMS;

      CheckColor(ExpectedColor32, ActualColor32, FMaxDifferenceLimit, 'Combinemem(FG:%.8X, BG:%.8X, Weight:%d)', [ForeGround, Background, Weight]);
    end
  );

end;

procedure TCustomTestBlendModes.TestCombineLine;
var
  ExpectedColor32 : TColor32Entry;
  Index           : Integer;
begin
  Rebind(FID_EMMS, False);
  if (not Rebind(FID_COMBINELINE)) then
  begin
    Check(True);
    Exit;
  end;

  for Index := 0 to High(Byte) do
  begin
    FBackground^[Index] := clBlack32;
    FForeground^[Index] := clWhite32;
    TColor32Entry(FForeground^[Index]).A := Index;
  end;

  CombineLine(PColor32(FForeground), PColor32(FBackground), 256, $FF);

  EMMS;

  for Index := 0 to High(Byte) do
  begin
    ExpectedColor32.ARGB := (Index shl 24) or $FFFFFF;

    CheckColor(ExpectedColor32, TColor32Entry(FBackground^[Index]), FMaxDifferenceLimit, 'CombineLine(FG:%.8X, BG:%.8X, Weight:%d)', [FForeground^[Index], FBackground^[Index], 255]);
  end;
end;

procedure TCustomTestBlendModes.TestMergeReg;
var
  MergeColor32    : TColor32Entry;
  BlendColor32    : TColor32Entry;
  CombinedColor32 : TColor32Entry;
  ExpectedColor32 : TColor32Entry;
  RefIndex, Index : Integer;
  AlphaIndex      : Integer;
const
  CAlphaValues : array [0..14] of Byte = ($00, $01, $20, $40, $41, $60, $7F, $80, $9F, $BF, $C0, $C1, $DF, $FE, $FF);
begin
  Rebind(FID_EMMS, False);
  if (not Rebind(FID_MERGEREG)) then
  begin
    Check(True);
    Exit;
  end;

  // static test
  MergeColor32.ARGB := clBlack32;
  MergeColor32.A := 3;
  BlendColor32.B := $D7;
  BlendColor32.G := $6B;
  BlendColor32.R := $35;
  BlendColor32.A := $10;
  ExpectedColor32.ARGB := MergeReg_Reference(BlendColor32.ARGB, MergeColor32.ARGB);
  CombinedColor32.ARGB := MergeReg(BlendColor32.ARGB, MergeColor32.ARGB);

  EMMS;

  CheckColor(ExpectedColor32, CombinedColor32, FMaxDifferenceLimit, 'MergeReg(FG: %.8X, BG: %.8X)', [BlendColor32.ARGB, MergeColor32.ARGB]);

  // sample test
  MergeColor32.ARGB := clBlack32;
  for RefIndex := 0 to High(Byte) do
  begin
    BlendColor32.B := RefIndex;
    BlendColor32.G := RefIndex shr 1;
    BlendColor32.R := RefIndex shr 2;
    for AlphaIndex := 0 to Length(CAlphaValues) - 1 do
    begin
      BlendColor32.A := AlphaIndex shl 4;
      for Index := 0 to High(Byte) do
      begin
        MergeColor32.A := Index;

        ExpectedColor32.ARGB := MergeReg_Reference(BlendColor32.ARGB, MergeColor32.ARGB);
        CombinedColor32.ARGB := MergeReg(BlendColor32.ARGB, MergeColor32.ARGB);

        EMMS;

        CheckColor(ExpectedColor32, CombinedColor32, FMaxDifferenceLimit,
          '(RefIndex: %d, Alpha Index: %d, MergeReg(FG: %.8X, BG: %.8X) )', [RefIndex, AlphaIndex, BlendColor32.ARGB, MergeColor32.ARGB]);
      end;
    end;
  end;
end;

procedure TCustomTestBlendModes.TestMergeRegEx;
var
  MergeColor32    : TColor32Entry;
  BlendColor32    : TColor32Entry;
  CombinedColor32 : TColor32Entry;
  ExpectedColor32 : TColor32Entry;
  RefIndex, Index : Integer;
  AlphaIndex      : Integer;
  MasterIndex     : Integer;
const
  CAlphaValues : array [0..14] of Byte = ($00, $01, $20, $40, $41, $60, $7F, $80, $9F, $BF, $C0, $C1, $DF, $FE, $FF);
begin
  Rebind(FID_EMMS, False);
  Rebind(FID_MERGEREG);
  if (not Rebind(FID_MERGEREGEX)) then
  begin
    Check(True);
    Exit;
  end;

  BlendColor32.ARGB := TColor32($1002050B);
  MergeColor32.ARGB := TColor32($01000000);
  ExpectedColor32.ARGB := MergeRegEx_Reference(BlendColor32.ARGB, MergeColor32.ARGB, 128);
  CombinedColor32.ARGB := MergeRegEx(BlendColor32.ARGB, MergeColor32.ARGB, 128);

  EMMS;

  CheckColor(ExpectedColor32, CombinedColor32, FMaxDifferenceLimit, 'MergeRegEx(FG: %.8X, BG: %.8X, Master: %d)', [BlendColor32.ARGB, MergeColor32.ARGB, 128]);

  MergeColor32.ARGB := clBlack32;
  for RefIndex := 0 to High(Byte) do
  begin
    BlendColor32.B := RefIndex;
    BlendColor32.G := RefIndex shr 1;
    BlendColor32.R := RefIndex shr 2;
    for AlphaIndex := 0 to Length(CAlphaValues) - 1 do
    begin
      BlendColor32.A := AlphaIndex shl 4;
      for Index := 0 to High(Byte) do
      begin
        for MasterIndex := 0 to 7 do
        begin
          MergeColor32.A := Index;
          ExpectedColor32.ARGB := MergeRegEx_Reference(BlendColor32.ARGB, MergeColor32.ARGB, TColor32(MasterIndex shl 5));
          CombinedColor32.ARGB := MergeRegEx(BlendColor32.ARGB, MergeColor32.ARGB, TColor32(MasterIndex shl 5));

          EMMS;

          CheckColor(ExpectedColor32, CombinedColor32, FMaxDifferenceLimit,
            '(RefIndex: %d, Alpha Index: %d, MergeRegEx(FG: %.8X, BG: %.8X, Master: %d) )',
            [RefIndex, AlphaIndex, BlendColor32.ARGB, MergeColor32.ARGB, MasterIndex shl 5]);
        end;
      end;
    end;
  end;
end;

procedure TCustomTestBlendModes.TestMergeMem;
var
  MergeColor32    : TColor32Entry;
  BlendColor32    : TColor32Entry;
  CombinedColor32 : TColor32Entry;
  ExpectedColor32 : TColor32Entry;
  RefIndex, Index : Integer;
  AlphaIndex      : Integer;
const
  CAlphaValues : array [0..14] of Byte = ($00, $01, $20, $40, $41, $60, $7F, $80, $9F, $BF, $C0, $C1, $DF, $FE, $FF);
begin
  Rebind(FID_EMMS, False);
  if (not Rebind(FID_MERGEMEM)) then
  begin
    Check(True);
    Exit;
  end;

  // sample test
  for RefIndex := 0 to High(Byte) do
  begin
    BlendColor32.B := RefIndex;
    BlendColor32.G := RefIndex shr 1;
    BlendColor32.R := RefIndex shr 2;
    for AlphaIndex := 0 to Length(CAlphaValues) - 1 do
    begin
      BlendColor32.A := AlphaIndex shl 4;
      for Index := 0 to High(Byte) do
      begin
        MergeColor32.ARGB := clBlack32;
        MergeColor32.A := Index;
        ExpectedColor32 := MergeColor32;
        CombinedColor32 := MergeColor32;
        MergeMem_Reference(BlendColor32.ARGB, ExpectedColor32.ARGB);
        MergeMem(BlendColor32.ARGB, CombinedColor32.ARGB);

        EMMS;

        CheckColor(ExpectedColor32, CombinedColor32, FMaxDifferenceLimit,
          '(RefIndex: %d, Alpha Index: %d, MergeMem(FG: %.8X, BG: %.8X) )',
          [RefIndex, AlphaIndex, BlendColor32.ARGB, MergeColor32.ARGB]);
      end;
    end;
  end;
end;

procedure TCustomTestBlendModes.TestMergeMemEx;
var
  MergeColor32    : TColor32Entry;
  BlendColor32    : TColor32Entry;
  CombinedColor32 : TColor32Entry;
  ExpectedColor32 : TColor32Entry;
  RefIndex, Index : Integer;
  AlphaIndex      : Integer;
  MasterIndex     : Integer;
const
  CAlphaValues : array [0..14] of Byte = ($00, $01, $20, $40, $41, $60, $7F, $80, $9F, $BF, $C0, $C1, $DF, $FE, $FF);
begin
  Rebind(FID_EMMS, False);
  Rebind(FID_MERGEREG);
  Rebind(FID_MERGEMEM);
  if (not Rebind(FID_MERGEMEMEX)) then
  begin
    Check(True);
    Exit;
  end;

  MergeColor32.ARGB := clBlack32;
  for RefIndex := 0 to High(Byte) do
  begin
    BlendColor32.B := RefIndex;
    BlendColor32.G := RefIndex shr 1;
    BlendColor32.R := RefIndex shr 2;
    for AlphaIndex := 0 to Length(CAlphaValues) - 1 do
    begin
      BlendColor32.A := AlphaIndex shl 4;
      for Index := 0 to High(Byte) do
      begin
        for MasterIndex := 0 to 7 do
        begin
          MergeColor32.ARGB := clBlack32;
          MergeColor32.A := Index;
          ExpectedColor32 := MergeColor32;
          CombinedColor32 := MergeColor32;
          MergeMemEx_Reference(BlendColor32.ARGB, ExpectedColor32.ARGB, TColor32(MasterIndex shl 5));
          MergeMemEx(BlendColor32.ARGB, CombinedColor32.ARGB, TColor32(MasterIndex shl 5));

          EMMS;

          CheckColor(ExpectedColor32, CombinedColor32, FMaxDifferenceLimit,
            '(RefIndex: %d, Alpha Index: %d, MergeMemEx(FG: %.8X, BG: %.8X, %d) )',
            [RefIndex, AlphaIndex, BlendColor32.ARGB, MergeColor32.ARGB, MasterIndex shl 5]);
        end;
      end;
    end;
  end;
end;

procedure TCustomTestBlendModes.TestMergeLine;
var
  BlendColor32    : TColor32Entry;
  MergedColor32   : TColor32Entry;
  ExpectedColor32 : TColor32Entry;
  RefIndex, Index : Integer;
  AlphaIndex      : Integer;
const
  CAlphaValues : array [0..14] of Byte = ($00, $01, $20, $40, $41, $60, $7F, $80, $9F, $BF, $C0, $C1, $DF, $FE, $FF);
begin
  Rebind(FID_EMMS, False);
  Rebind(FID_MERGEREG);
  if (not Rebind(FID_MERGELINE)) then
  begin
    Check(True);
    Exit;
  end;

  for RefIndex := 0 to High(Byte) do
  begin
    for AlphaIndex := 0 to Length(CAlphaValues) - 1 do
    begin
      for Index := 0 to High(Byte) do
      begin
        FBackground^[Index] := clBlack32;
        TColor32Entry(FBackground^[Index]).A := CAlphaValues[AlphaIndex];
        BlendColor32.B := RefIndex;
        BlendColor32.G := RefIndex shr 1;
        BlendColor32.R := RefIndex shr 2;
        BlendColor32.A := Index;
        FForeground^[Index] := BlendColor32.ARGB;
      end;

      MergeLine(PColor32(FForeground), PColor32(FBackground), 256);

      EMMS;

      for Index := 0 to High(Byte) do
      begin
        BlendColor32.ARGB := clBlack32;
        BlendColor32.A := CAlphaValues[AlphaIndex];
        ExpectedColor32.ARGB := MergeReg_Reference(FForeground^[Index], BlendColor32.ARGB);
        MergedColor32.ARGB := FBackground^[Index];

        EMMS;

        CheckColor(ExpectedColor32, MergedColor32, FMaxDifferenceLimit);
      end;
    end;
  end;
end;

procedure TCustomTestBlendModes.TestMergeLineEx;
var
  BlendColor32    : TColor32Entry;
  MergedColor32   : TColor32Entry;
  ExpectedColor32 : TColor32Entry;
  RefIndex, Index : Integer;
  AlphaIndex      : Integer;
  MasterIndex     : Integer;
const
  CAlphaValues : array [0..14] of Byte = ($00, $01, $20, $40, $41, $60, $7F, $80, $9F, $BF, $C0, $C1, $DF, $FE, $FF);
begin
  Rebind(FID_EMMS, False);
  if (not Rebind(FID_MERGELINEEX)) then
  begin
    Check(True);
    Exit;
  end;

  // static test

  // sample test
  for MasterIndex := 0 to 7 do
  begin
    for RefIndex := 0 to High(Byte) do
    begin
      for AlphaIndex := 0 to Length(CAlphaValues) - 1 do
      begin
        for Index := 0 to High(Byte) do
        begin
          FBackground^[Index] := clBlack32;
          TColor32Entry(FBackground^[Index]).A := CAlphaValues[AlphaIndex];
          BlendColor32.B := RefIndex;
          BlendColor32.G := RefIndex shr 1;
          BlendColor32.R := RefIndex shr 2;
          BlendColor32.A := Index;
          FForeground^[Index] := BlendColor32.ARGB;
        end;

        MergeLineEx(PColor32(FForeground), PColor32(FBackground), 256, TColor32(MasterIndex shl 5));

        EMMS;

        for Index := 0 to High(Byte) do
        begin
          BlendColor32.ARGB := clBlack32;
          BlendColor32.A := CAlphaValues[AlphaIndex];
          MergedColor32.ARGB := FBackground^[Index];
          ExpectedColor32.ARGB := MergeRegEx_Reference(FForeground^[Index], BlendColor32.ARGB, TColor32(MasterIndex shl 5));

          EMMS;

          CheckColor(ExpectedColor32, MergedColor32, FMaxDifferenceLimit,
            '(Index: %d, RefIndex: %d, Alpha Index: %d, MergeRegEx(FG: %.8X, BG: %.8X, Master: %d))',
            [Index, RefIndex, AlphaIndex, FForeground^[Index], BlendColor32.ARGB, MasterIndex shl 5]);
        end;
      end;
    end;
  end;
end;


{ TTestBlendModesPas }

class function TTestBlendModesPas.PriorityProc: TFunctionPriority;
begin
  Result := PriorityProcPas;
end;

class function TTestBlendModesPas.PriorityProcPas(Info: PFunctionInfo): Integer;
begin
  if (Info^.Flags and BlendBindingFlagPascal <> 0) then
    Result := 0
  else
    Result := INVALID_PRIORITY;
end;

procedure TTestBlendModesPas.TestBlendReg;
begin
  inherited;
end;

procedure TTestBlendModesPas.TestBlendRegEx;
begin
  inherited;
end;

procedure TTestBlendModesPas.TestBlendMem;
begin
  inherited;
end;

procedure TTestBlendModesPas.TestBlendMemEx;
begin
  inherited;
end;

procedure TTestBlendModesPas.TestBlendLine;
begin
  inherited;
end;

procedure TTestBlendModesPas.TestBlendLine1;
begin
  inherited;
end;

procedure TTestBlendModesPas.TestBlendLineEx;
begin
  inherited;
end;

procedure TTestBlendModesPas.TestCombineReg;
begin
  inherited;
end;

procedure TTestBlendModesPas.TestCombineMem;
begin
  inherited;
end;

procedure TTestBlendModesPas.TestCombineLine;
begin
  inherited;
end;

procedure TTestBlendModesPas.TestMergeReg;
begin
  inherited;
end;

procedure TTestBlendModesPas.TestMergeRegEx;
begin
  inherited;
end;

procedure TTestBlendModesPas.TestMergeMem;
begin
  inherited;
end;

procedure TTestBlendModesPas.TestMergeMemEx;
begin
  inherited;
end;

procedure TTestBlendModesPas.TestMergeLine;
begin
  inherited;
end;

procedure TTestBlendModesPas.TestMergeLineEx;
begin
  inherited;
end;

{ TTestBlendModesAsm }

class function TTestBlendModesAsm.PriorityProc: TFunctionPriority;
begin
  Result := PriorityProcAsm;
end;

class function TTestBlendModesAsm.PriorityProcAsm(Info: PFunctionInfo): Integer;
begin
  if (Info^.InstructionSupport = []) and (Info^.Flags and BlendBindingFlagPascal = 0) then
    Result := 0
  else
    Result := INVALID_PRIORITY;
end;

procedure TTestBlendModesAsm.TestBlendReg;
begin
  inherited;
end;

procedure TTestBlendModesAsm.TestBlendRegEx;
begin
  inherited;
end;

procedure TTestBlendModesAsm.TestBlendMem;
begin
  inherited;
end;

procedure TTestBlendModesAsm.TestBlendMemEx;
begin
  inherited;
end;

procedure TTestBlendModesAsm.TestBlendLine;
begin
  inherited;
end;

procedure TTestBlendModesAsm.TestBlendLine1;
begin
  inherited;
end;

procedure TTestBlendModesAsm.TestBlendLineEx;
begin
  inherited;
end;

procedure TTestBlendModesAsm.TestCombineReg;
begin
  inherited;
end;

procedure TTestBlendModesAsm.TestCombineMem;
begin
  inherited;
end;

procedure TTestBlendModesAsm.TestCombineLine;
begin
  inherited;
end;

procedure TTestBlendModesAsm.TestMergeReg;
begin
  inherited;
end;

procedure TTestBlendModesAsm.TestMergeRegEx;
begin
  inherited;
end;

procedure TTestBlendModesAsm.TestMergeMem;
begin
  inherited;
end;

procedure TTestBlendModesAsm.TestMergeMemEx;
begin
  inherited;
end;

procedure TTestBlendModesAsm.TestMergeLine;
begin
  inherited;
end;

procedure TTestBlendModesAsm.TestMergeLineEx;
begin
  inherited;
end;


{ TTestBlendModesMMX }

class function TTestBlendModesMMX.PriorityProc: TFunctionPriority;
begin
  Result := PriorityProcMMX;
end;

class function TTestBlendModesMMX.PriorityProcMMX(Info: PFunctionInfo): Integer;
begin
  if (isMMX in Info^.InstructionSupport) then
    Result := 0
  else
    Result := INVALID_PRIORITY;
end;

procedure TTestBlendModesMMX.TestBlendReg;
begin
  inherited;
end;

procedure TTestBlendModesMMX.TestBlendRegEx;
begin
  inherited;
end;

procedure TTestBlendModesMMX.TestBlendMem;
begin
  inherited;
end;

procedure TTestBlendModesMMX.TestBlendMemEx;
begin
  inherited;
end;

procedure TTestBlendModesMMX.TestBlendLine;
begin
  inherited;
end;

procedure TTestBlendModesMMX.TestBlendLine1;
begin
  inherited;
end;

procedure TTestBlendModesMMX.TestBlendLineEx;
begin
  inherited;
end;

procedure TTestBlendModesMMX.TestCombineReg;
begin
  inherited;
end;

procedure TTestBlendModesMMX.TestCombineMem;
begin
  inherited;
end;

procedure TTestBlendModesMMX.TestCombineLine;
begin
  inherited;
end;

procedure TTestBlendModesMMX.TestMergeReg;
begin
  inherited;
end;

procedure TTestBlendModesMMX.TestMergeRegEx;
begin
  inherited;
end;

procedure TTestBlendModesMMX.TestMergeMem;
begin
  inherited;
end;

procedure TTestBlendModesMMX.TestMergeMemEx;
begin
  inherited;
end;

procedure TTestBlendModesMMX.TestMergeLine;
begin
  inherited;
end;

procedure TTestBlendModesMMX.TestMergeLineEx;
begin
  inherited;
end;


{ TTestBlendModesSSE2 }

class function TTestBlendModesSSE2.PriorityProc: TFunctionPriority;
begin
  Result := PriorityProcSSE2;
end;

class function TTestBlendModesSSE2.PriorityProcSSE2(Info: PFunctionInfo): Integer;
begin
  if (isSSE2 in Info^.InstructionSupport) then
    Result := 0
  else
    Result := INVALID_PRIORITY;
end;

procedure TTestBlendModesSSE2.TestBlendReg;
begin
  inherited;
end;

procedure TTestBlendModesSSE2.TestBlendRegEx;
begin
  inherited;
end;

procedure TTestBlendModesSSE2.TestBlendMem;
begin
  inherited;
end;

procedure TTestBlendModesSSE2.TestBlendMemEx;
begin
  inherited;
end;

procedure TTestBlendModesSSE2.TestBlendLine;
begin
  inherited;
end;

procedure TTestBlendModesSSE2.TestBlendLine1;
begin
  inherited;
end;

procedure TTestBlendModesSSE2.TestBlendLineEx;
begin
  inherited;
end;

procedure TTestBlendModesSSE2.TestCombineReg;
begin
  inherited;
end;

procedure TTestBlendModesSSE2.TestCombineMem;
begin
  inherited;
end;

procedure TTestBlendModesSSE2.TestCombineLine;
begin
  inherited;
end;

procedure TTestBlendModesSSE2.TestMergeReg;
begin
  inherited;
end;

procedure TTestBlendModesSSE2.TestMergeRegEx;
begin
  inherited;
end;

procedure TTestBlendModesSSE2.TestMergeMem;
begin
  inherited;
end;

procedure TTestBlendModesSSE2.TestMergeMemEx;
begin
  inherited;
end;

procedure TTestBlendModesSSE2.TestMergeLine;
begin
  inherited;
end;

procedure TTestBlendModesSSE2.TestMergeLineEx;
begin
  inherited;
end;


{ TTestBlendTables }

procedure TTestBlendTables.TestAlphaTable;
var
  a, b, c: integer;
  Expected, Actual: integer;
  Errors: integer;
const
  MaxAbsoluteError = 1;
  MaxErrors = 63232;
begin
  Errors := 0;
  for a := 0 to 255 do
    for b := 0 to 255 do
    begin
      Expected := Round(a * b / 255);

      for c := 0 to 3 do
      begin
        // Actual is an *approximation* of Round(a * b / 255) calculated as:
        //   Actual := (a * b + 128) shr 8;

        Actual := ((a * alpha_ptr[b][c].R + bias_ptr[c].R) shr 8) and $FF;

        if (Expected <> Actual) then
        begin
          Inc(Errors);

          if (Abs(Expected-Actual) > MaxAbsoluteError) then
            CheckEquals(Expected, Actual, Format('%d * %d / 255', [a, b]));
        end;
      end;
    end;

  if (Errors > MaxErrors) then
    Fail(Format('Too many errors: %d (expected max %d)', [Errors, MaxErrors]));
end;

procedure TTestBlendTables.TestAlphaTableAlignment;
begin
  Check(NativeUInt(alpha_ptr) and $F = 0);
end;

procedure TTestBlendTables.TestDivisionTable;
var
  a, b: integer;
  Expected, Actual: integer;
begin
  for a := 0 to 255 do
    for b := 0 to 255 do
    begin
      if (a <> 0) then
        Expected := Clamp(Round(b / a * 255))
      else
        Expected := 0;

      Actual := RcTable[a, b];

      if (Expected <> Actual) then
        CheckEquals(Expected, Actual, Format('%d / %d * 255', [a, b]));
    end;
end;

procedure TTestBlendTables.TestMultiplicationTable;
var
  a, b: integer;
  Expected, Actual: integer;
begin
  for a := 0 to 255 do
    for b := 0 to 255 do
    begin
      Expected := Round(a * b / 255);

      Actual := DivTable[a, b];

      if (Expected <> Actual) then
        CheckEquals(Expected, Actual, Format('%d * %d / 255', [a, b]));
    end;
end;

{ TTestBlendModesSSE41 }

class function TTestBlendModesSSE41.PriorityProc: TFunctionPriority;
begin
  Result := PriorityProcSSE41;
end;

class function TTestBlendModesSSE41.PriorityProcSSE41(Info: PFunctionInfo): Integer;
begin
  if (isSSE41 in Info^.InstructionSupport) then
    Result := 0
  else
    Result := INVALID_PRIORITY;
end;

procedure TTestBlendModesSSE41.TestBlendLine;
begin
  inherited;

end;

procedure TTestBlendModesSSE41.TestBlendLine1;
begin
  inherited;

end;

procedure TTestBlendModesSSE41.TestBlendLineEx;
begin
  inherited;

end;

procedure TTestBlendModesSSE41.TestBlendMem;
begin
  inherited;

end;

procedure TTestBlendModesSSE41.TestBlendMemEx;
begin
  inherited;

end;

procedure TTestBlendModesSSE41.TestBlendReg;
begin
  inherited;

end;

procedure TTestBlendModesSSE41.TestBlendRegEx;
begin
  inherited;

end;

procedure TTestBlendModesSSE41.TestCombineLine;
begin
  inherited;

end;

procedure TTestBlendModesSSE41.TestCombineMem;
begin
  inherited;

end;

procedure TTestBlendModesSSE41.TestCombineReg;
begin
  inherited;

end;

procedure TTestBlendModesSSE41.TestMergeLine;
begin
  inherited;

end;

procedure TTestBlendModesSSE41.TestMergeLineEx;
begin
  inherited;

end;

procedure TTestBlendModesSSE41.TestMergeMem;
begin
  inherited;

end;

procedure TTestBlendModesSSE41.TestMergeMemEx;
begin
  inherited;

end;

procedure TTestBlendModesSSE41.TestMergeReg;
begin
  inherited;

end;

procedure TTestBlendModesSSE41.TestMergeRegEx;
begin
  inherited;

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
  RegisterTest(TTestBlendTables.Suite);

  RegisterTest(TTestBlendModesPas.Suite);
  RegisterTest(TTestBlendModesAsm.Suite);
  if isMMX in GR32_System.CPU.InstructionSupport then
    RegisterTest(TTestBlendModesMMX.Suite);
  if isSSE2 in GR32_System.CPU.InstructionSupport then
    RegisterTest(TTestBlendModesSSE2.Suite);
  if isSSE41 in GR32_System.CPU.InstructionSupport then
    RegisterTest(TTestBlendModesSSE41.Suite);

end.
