unit TestGR32Blend;

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
 * The Original Code is Blend Unit Test for Graphics32
 *
 * The Initial Developer of the Original Code is Anders Melander
 *
 * Portions created by the Initial Developer are Copyright (C) 2026
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I ..\..\Source\GR32.inc}

uses
  DUnitX.TestFramework,
{$ifdef MSWINDOWS}
  Windows,
{$endif}
  SysUtils,
{$IFDEF FPC}
  Rtti,
  TypInfo,
  Generics.Collections,
{$ELSE}
  System.Rtti,
  System.TypInfo,
  System.Generics.Collections,
{$ENDIF}
  GR32,
  GR32_Blend,
  GR32_Bindings,

  GR32.DUnitx;

type
  TestcaseThresholdAttribute = class abstract(TCustomAttribute)
  private
    FValue: integer;
    FInstructionSupport: TInstructionSupport;
  public
    constructor Create(AValue: integer); overload;
    constructor Create(AInstructionSupport: TInstructionSupport; AValue: integer); overload;
  public
    property Value: integer read FValue;
    property InstructionSupport: TInstructionSupport read FInstructionSupport;
  end;

  // Max absolute value difference allowed for a test case
  // Default is 0 meaning no difference allowed.
  //
  // - Apply limit to a test case:
  //   [MaxError(Value: integer)]
  //
  // - Apply limit to a particular test case variant:
  //   [MaxError(InstructionSupport: TInstructionSupport; Value: integer)]
  //
  MaxErrorAttribute = class(TestcaseThresholdAttribute);

  // The number of times a test case is allowed to exceed MaxError.
  // Default is -1 meaning no limit.
  //
  // - Apply limit to a test case
  //   [MaxErrorCount(Value: integer)]
  //
  // - Apply limit to a particular test case variant
  //   [MaxErrorCount(InstructionSupport: TInstructionSupport; Value: integer)]
  //
  MaxErrorCountAttribute = class(TestcaseThresholdAttribute);

type
  [TestFixture]
  TTestBlendTables = class
  public
    [Test]
    procedure TestAlphaTableAlignment;
    [Test]
    procedure TestAlphaTable;
    [Test]
    procedure TestDivisionTable;
    [Test]
    procedure TestMultiplicationTable;
  end;

type
  TCheckCombine = reference to procedure (ForeGround, Background: TColor32; Weight: Cardinal);

  [TestFixture]
  TTestBlendModes = class
  strict private
    FForeground: PColor32Array;
    FBackground: PColor32Array;
    FForegroundPreguard: PColor32;
    FBackgroundPreguard: PColor32;
    FForegroundPostguard: PColor32;
    FBackgroundPostguard: PColor32;
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

    function GetThreshold<T: TestcaseThresholdAttribute>(AMethod: TRttiMethod; const AFunctionInfo: IFunctionInfo; ADefault: integer): integer;
    function BindImplementation: boolean;

    procedure DoCheckCombine(CheckCombineProc: TCheckCombine);
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

    [Test]
    [TestCaseSource('GR32_Blend', 'BlendReg')]
    [MaxError(1)]
    procedure TestBlendReg(const ImplName: string);

    [Test]
    [TestCaseSource('GR32_Blend', 'BlendRegEx')]
    [MaxError(2)]
    procedure TestBlendRegEx(const ImplName: string);

    [Test]
    [TestCaseSource('GR32_Blend', 'BlendMem')]
    [MaxError(1)]
    procedure TestBlendMem(const ImplName: string);

    [Test]
    [TestCaseSource('GR32_Blend', 'BlendMems')]
    [MaxError(1)]
    procedure TestBlendMems(const ImplName: string);

    [Test]
    [TestCaseSource('GR32_Blend', 'BlendMemEx')]
    [MaxError(2)]
    procedure TestBlendMemEx(const ImplName: string);

    [Test]
    [TestCaseSource('GR32_Blend', 'BlendLine')]
    [MaxError(1)]
    procedure TestBlendLine(const ImplName: string);

    [Test]
    [TestCaseSource('GR32_Blend', 'BlendLineEx')]
    [MaxError(2)]
    procedure TestBlendLineEx(const ImplName: string);

    [Test]
    [TestCaseSource('GR32_Blend', 'CombineReg')]
    [MaxError(1)]
    [MaxError([isAssembler], 2)]
    procedure TestCombineReg(const ImplName: string);

    [Test]
    [TestCaseSource('GR32_Blend', 'CombineMem')]
    [MaxError(1), MaxErrorCount(-1)]
    [MaxError([isAssembler], 2)]
    procedure TestCombineMem(const ImplName: string);

    [Test]
    [TestCaseSource('GR32_Blend', 'CombineLine')]
    [MaxError(1)]
    procedure TestCombineLine(const ImplName: string);

    [Test]
    [TestCaseSource('GR32_Blend', 'MergeReg')]
    [MaxError(4)]
    [MaxError([isSSE41], 5)]
    procedure TestMergeReg(const ImplName: string);

    [Test]
    [TestCaseSource('GR32_Blend', 'MergeRegEx')]
    [MaxError(4)]
    [MaxError([isPascal], 5)]
    procedure TestMergeRegEx(const ImplName: string);

    [Test]
    [TestCaseSource('GR32_Blend', 'MergeMem')]
    [MaxError(4)]
    [MaxError([isPascal], 5)]
    [MaxError([isSSE41], 5)]
    procedure TestMergeMem(const ImplName: string);

    [Test]
    [TestCaseSource('GR32_Blend', 'MergeMemEx')]
    [MaxError(4)]
    [MaxError([isPascal], 5)]
    procedure TestMergeMemEx(const ImplName: string);

    [Test]
    [TestCaseSource('GR32_Blend', 'MergeLine')]
    [MaxError(4)]
    procedure TestMergeLine(const ImplName: string);

    [Test]
    [TestCaseSource('GR32_Blend', 'MergeMems')]
    [MaxError(4)]
    procedure TestMergeMems(const ImplName: string);

    [Test]
    [TestCaseSource('GR32_Blend', 'MergeLineEx')]
    [MaxError(4)]
    procedure TestMergeLineEx(const ImplName: string);
  end;

implementation

uses
  Math,
  GR32_System,
  GR32_LowLevel,
  GR32_BlendReference;

constructor TestcaseThresholdAttribute.Create(AValue: integer);
begin
  inherited Create;
  FValue := AValue;
end;

constructor TestcaseThresholdAttribute.Create(AInstructionSupport: TInstructionSupport; AValue: integer);
begin
  inherited Create;
  FValue := AValue;
  FInstructionSupport := AInstructionSupport;
end;

{ TTestBlendModes }

function TTestBlendModes.GetThreshold<T>(AMethod: TRttiMethod; const AFunctionInfo: IFunctionInfo; ADefault: integer): integer;
var
  Attribute: TCustomAttribute;
begin
  Result := ADefault;
  for Attribute in AMethod.GetAttributes do
    if Attribute is T then
    begin
      if (T(Attribute).InstructionSupport <> []) then
      begin
        if (T(Attribute).InstructionSupport * AFunctionInfo.InstructionSupport <> []) then
        begin
          Result := T(Attribute).Value;
          break;
        end;
      end else
        Result := T(Attribute).Value;
    end;
end;

function TTestBlendModes.BindImplementation: boolean;
begin
  var RttiContext := TRttiContext.Create;
  try
    var RttiType := RttiContext.GetType(ClassType);
    Assert.IsNotNull(RttiType, Format('Failed to acquire RTTI for %s', [ClassType.ClassName]));

    var ParentMethodName := TDUnitX.CurrentRunner.CurrentTestName;
    var ChildMethodName: string;
    var n := Pos('.', ParentMethodName);
    if n = 0 then
      Assert.FailFmt('Failed to parse test name as Binding.Implementation: %s', [ParentMethodName]);
    ChildMethodName := Copy(ParentMethodName, n+1, MaxInt);
    SetLength(ParentMethodName, n-1);

    var RttiMethod := RttiType.GetMethod(ParentMethodName);
    Assert.IsNotNull(RttiMethod, Format('Failed to acquire RTTI for method %s.%s', [ClassType.ClassName, ParentMethodName]));

    var TestCaseSource := RttiMethod.GetAttribute<TestCaseSourceAttribute>;
    Assert.IsNotNull(TestCaseSource, Format('Failed to acquire TestCaseSourceAttribute attribute for %s.%s', [ClassType.ClassName, ParentMethodName]));

    var BindRegistry := TFunctionRegistry.FindRegistry(TestCaseSource.Registry.Name);
    Assert.IsNotNull(BindRegistry, Format('Binding restry not found: %s', [TestCaseSource.Registry.Name]));

    // Clear fallback bindings.
    // A fallback is a function that is used by a blend function when an actual
    // implementation isn't available.
    //
    // For example, MergeMems_Pas and MergeLine_Pas are implemented as loops that
    // call MergeReg.
    // This means that if, for example, MergeReg_SSE2 is implemented but
    // MergeLine_SSE2 isn't implemented, then calling MergeLine will in effect
    // call MergeLine_Pas which will then call MergeReg_SSE2.
    BindRegistry.RebindAll(nil, True);

    var Binding := BindRegistry.FindBinding(TestCaseSource.BindingName);
    Assert.IsNotNull(Binding, Format('Binding not found in %s registry: %s', [TestCaseSource.Registry.Name, TestCaseSource.BindingName]));

    var Impl := Binding.FindImplementation(ChildMethodName);
    Assert.IsNotNull(Binding, Format('Implementation not found in %s.%s binding: %s', [TestCaseSource.Registry.Name, TestCaseSource.BindingName, ChildMethodName]));

    if not (Impl.InstructionSupport <= CPU.InstructionSupport) then
      Assert.Pass(Format('Unsupported CPU instruction set for %s.%s.%s', [TestCaseSource.Registry.Name, TestCaseSource.BindingName, ChildMethodName]));

    // Apply binding
    Binding.BindVariable^ := Impl.Proc;

    // Apply custom error tolerances
    FMaxDifferenceLimit := GetThreshold<MaxErrorAttribute>(RttiMethod, Impl, FMaxDifferenceLimit);
    FErrorCountLimit := GetThreshold<MaxErrorCountAttribute>(RttiMethod, Impl, FErrorCountLimit);
  finally
    RttiContext.Free
  end;

  Result := True;
end;

procedure TTestBlendModes.SetUp;
begin
  BlendRegistry.RebindAll(nil, True);

  FErrorCountLimit := -1;
  FMaxDifferenceLimit := 0;

  FTestCount := 0;
  FErrorCount := 0;
  FMaxAbsoluteDifference := 0;
  FDifferenceCount := 0;
  FDifferenceSum := 0;

  GetMem(FForegroundPreguard, (256 + 2) * SizeOf(TColor32));
  GetMem(FBackgroundPreguard, (256 + 2) * SizeOf(TColor32));
  GetMem(FReference, 256 * SizeOf(TColor32));

  FForeground := pointer(FForegroundPreguard);
  Inc(PColor32(FForeground));
  FBackground := pointer(FBackgroundPreguard);
  Inc(PColor32(FBackground));

  FForegroundPostguard := PColor32(FForeground);
  Inc(FForegroundPostguard, 256);
  FBackgroundPostguard := PColor32(FBackground);
  Inc(FBackgroundPostguard, 256);

  FForegroundPreguard^ := $315191e1;
  FBackgroundPreguard^ := $3456789a;
  FForegroundPostguard^ := $3456789a;
  FBackgroundPostguard^ := $315191e1;
end;

procedure TTestBlendModes.TearDown;
begin
  Assert.AreEqual(Cardinal($315191e1), FForegroundPreguard^, 'Memory underrun in foreground bytes');
  Assert.AreEqual(Cardinal($3456789a), FBackgroundPreguard^, 'Memory underrun in background bytes');
  Assert.AreEqual(Cardinal($3456789a), FForegroundPostguard^, 'Memory overrun in foreground bytes');
  Assert.AreEqual(Cardinal($315191e1), FBackgroundPostguard^, 'Memory overrun in background bytes');

  if (FDifferenceCount > 0) then
    Status(Format('***'#13+
      'Errors: %.0n = %.1n %% (Limit: %d)'#13+
      'Differences: %.0n'#13+
      'Average difference: %.2n'#13+
      'Max difference: %d (Limit: %d)',
      [FErrorCount*1.0, FErrorCount/FTestCount*100, FErrorCountLimit, FDifferenceCount*1.0, FDifferenceSum/FDifferenceCount, FMaxAbsoluteDifference, FMaxDifferenceLimit]));

  // This is just for verification that we're testing against the strictest possible criteria
  if (FMaxAbsoluteDifference < FMaxDifferenceLimit) then
    Status(Format('***'#13+'Expected max difference: %d, Actual: %d', [FMaxDifferenceLimit, FMaxAbsoluteDifference]));

  if (FErrorCountLimit > 0) and (FErrorCount < FErrorCountLimit) then
    Status(Format('***'#13+'Expected errors: %d, Actual: %d', [FErrorCountLimit, FErrorCount]));

  Dispose(FForegroundPreguard);
  Dispose(FBackgroundPreguard);
  Dispose(FReference);

  // Clean up so that we leave the bindings in an usable state for other unit tests
  BlendRegistry.RebindAll(nil, True);
end;

procedure TTestBlendModes.DoCheckColor(ExpectedColor32, ActualColor32: TColor32Entry; MaxDifferenceLimit: Byte; const AExtra: string; const AExtraParams: array of const);
var
  Msg, MsgExtra: string;
  DifferenceA: integer;
  DifferenceR: integer;
  DifferenceG: integer;
  DifferenceB: integer;
  MaxAbsoluteDifference: integer;
begin
  Inc(FTestCount);

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
      if (FErrorCountLimit <> -1) and (FErrorCount > FErrorCountLimit) then
        Status(Format('Error limit exceeded: %d', [FErrorCount]));

      if (MaxAbsoluteDifference > MaxDifferenceLimit) then
        Status(Format('Difference threshold exceeded: %d (max allowed: %d)', [MaxAbsoluteDifference, MaxDifferenceLimit]));

      if (AExtra <> '') then
        MsgExtra := Format(AExtra, AExtraParams)
      else
        MsgExtra := '';

      Msg := Format('Expected:%.8X, Actual:%.8X, Dif:%.2X%.2X%.2X%.2X %s',
        [ExpectedColor32.ARGB, ActualColor32.ARGB, Abs(DifferenceA), Abs(DifferenceR), Abs(DifferenceG), Abs(DifferenceB), MsgExtra]);
      Assert.Fail(Msg);
    end else
      Assert.AreEqual(0, 0);
  end else
    Assert.AreEqual(0, 0);
end;

procedure TTestBlendModes.CheckColor(ExpectedColor32, ActualColor32: TColor32Entry; MaxDifferenceLimit: Byte);
begin
  DoCheckColor(ExpectedColor32, ActualColor32, MaxDifferenceLimit, '', []);
end;

procedure TTestBlendModes.CheckColor(ExpectedColor32, ActualColor32: TColor32Entry; MaxDifferenceLimit: Byte; const AExtra: string; const AExtraParams: array of const);
begin
  DoCheckColor(ExpectedColor32, ActualColor32, MaxDifferenceLimit, AExtra, AExtraParams);
end;

procedure TTestBlendModes.TestBlendReg(const ImplName: string);
var
  BlendColor32    : TColor32Entry;
  CombinedColor32 : TColor32Entry;
  ExpectedColor32 : TColor32Entry;
  RefIndex, Index : Integer;
begin
  if not BindImplementation then
    Exit;

  // static test
  BlendColor32.A := $1A;
  BlendColor32.B := $2B;
  BlendColor32.G := $3C;
  BlendColor32.R := $4D;

  ExpectedColor32.ARGB := BlendReg_Reference(BlendColor32.ARGB, BlendColor32.ARGB);
  ExpectedColor32.A := $FF;
  CombinedColor32.ARGB := BlendReg(BlendColor32.ARGB, BlendColor32.ARGB);
  CombinedColor32.A := $FF;
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
      ExpectedColor32.A := $FF;
      CombinedColor32.ARGB := BlendReg(BlendColor32.ARGB, clBlack32);
      CombinedColor32.A := $FF;

      CheckColor(ExpectedColor32, CombinedColor32, FMaxDifferenceLimit);
    end;
  end;
end;

procedure TTestBlendModes.TestBlendRegEx(const ImplName: string);
var
  BlendColor32    : TColor32Entry;
  CombinedColor32 : TColor32Entry;
  ExpectedColor32 : TColor32Entry;
  RefIndex, Index : Integer;
  MasterIndex     : Integer;
begin
  if not BindImplementation then
    Exit;

  // static test
  BlendColor32.A := $F8;
  BlendColor32.B := $A1;
  BlendColor32.G := $50;
  BlendColor32.R := $28;

  ExpectedColor32.ARGB := BlendRegEx_Reference(BlendColor32.ARGB, clBlack32, TColor32(7 shl 5));
  ExpectedColor32.A := $FF;
  CombinedColor32.ARGB := BlendRegEx(BlendColor32.ARGB, clBlack32, TColor32(7 shl 5));
  CombinedColor32.A := $FF;

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
        ExpectedColor32.A := $FF;
        CombinedColor32.ARGB := BlendRegEx(BlendColor32.ARGB, clBlack32, TColor32(MasterIndex shl 5));
        CombinedColor32.A := $FF;

        CheckColor(ExpectedColor32, CombinedColor32, FMaxDifferenceLimit);
      end;
    end;
  end;
end;

procedure TTestBlendModes.TestBlendMem(const ImplName: string);
var
  BlendColor32    : TColor32Entry;
  CombinedColor32 : TColor32Entry;
  ExpectedColor32 : TColor32Entry;
  RefIndex, Index : Integer;
begin
  if not BindImplementation then
    Exit;

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

      // Documentation states that background is assumed to be opaque but then continues
      // to explain what happens when it's not. This is probably a doc bug.
      // BlendMem_Pas/Asm forces the Alpha to 255 but BlendMem_Reference doesn't.
      ExpectedColor32.A := $FF;
      CombinedColor32.A := $FF;

      CheckColor(ExpectedColor32, CombinedColor32, FMaxDifferenceLimit);
    end;
  end;
end;

procedure TTestBlendModes.TestBlendMemEx(const ImplName: string);
var
  BlendColor32    : TColor32Entry;
  CombinedColor32 : TColor32Entry;
  ExpectedColor32 : TColor32Entry;
  RefIndex, Index : Integer;
  MasterIndex     : Integer;
begin
  if not BindImplementation then
    Exit;

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

procedure TTestBlendModes.TestBlendMems(const ImplName: string);

  procedure DoTest(Color: TColor32; Count: integer);
  var
    CombinedColor32 : TColor32Entry;
    ExpectedColor32 : TColor32Entry;
    Index           : Integer;
  begin
    for Index := 0 to Count-1 do
    begin
      FBackground^[Index] := clBlack32;
      TColor32Entry(FBackground^[Index]).R := Index;
      TColor32Entry(FBackground^[Index]).G := High(Byte) - Index;
    end;

    BlendMems(Color, PColor32(FBackground), Count);

    for Index := 0 to Count-1 do
    begin
      ExpectedColor32.ARGB := clBlack32;
      TColor32Entry(ExpectedColor32).R := Index;
      TColor32Entry(ExpectedColor32).G := High(Byte) - Index;

      BlendMem_Reference(Color, ExpectedColor32.ARGB);

      CombinedColor32.ARGB := FBackground^[Index];
      CombinedColor32.A := $FF;
      ExpectedColor32.A := $FF;

      CheckColor(ExpectedColor32, CombinedColor32, FMaxDifferenceLimit);
    end;
  end;

  procedure DoTestColor(Color: TColor32);
  begin
    // Negative
    DoTest(Color, -1);

    // Zero
    DoTest(Color, 0);
    DoTest(Color, 1);

    // Odd count
    DoTest(Color, 3);
    DoTest(Color, 255);
    // Even count
    DoTest(Color, 2);
    DoTest(Color, 256);
  end;

begin
  if not BindImplementation then
    Exit;

  DoTestColor($00FF7F00);
  DoTestColor($80FF7F00);
  DoTestColor($FFFF7F00);
end;

procedure TTestBlendModes.TestBlendLine(const ImplName: string);

  procedure DoTest(Count: integer);
  var
    CombinedColor32 : TColor32Entry;
    ExpectedColor32 : TColor32Entry;
    Index           : Integer;
  begin
    for Index := 0 to Count-1 do
    begin
      FBackground^[Index] := clBlack32;
      FForeground^[Index] := clWhite32;
      TColor32Entry(FForeground^[Index]).A := Index;
    end;

    BlendLine(PColor32(FForeground), PColor32(FBackground), Count);

    for Index := 0 to Count-1 do
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

begin
  if not BindImplementation then
    Exit;

  // Negative
  DoTest(-1);

  // Zero
  DoTest(0);
  // One
  DoTest(1);

  // Odd count
  DoTest(255);
  // Even count
  DoTest(256);
end;

procedure TTestBlendModes.TestBlendLineEx(const ImplName: string);

  procedure DoTest(Count: integer);
  var
    ActualColor32      : TColor32Entry;
    ExpectedColor32    : TColor32Entry;
    Index, MasterIndex : Integer;
  begin
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

begin
  if not BindImplementation then
    Exit;

//  FMaxDifferenceLimit := 2;

  // Negative
  DoTest(-1);

  // Zero
  DoTest(0);
  // One
  DoTest(1);

  // Odd count
  DoTest(255);
  // Even count
  DoTest(256);
end;

procedure TTestBlendModes.DoCheckCombine(CheckCombineProc: TCheckCombine);
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

procedure TTestBlendModes.TestCombineReg(const ImplName: string);
begin
  if not BindImplementation then
    Exit;

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

      CheckColor(ExpectedColor32, ActualColor32, FMaxDifferenceLimit, 'CombineReg(FG:%.8X, BG:%.8X, Weight:%d)', [ForeGround, Background, Weight]);
    end
  );
end;

procedure TTestBlendModes.TestCombineMem(const ImplName: string);
begin
  if not BindImplementation then
    Exit;

//  FMaxDifferenceLimit := 2;

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

      CheckColor(ExpectedColor32, ActualColor32, FMaxDifferenceLimit, 'Combinemem(FG:%.8X, BG:%.8X, Weight:%d)', [ForeGround, Background, Weight]);
    end
  );
end;

procedure TTestBlendModes.TestCombineLine(const ImplName: string);
var
  ExpectedColor32 : TColor32Entry;
  Index           : Integer;
begin
  if not BindImplementation then
    Exit;

  for Index := 0 to High(Byte) do
  begin
    FBackground^[Index] := clBlack32;
    FForeground^[Index] := clWhite32;
    TColor32Entry(FForeground^[Index]).A := Index;
  end;

  CombineLine(PColor32(FForeground), PColor32(FBackground), 256, $FF);

  for Index := 0 to High(Byte) do
  begin
    ExpectedColor32.ARGB := (Index shl 24) or $FFFFFF;

    CheckColor(ExpectedColor32, TColor32Entry(FBackground^[Index]), FMaxDifferenceLimit, 'CombineLine(FG:%.8X, BG:%.8X, Weight:%d)', [FForeground^[Index], FBackground^[Index], 255]);
  end;
end;

procedure TTestBlendModes.TestMergeReg(const ImplName: string);
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
  if not BindImplementation then
    Exit;

//  FMaxDifferenceLimit := 5;

  // static test
  MergeColor32.ARGB := clBlack32;
  MergeColor32.A := 3;
  BlendColor32.B := $D7;
  BlendColor32.G := $6B;
  BlendColor32.R := $35;
  BlendColor32.A := $10;
  ExpectedColor32.ARGB := MergeReg_Reference(BlendColor32.ARGB, MergeColor32.ARGB);
  CombinedColor32.ARGB := MergeReg(BlendColor32.ARGB, MergeColor32.ARGB);

  CheckColor(ExpectedColor32, CombinedColor32, FMaxDifferenceLimit, 'MergeReg(FG: %.8X, BG: %.8X)', [BlendColor32.ARGB, MergeColor32.ARGB]);

  // Test for alpha-premultiplication; Color with Alpha=0 should not contribute to result
  ExpectedColor32.ARGB := MergeReg_Reference($FF00FFFF, $00FF0000);
  CombinedColor32.ARGB := MergeReg($FF00FFFF, $00FF0000);
  Assert.AreEqual(255, Cardinal(CombinedColor32.A));
  Assert.AreEqual(0, Cardinal(CombinedColor32.R));
  CheckColor(ExpectedColor32, CombinedColor32, FMaxDifferenceLimit);

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

        CheckColor(ExpectedColor32, CombinedColor32, FMaxDifferenceLimit,
          '(RefIndex: %d, Alpha Index: %d, MergeReg(FG: %.8X, BG: %.8X) )', [RefIndex, AlphaIndex, BlendColor32.ARGB, MergeColor32.ARGB]);
      end;
    end;
  end;
end;

procedure TTestBlendModes.TestMergeRegEx(const ImplName: string);
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
  if not BindImplementation then
    Exit;

//  FMaxDifferenceLimit := 5;

  BlendColor32.ARGB := TColor32($1002050B);
  MergeColor32.ARGB := TColor32($01000000);
  ExpectedColor32.ARGB := MergeRegEx_Reference(BlendColor32.ARGB, MergeColor32.ARGB, 128);
  CombinedColor32.ARGB := MergeRegEx(BlendColor32.ARGB, MergeColor32.ARGB, 128);

  CheckColor(ExpectedColor32, CombinedColor32, FMaxDifferenceLimit, 'MergeRegEx(FG: %.8X, BG: %.8X, Master: %d)', [BlendColor32.ARGB, MergeColor32.ARGB, 128]);

  // Test for alpha-premultiplication; Color with Alpha=0 should not contribute to result
  ExpectedColor32.ARGB := MergeRegEx_Reference($FF00FFFF, $00FF0000, 127);
  CombinedColor32.ARGB := MergeRegEx($FF00FFFF, $00FF0000, 127);
  Assert.AreEqual(127, Cardinal(CombinedColor32.A));
  Assert.AreEqual(0, Cardinal(CombinedColor32.R));
  CheckColor(ExpectedColor32, CombinedColor32, FMaxDifferenceLimit);

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

          CheckColor(ExpectedColor32, CombinedColor32, FMaxDifferenceLimit,
            '(RefIndex: %d, Alpha Index: %d, MergeRegEx(FG: %.8X, BG: %.8X, Master: %d) )',
            [RefIndex, AlphaIndex, BlendColor32.ARGB, MergeColor32.ARGB, MasterIndex shl 5]);
        end;
      end;
    end;
  end;
end;

procedure TTestBlendModes.TestMergeMem(const ImplName: string);
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
  if not BindImplementation then
    Exit;

//  FMaxDifferenceLimit := 5;

  // Test for alpha-premultiplication; Color with Alpha=0 should not contribute to result
  ExpectedColor32.ARGB := $00FF0000;
  CombinedColor32.ARGB := $00FF0000;
  MergeMem_Reference($FF00FFFF, ExpectedColor32.ARGB);
  MergeMem($FF00FFFF, CombinedColor32.ARGB);
  Assert.AreEqual(255, Cardinal(CombinedColor32.A));
  Assert.AreEqual(0, Cardinal(CombinedColor32.R));
  CheckColor(ExpectedColor32, CombinedColor32, FMaxDifferenceLimit);

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

        CheckColor(ExpectedColor32, CombinedColor32, FMaxDifferenceLimit,
          '(RefIndex: %d, Alpha Index: %d, MergeMem(FG: %.8X, BG: %.8X) )',
          [RefIndex, AlphaIndex, BlendColor32.ARGB, MergeColor32.ARGB]);
      end;
    end;
  end;
end;

procedure TTestBlendModes.TestMergeMemEx(const ImplName: string);
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
  if not BindImplementation then
    Exit;

//  FMaxDifferenceLimit := 5;

  // Test for alpha-premultiplication; Color with Alpha=0 should not contribute to result
  ExpectedColor32.ARGB := $00FF0000;
  CombinedColor32.ARGB := $00FF0000;
  MergeMemEx_Reference($FF00FFFF, ExpectedColor32.ARGB, 127);
  MergeMemEx($FF00FFFF, CombinedColor32.ARGB, 127);
  Assert.AreEqual(127, Cardinal(CombinedColor32.A));
  Assert.AreEqual(0, Cardinal(CombinedColor32.R));
  CheckColor(ExpectedColor32, CombinedColor32, FMaxDifferenceLimit);

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

          CheckColor(ExpectedColor32, CombinedColor32, FMaxDifferenceLimit,
            '(RefIndex: %d, Alpha Index: %d, MergeMemEx(FG: %.8X, BG: %.8X, %d) )',
            [RefIndex, AlphaIndex, BlendColor32.ARGB, MergeColor32.ARGB, MasterIndex shl 5]);
        end;
      end;
    end;
  end;
end;

procedure TTestBlendModes.TestMergeLine(const ImplName: string);
var
  BlendColor32    : TColor32Entry;
  MergedColor32   : TColor32Entry;
  ExpectedColor32 : TColor32Entry;
  RefIndex, Index : Integer;
  AlphaIndex      : Integer;
const
  CAlphaValues : array [0..14] of Byte = ($00, $01, $20, $40, $41, $60, $7F, $80, $9F, $BF, $C0, $C1, $DF, $FE, $FF);
begin
  if not BindImplementation then
    Exit;

//  FMaxDifferenceLimit := 5;

  // Test for alpha-premultiplication; Color with Alpha=0 should not contribute to result
  for Index := 0 to High(Byte) do
  begin
    FBackground^[Index] := $00FF0000;
    BlendColor32.B := Index;
    BlendColor32.G := Index shr 1;
    BlendColor32.R := 0;
    BlendColor32.A := Index;
    FForeground^[Index] := BlendColor32.ARGB;
  end;
  MergeLine(PColor32(FForeground), PColor32(FBackground), 256);
  for Index := 0 to High(Byte) do
  begin
    ExpectedColor32.ARGB := MergeReg_Reference(FForeground^[Index], $00FF0000);
    MergedColor32.ARGB := FBackground^[Index];

    if (Index <> MergedColor32.A) then
      Assert.AreEqual(Index, Integer(MergedColor32.A), Format('Incorrect alpha (Index: %d, Merge(FG: %.8X, BG: %.8X) -> %.8X)', [Index, FForeground^[Index], $00FF0000, MergedColor32.ARGB]));

    (* This criteria is too strict
    if (MergedColor32.A <> 0) and (MergedColor32.R <> 0) then
      Assert.AreEqual(0, Integer(MergedColor32.R), Format('Incorrect color (Index: %d, Merge(FG: %.8X, BG: %.8X) -> %.8X, Reference: %.8X)', [Index, FForeground^[Index], $00FF0000, MergedColor32.ARGB, ExpectedColor32.ARGB]));
    *)

    CheckColor(ExpectedColor32, MergedColor32, FMaxDifferenceLimit, 'Incorrect result (Index: %d, Merge(FG: %.8X, BG: %.8X) -> %.8X)', [Index, FForeground^[Index], $00FF0000, MergedColor32.ARGB]);
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

      for Index := 0 to High(Byte) do
      begin
        BlendColor32.ARGB := clBlack32;
        BlendColor32.A := CAlphaValues[AlphaIndex];
        ExpectedColor32.ARGB := MergeReg_Reference(FForeground^[Index], BlendColor32.ARGB);
        MergedColor32.ARGB := FBackground^[Index];

        CheckColor(ExpectedColor32, MergedColor32, FMaxDifferenceLimit, 'Incorrect result (Index: %d, Merge(FG: %.8X, BG: %.8X) -> %.8X)', [Index, FForeground^[Index], BlendColor32.ARGB, MergedColor32.ARGB]);
      end;
    end;
  end;
end;

procedure TTestBlendModes.TestMergeMems(const ImplName: string);

  procedure DoTest(Color: TColor32; Count: integer);
  var
    CombinedColor32 : TColor32Entry;
    ExpectedColor32 : TColor32Entry;
    Index           : Integer;
  begin
    for Index := 0 to Count-1 do
    begin
      FBackground^[Index] := clBlack32;
      TColor32Entry(FBackground^[Index]).R := Index;
      TColor32Entry(FBackground^[Index]).G := High(Byte) - Index;
    end;

    MergeMems(Color, PColor32(FBackground), Count);

    for Index := 0 to Count-1 do
    begin
      ExpectedColor32.ARGB := clBlack32;
      TColor32Entry(ExpectedColor32).R := Index;
      TColor32Entry(ExpectedColor32).G := High(Byte) - Index;

      MergeMem_Reference(Color, ExpectedColor32.ARGB);

      CombinedColor32.ARGB := FBackground^[Index];

      CheckColor(ExpectedColor32, CombinedColor32, FMaxDifferenceLimit);
    end;
  end;

  procedure DoTestColor(Color: TColor32);
  begin
    // Negative
    DoTest(Color, -1);

    // Zero
    DoTest(Color, 0);
    // One
    DoTest(Color, 1);

    // Odd count
    DoTest(Color, 3);
    DoTest(Color, 255);
    // Even count
    DoTest(Color, 2);
    DoTest(Color, 256);
  end;

begin
  if not BindImplementation then
    Exit;

//  FMaxDifferenceLimit := 5;

  DoTestColor($00FF7F00);
  DoTestColor($80FF7F00);
  DoTestColor($FFFF7F00);
end;

procedure TTestBlendModes.TestMergeLineEx(const ImplName: string);
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
  if not BindImplementation then
    Exit;

//  FMaxDifferenceLimit := 5;

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

        for Index := 0 to High(Byte) do
        begin
          BlendColor32.ARGB := clBlack32;
          BlendColor32.A := CAlphaValues[AlphaIndex];
          MergedColor32.ARGB := FBackground^[Index];
          ExpectedColor32.ARGB := MergeRegEx_Reference(FForeground^[Index], BlendColor32.ARGB, TColor32(MasterIndex shl 5));

          CheckColor(ExpectedColor32, MergedColor32, FMaxDifferenceLimit,
            '(Index: %d, RefIndex: %d, Alpha Index: %d, MergeRegEx(FG: %.8X, BG: %.8X, Master: %d))',
            [Index, RefIndex, AlphaIndex, FForeground^[Index], BlendColor32.ARGB, MasterIndex shl 5]);
        end;
      end;
    end;
  end;
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
            Assert.AreEqual(Expected, Actual, Format('%d * %d / 255', [a, b]));
        end;
      end;
    end;

  Assert.IsFalse(Errors > MaxErrors, Format('Too many errors: %d (expected max %d)', [Errors, MaxErrors]));
end;

procedure TTestBlendTables.TestAlphaTableAlignment;
begin
  Assert.IsTrue(NativeUInt(alpha_ptr) and $F = 0);
end;

procedure TTestBlendTables.TestDivisionTable;
var
  a, b: integer;
  Expected, Actual: integer;
begin
  Assert.AreEqual(0, 0);
  for a := 0 to 255 do
    for b := 0 to 255 do
    begin
      if (a <> 0) then
        Expected := Clamp(Round(b / a * 255))
      else
        Expected := 0;

      Actual := DivMul255Table[a, b];

      if (Expected <> Actual) then
        Assert.AreEqual(Expected, Actual, Format('%d / %d * 255', [a, b]));
    end;
end;

procedure TTestBlendTables.TestMultiplicationTable;
var
  a, b: integer;
  Expected, Actual: integer;
begin
  Assert.AreEqual(0, 0);
  for a := 0 to 255 do
    for b := 0 to 255 do
    begin
      Expected := Round(a * b / 255);

      Actual := MulDiv255Table[a, b];

      if (Expected <> Actual) then
        Assert.AreEqual(Expected, Actual, Format('%d * %d / 255', [a, b]));
    end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBlendTables);
  TDUnitX.RegisterTestFixture(TTestBlendModes);

end.
