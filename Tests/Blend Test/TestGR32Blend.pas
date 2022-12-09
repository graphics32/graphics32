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

uses
  {$IFDEF FPC} fpcunit, testregistry, {$ELSE} TestFramework, Windows, {$ENDIF}
  Controls, Types, Classes, SysUtils, Messages, Graphics,
  GR32,
  GR32_Blend,
  GR32_Bindings;

type
  TCustomTestBlendModes = class abstract(TTestCase)
  strict private
    FForeground : PColor32Array;
    FBackground : PColor32Array;
    FReference: PColor32Array;
  protected
    FColorDiff  : Byte;

    procedure CheckColor(ExpectedColor32, ActualColor32: TColor32Entry; Difference: Byte = 1); overload;
    procedure CheckColor(ExpectedColor32, ActualColor32: TColor32Entry; Difference: Byte; const AExtra: string; const AExtraParams: array of const); overload;

    function Rebind(FunctionID: Integer): boolean;
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
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
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
    procedure TestBlendReg; override;
    procedure TestBlendRegEx; override;
    procedure TestBlendMem; override;
    procedure TestBlendMemEx; override;
    procedure TestBlendLine; override;
    procedure TestBlendLineEx; override;
    procedure TestBlendLine1; override;
    procedure TestCombineReg; override;
    procedure TestCombineMem; override;
    procedure TestCombineLine; override;
    procedure TestMergeReg; override;
    procedure TestMergeRegEx; override;
    procedure TestMergeMem; override;
    procedure TestMergeMemEx; override;
    procedure TestMergeLine; override;
    procedure TestMergeLineEx; override;
  end;

  TTestBlendModesAsm = class(TCustomTestBlendModes)
  private
    class function PriorityProcAsm(Info: PFunctionInfo): Integer; static;
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
    procedure TestCombineMem; override;
    procedure TestCombineLine; override;
    procedure TestMergeReg; override;
    procedure TestMergeRegEx; override;
    procedure TestMergeMem; override;
    procedure TestMergeMemEx; override;
    procedure TestMergeLine; override;
    procedure TestMergeLineEx; override;
  end;

  TTestBlendModesMMX = class(TCustomTestBlendModes)
  private
    class function PriorityProcMMX(Info: PFunctionInfo): Integer; static;
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
    procedure TestCombineMem; override;
    procedure TestCombineLine; override;
    procedure TestMergeReg; override;
    procedure TestMergeRegEx; override;
    procedure TestMergeMem; override;
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
    procedure TestBlendReg; override;
    procedure TestBlendRegEx; override;
    procedure TestBlendMem; override;
    procedure TestBlendMemEx; override;
    procedure TestBlendLine; override;
    procedure TestBlendLineEx; override;
    procedure TestBlendLine1; override;
    procedure TestCombineReg; override;
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
  GR32_BlendReference;

{ TTestBlendModes }

procedure TCustomTestBlendModes.SetUp;
begin
  inherited;
  FColorDiff := 1;
  GetMem(FForeground, 256 * SizeOf(TColor32));
  GetMem(FBackground, 256 * SizeOf(TColor32));
  GetMem(FReference, 256 * SizeOf(TColor32));
end;

procedure TCustomTestBlendModes.TearDown;
begin
  inherited;
  Dispose(FForeground);
  Dispose(FBackground);
  Dispose(FReference);
end;


procedure TCustomTestBlendModes.TestBlendReg;
var
  BlendColor32    : TColor32Entry;
  CombinedColor32 : TColor32Entry;
  ExpectedColor32 : TColor32Entry;
  RefIndex, Index : Integer;
begin
  Rebind(FID_EMMS);
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
  CheckColor(ExpectedColor32, CombinedColor32, FColorDiff);

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

      CheckColor(ExpectedColor32, CombinedColor32, FColorDiff);
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
  Rebind(FID_EMMS);
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

  CheckColor(ExpectedColor32, CombinedColor32, FColorDiff);

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

        CheckColor(ExpectedColor32, CombinedColor32, FColorDiff);
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
  Rebind(FID_EMMS);
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

      CheckColor(ExpectedColor32, CombinedColor32, FColorDiff);
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
  Rebind(FID_EMMS);
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

        CheckColor(ExpectedColor32, CombinedColor32, FColorDiff,
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
  Rebind(FID_EMMS);
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
    ExpectedColor32.ARGB := (Index shl 16) or (Index shl 8) or Index;
    CombinedColor32.ARGB := FBackground^[Index];
    CombinedColor32.A := 0;

    CheckColor(ExpectedColor32, CombinedColor32, FColorDiff);
  end;
end;

procedure TCustomTestBlendModes.TestBlendLine1;
var
  CombinedColor32 : TColor32Entry;
  ExpectedColor32 : TColor32Entry;
  Index           : Integer;
begin
  Rebind(FID_EMMS);
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
    CombinedColor32.ARGB := clBlack32;
    TColor32Entry(CombinedColor32).R := Index;
    TColor32Entry(CombinedColor32).G := High(Byte) - Index;
    ExpectedColor32.ARGB := BlendReg(clTrWhite32, CombinedColor32.ARGB);
    ExpectedColor32.A := $FF;

    CombinedColor32.ARGB := FBackground^[Index];
    CombinedColor32.A := $FF;
    EMMS;

    CheckColor(ExpectedColor32, CombinedColor32, FColorDiff);
  end;
end;

procedure TCustomTestBlendModes.TestBlendLineEx;
var
  CombinedColor32    : TColor32Entry;
  ExpectedColor32    : TColor32Entry;
  Index, MasterIndex : Integer;
begin
  Rebind(FID_EMMS);
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

      CombinedColor32.ARGB := FBackground^[Index];
      CombinedColor32.A := 0;

      CheckColor(ExpectedColor32, CombinedColor32, FColorDiff, '(Index: %d, BlendMemEx(FG:%.8X, BG:%.8X, Master:%d)', [Index, FForeground^[Index], clBlack32, MasterIndex shl 5]);
    end;
  end;
end;

procedure TCustomTestBlendModes.TestCombineReg;
var
  BlendColor32    : TColor32Entry;
  CombinedColor32 : TColor32Entry;
  ExpectedColor32 : TColor32Entry;
  RefIndex, Index : Integer;
begin
  Rebind(FID_EMMS);
  if (not Rebind(FID_COMBINEREG)) then
  begin
    Check(True);
    Exit;
  end;

  BlendColor32.ARGB := $FF000000;
  ExpectedColor32.ARGB := CombineReg_Reference(BlendColor32.ARGB, clBlack32, 2);
  CombinedColor32.ARGB := CombineReg(BlendColor32.ARGB, clBlack32, 2);
  EMMS;

  CheckColor(ExpectedColor32, CombinedColor32, FColorDiff);

  BlendColor32.ARGB := $FF000000;
  ExpectedColor32.ARGB := CombineReg_Reference(BlendColor32.ARGB, clBlack32, 255);
  CombinedColor32.ARGB := CombineReg(BlendColor32.ARGB, clBlack32, 255);

  EMMS;

  CheckColor(ExpectedColor32, CombinedColor32, FColorDiff);

  for RefIndex := 0 to High(Byte) do
  begin
    BlendColor32.A := $FF;
    BlendColor32.B := RefIndex;
    BlendColor32.G := RefIndex shr 1;
    BlendColor32.R := RefIndex shr 2;
    for Index := 0 to High(Byte) do
    begin
      ExpectedColor32.ARGB := CombineReg_Reference(BlendColor32.ARGB, clBlack32, Index);
      CombinedColor32.ARGB := CombineReg(BlendColor32.ARGB, clBlack32, Index);

      EMMS;

      CheckColor(ExpectedColor32, CombinedColor32, FColorDiff);
    end;
  end;
end;

procedure TCustomTestBlendModes.TestCombineMem;
var
  BlendColor32    : TColor32Entry;
  CombinedColor32 : TColor32Entry;
  ExpectedColor32 : TColor32Entry;
  RefIndex, Index : Integer;
begin
  Rebind(FID_EMMS);
  if (not Rebind(FID_COMBINEMEM)) then
  begin
    Check(True);
    Exit;
  end;

  for RefIndex := 0 to High(Byte) do
  begin
    BlendColor32.A := $FF;
    BlendColor32.B := RefIndex;
    BlendColor32.G := RefIndex shr 1;
    BlendColor32.R := RefIndex shr 2;
    for Index := 0 to High(Byte) do
    begin
      ExpectedColor32.ARGB := clBlack32;
      CombineMem_Reference(BlendColor32.ARGB, ExpectedColor32.ARGB, Index);
      CombinedColor32.ARGB := clBlack32;
      CombineMem(BlendColor32.ARGB, CombinedColor32.ARGB, Index);

      EMMS;

      CheckColor(ExpectedColor32, CombinedColor32, FColorDiff);
    end;
  end;
end;

procedure TCustomTestBlendModes.TestCombineLine;
var
  ExpectedColor32 : TColor32Entry;
  Index           : Integer;
begin
  Rebind(FID_EMMS);
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

    CheckColor(ExpectedColor32, TColor32Entry(FBackground^[Index]), FColorDiff);
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
  Rebind(FID_EMMS);
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

  CheckColor(ExpectedColor32, CombinedColor32, FColorDiff, 'MergeReg(FG: %.8X, BG: %.8X)', [BlendColor32.ARGB, MergeColor32.ARGB]);

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

        CheckColor(ExpectedColor32, CombinedColor32, FColorDiff,
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
  Rebind(FID_EMMS);
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

  CheckColor(ExpectedColor32, CombinedColor32, FColorDiff, 'MergeRegEx(FG: %.8X, BG: %.8X, Master: %d)', [BlendColor32.ARGB, MergeColor32.ARGB, 128]);

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

          CheckColor(ExpectedColor32, CombinedColor32, FColorDiff,
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
  Rebind(FID_EMMS);
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

        CheckColor(ExpectedColor32, CombinedColor32, FColorDiff,
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
  Rebind(FID_EMMS);
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

          CheckColor(ExpectedColor32, CombinedColor32, FColorDiff,
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
  Rebind(FID_EMMS);
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

        CheckColor(ExpectedColor32, MergedColor32, FColorDiff);
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
  Rebind(FID_EMMS);
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

          CheckColor(ExpectedColor32, MergedColor32, FColorDiff,
            '(Index: %d, RefIndex: %d, Alpha Index: %d, MergeRegEx(FG: %.8X, BG: %.8X, Master: %d))',
            [Index, RefIndex, AlphaIndex, FForeground^[Index], BlendColor32.ARGB, MasterIndex shl 5]);
        end;
      end;
    end;
  end;
end;

{$IFNDEF FPC}
procedure TCustomTestBlendModes.CheckColor(ExpectedColor32, ActualColor32: TColor32Entry; Difference: Byte);
var
  Msg: string;
begin
  FCheckCalled := True;

  if (Abs(ExpectedColor32.A - ActualColor32.A) > Difference) or
    (Abs(ExpectedColor32.R - ActualColor32.R) > Difference) or
    (Abs(ExpectedColor32.G - ActualColor32.G) > Difference) or
    (Abs(ExpectedColor32.B - ActualColor32.B) > Difference) then
  begin
    Msg := Format('Expected color: %.8X, Actual color: %.8X', [ExpectedColor32.ARGB, ActualColor32.ARGB]);
    Fail(Msg, ReturnAddress);
  end;
end;

procedure TCustomTestBlendModes.CheckColor(ExpectedColor32, ActualColor32: TColor32Entry; Difference: Byte; const AExtra: string; const AExtraParams: array of const);
var
  Msg: string;
begin
  FCheckCalled := True;

  if (Abs(ExpectedColor32.A - ActualColor32.A) > Difference) or
    (Abs(ExpectedColor32.R - ActualColor32.R) > Difference) or
    (Abs(ExpectedColor32.G - ActualColor32.G) > Difference) or
    (Abs(ExpectedColor32.B - ActualColor32.B) > Difference) then
  begin
    Msg := Format('Expected color: %.8X, Actual color: %.8X %s', [ExpectedColor32.ARGB, ActualColor32.ARGB, Format(AExtra, AExtraParams)]);
    Fail(Msg, ReturnAddress);
  end;
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

  Status('Performance: ' + FloatToStr(1000 * (Stop - Start) / Freq));
  Check(True);
end;

function TCustomTestBlendModes.Rebind(FunctionID: Integer): boolean;
begin
  Result := BlendRegistry.Rebind(FunctionID, pointer(PriorityProc));
  if (not Result) then
    // Not really an error but we need to indicate that nothing was tested
    Fail('Not implemented');
end;

{$ENDIF}


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
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesPas.TestBlendRegEx;
begin
  FColorDiff := 2;
  inherited;
end;

procedure TTestBlendModesPas.TestBlendMem;
begin
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesPas.TestBlendMemEx;
begin
  FColorDiff := 2;
  inherited;
end;

procedure TTestBlendModesPas.TestBlendLine;
begin
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesPas.TestBlendLine1;
begin
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesPas.TestBlendLineEx;
begin
  FColorDiff := 2;
  inherited;
end;

procedure TTestBlendModesPas.TestCombineReg;
begin
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesPas.TestCombineMem;
begin
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesPas.TestCombineLine;
begin
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesPas.TestMergeReg;
begin
  FColorDiff := 9;
  inherited;
end;

procedure TTestBlendModesPas.TestMergeRegEx;
begin
  FColorDiff := 9;
  inherited;
end;

procedure TTestBlendModesPas.TestMergeMem;
begin
  FColorDiff := 9;
  inherited;
end;

procedure TTestBlendModesPas.TestMergeMemEx;
begin
  FColorDiff := 9;
  inherited;
end;

procedure TTestBlendModesPas.TestMergeLine;
begin
  FColorDiff := 9;
  inherited;
end;

procedure TTestBlendModesPas.TestMergeLineEx;
begin
  FColorDiff := 9;
  inherited;
end;

{ TTestBlendModesAsm }

class function TTestBlendModesAsm.PriorityProc: TFunctionPriority;
begin
  Result := PriorityProcAsm;
end;

class function TTestBlendModesAsm.PriorityProcAsm(Info: PFunctionInfo): Integer;
begin
  if (Info^.CPUFeatures = []) then
    Result := 0
  else
    Result := INVALID_PRIORITY;
end;

procedure TTestBlendModesAsm.TestBlendReg;
begin
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesAsm.TestBlendRegEx;
begin
  FColorDiff := 2;
  inherited;
end;

procedure TTestBlendModesAsm.TestBlendMem;
begin
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesAsm.TestBlendMemEx;
begin
  FColorDiff := 2;
  inherited;
end;

procedure TTestBlendModesAsm.TestBlendLine;
begin
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesAsm.TestBlendLine1;
begin
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesAsm.TestBlendLineEx;
begin
  FColorDiff := 2;
  inherited;
end;

procedure TTestBlendModesAsm.TestCombineReg;
begin
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesAsm.TestCombineMem;
begin
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesAsm.TestCombineLine;
begin
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesAsm.TestMergeReg;
begin
  FColorDiff := 9;
  inherited;
end;

procedure TTestBlendModesAsm.TestMergeRegEx;
begin
  FColorDiff := 9;
  inherited;
end;

procedure TTestBlendModesAsm.TestMergeMem;
begin
  FColorDiff := 9;
  inherited;
end;

procedure TTestBlendModesAsm.TestMergeMemEx;
begin
  FColorDiff := 9;
  inherited;
end;

procedure TTestBlendModesAsm.TestMergeLine;
begin
  FColorDiff := 9;
  inherited;
end;

procedure TTestBlendModesAsm.TestMergeLineEx;
begin
  FColorDiff := 9;
  inherited;
end;


{ TTestBlendModesMMX }

class function TTestBlendModesMMX.PriorityProc: TFunctionPriority;
begin
  Result := PriorityProcMMX;
end;

class function TTestBlendModesMMX.PriorityProcMMX(Info: PFunctionInfo): Integer;
begin
  if (ciMMX in Info^.CPUFeatures) then
    Result := 0
  else
    Result := INVALID_PRIORITY;
end;

procedure TTestBlendModesMMX.TestBlendReg;
begin
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesMMX.TestBlendRegEx;
begin
  FColorDiff := 2;
  inherited;
end;

procedure TTestBlendModesMMX.TestBlendMem;
begin
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesMMX.TestBlendMemEx;
begin
  FColorDiff := 2;
  inherited;
end;

procedure TTestBlendModesMMX.TestBlendLine;
begin
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesMMX.TestBlendLine1;
begin
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesMMX.TestBlendLineEx;
begin
  FColorDiff := 2;
  inherited;
end;

procedure TTestBlendModesMMX.TestCombineReg;
begin
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesMMX.TestCombineMem;
begin
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesMMX.TestCombineLine;
begin
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesMMX.TestMergeReg;
begin
  FColorDiff := 9;
  inherited;
end;

procedure TTestBlendModesMMX.TestMergeRegEx;
begin
  FColorDiff := 9;
  inherited;
end;

procedure TTestBlendModesMMX.TestMergeMem;
begin
  FColorDiff := 9;
  inherited;
end;

procedure TTestBlendModesMMX.TestMergeMemEx;
begin
  FColorDiff := 9;
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
  if (ciSSE2 in Info^.CPUFeatures) then
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
  FColorDiff := 2;
  inherited;
end;

procedure TTestBlendModesSSE2.TestBlendMem;
begin
  inherited;
end;

procedure TTestBlendModesSSE2.TestBlendMemEx;
begin
  FColorDiff := 2;
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
  FColorDiff := 2;
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
  FColorDiff := 4;
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
  FColorDiff := 2;
  inherited;
end;


initialization
  RegisterTest(TTestBlendModesPas.Suite);
  RegisterTest(TTestBlendModesAsm.Suite);
  if ciMMX in CPUFeatures then
    RegisterTest(TTestBlendModesMMX.Suite);
  if ciSSE2 in CPUFeatures then
    RegisterTest(TTestBlendModesSSE2.Suite);

end.
