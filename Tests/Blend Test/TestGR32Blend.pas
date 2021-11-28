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
  {$IFDEF FPC} fpcunit, testregistry, {$ELSE} TestFramework, Windows,
  {$ENDIF} Controls, Types, Classes, SysUtils, Messages, Graphics,
  GR32, GR32_Blend, GR32_Bindings;

type
  TCustomTestBlendModes = class(TTestCase)
  strict private
    FForeground : PColor32Array;
    FBackground : PColor32Array;
    FReference: PColor32Array;
  protected
    FColorDiff  : Byte;

    procedure CheckColor(ExpectedColor32, ActualColor32: TColor32Entry; Difference: Byte = 1); overload;
    procedure CheckColor(ExpectedColor32, ActualColor32: TColor32Entry; Difference: Byte; const AExtra: string; const AExtraParams: array of const); overload;

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
    {$IFNDEF FPC}
    procedure PerformanceTest; virtual;
    {$ENDIF}
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TTestBlendModesNative = class(TCustomTestBlendModes)
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
    {$IFNDEF FPC}
    procedure PerformanceTest; override;
    {$ENDIF}
  end;

  TTestBlendModesAsm = class(TCustomTestBlendModes)
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
    {$IFNDEF FPC}
    procedure PerformanceTest; override;
    {$ENDIF}
  end;

  TTestBlendModesMMX = class(TCustomTestBlendModes)
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
    {$IFNDEF FPC}
    procedure PerformanceTest; override;
    {$ENDIF}
  end;

  TTestBlendModesSSE2 = class(TCustomTestBlendModes)
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
    {$IFNDEF FPC}
    procedure PerformanceTest; override;
    {$ENDIF}
  end;

implementation

uses
  Math, GR32_System, GR32_BlendReference;

{ The below codes shall be made available public by GR32_Blend }

const
  FID_EMMS = 0;
  FID_MERGEREG = 1;
  FID_MERGEMEM = 2;
  FID_MERGELINE = 3;
  FID_MERGELINE1 = 4;
  FID_MERGEREGEX = 5;
  FID_MERGEMEMEX = 6;
  FID_MERGELINEEX = 7;
  FID_COMBINEREG = 8;
  FID_COMBINEMEM = 9;
  FID_COMBINELINE = 10;

  FID_BLENDREG = 11;
  FID_BLENDMEM = 12;
  FID_BLENDMEMS = 13;
  FID_BLENDLINE = 14;
  FID_BLENDREGEX = 15;
  FID_BLENDMEMEX = 16;
  FID_BLENDLINEEX = 17;
  FID_BLENDLINE1 = 18;

  FID_COLORMAX = 19;
  FID_COLORMIN = 20;
  FID_COLORAVERAGE = 21;
  FID_COLORADD = 22;
  FID_COLORSUB = 23;
  FID_COLORDIV = 24;
  FID_COLORMODULATE = 25;
  FID_COLORDIFFERENCE = 26;
  FID_COLOREXCLUSION = 27;
  FID_COLORSCALE = 28;
  FID_LIGHTEN = 29;

  FID_BLENDREGRGB = 29;
  FID_BLENDMEMRGB = 30;
{$IFDEF TEST_BLENDMEMRGB128SSE4}
  FID_BLENDMEMRGB128 = 31;
{$ENDIF}


function CompareColors(Expected, Actual: TColor32Entry; Difference: Byte = 1): Boolean;
begin
  Result := False;
  if Abs(Expected.A - Actual.A) > Difference then
    Exit;
  if Abs(Expected.R - Actual.R) > Difference then
    Exit;
  if Abs(Expected.G - Actual.G) > Difference then
    Exit;
  if Abs(Expected.B - Actual.B) > Difference then
    Exit;
  Result := True;
end;


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

  CheckColor(ExpectedColor32, CombinedColor32, FColorDiff, '(Blend Color: %.8X Merge Color: %.8X)', [BlendColor32.ARGB, MergeColor32.ARGB]);

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
          '(RefIndex: %d, AlphaIndex: %d, Blend Color: %.8X Merge Color: %.8X)', [RefIndex, AlphaIndex, BlendColor32.ARGB, MergeColor32.ARGB]);
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
  BlendColor32.ARGB := TColor32($1002050B);
  MergeColor32.ARGB := TColor32($01000000);
  ExpectedColor32.ARGB := MergeRegEx_Reference(BlendColor32.ARGB, MergeColor32.ARGB, 128);
  CombinedColor32.ARGB := MergeRegEx(BlendColor32.ARGB, MergeColor32.ARGB, 128);

  EMMS;

  CheckColor(ExpectedColor32, CombinedColor32, FColorDiff, '(Blend Color: %.8X, Merge Color: %.8X)', [BlendColor32.ARGB, MergeColor32.ARGB]);

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
            '(RefIndex: %d, Alpha Index: %d, Master: %.8X, Blend Color: %.8X, Merge Color: %.8X)',
            [RefIndex, AlphaIndex, MasterIndex shl 5, BlendColor32.ARGB, MergeColor32.ARGB]);
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
          '(RefIndex: %d, Alpha Index: %d, Blend Color: %.8X, Merge Color: %.8X)',
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
            '(RefIndex: %d, Alpha Index: %d, Blend Color: %.8X, Merge Color: %.8X)',
            [RefIndex, AlphaIndex, BlendColor32.ARGB, MergeColor32.ARGB]);
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
  CAlphaValues : array [0..14] of Byte = ($00, $01, $20, $40, $41, $60, $7F,
    $80, $9F, $BF, $C0, $C1, $DF, $FE, $FF);
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
            '(RefIndex: %d, Alpha Index: %d, Master: %d, Blend Color: %.8X, Merge Color: %.8X)',
            [RefIndex, AlphaIndex, MasterIndex shl 5, BlendColor32.ARGB, MergedColor32.ARGB]);
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
{$ENDIF}


{ TTestBlendModesNative }

function NativePriorityProc(Info: PFunctionInfo): Integer;
begin
  if (Info^.Flags and BlendBindingFlagPascal <> 0) then
    Result := 0
  else
    Result := INVALID_PRIORITY;
end;

procedure TTestBlendModesNative.TestBlendReg;
begin
  BlendRegistry.Rebind(FID_EMMS, NativePriorityProc);
  BlendRegistry.Rebind(FID_BLENDREG, NativePriorityProc);
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesNative.TestBlendRegEx;
begin
  BlendRegistry.Rebind(FID_EMMS, NativePriorityProc);
  BlendRegistry.Rebind(FID_BLENDREGEX, NativePriorityProc);
  FColorDiff := 2;
  inherited;
end;

procedure TTestBlendModesNative.TestBlendMem;
begin
  BlendRegistry.Rebind(FID_EMMS, NativePriorityProc);
  BlendRegistry.Rebind(FID_BLENDMEM, NativePriorityProc);
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesNative.TestBlendMemEx;
begin
  BlendRegistry.Rebind(FID_EMMS, NativePriorityProc);
  BlendRegistry.Rebind(FID_BLENDMEMEX, NativePriorityProc);
  FColorDiff := 2;
  inherited;
end;

procedure TTestBlendModesNative.TestBlendLine;
begin
  BlendRegistry.Rebind(FID_EMMS, NativePriorityProc);
  BlendRegistry.Rebind(FID_BLENDLINE, NativePriorityProc);
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesNative.TestBlendLine1;
begin
  BlendRegistry.Rebind(FID_EMMS, NativePriorityProc);
  BlendRegistry.Rebind(FID_BLENDLINE1, NativePriorityProc);
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesNative.TestBlendLineEx;
begin
  BlendRegistry.Rebind(FID_EMMS, NativePriorityProc);
  BlendRegistry.Rebind(FID_BLENDLINEEX, NativePriorityProc);
  FColorDiff := 2;
  inherited;
end;

procedure TTestBlendModesNative.TestCombineReg;
begin
  BlendRegistry.Rebind(FID_EMMS, NativePriorityProc);
  BlendRegistry.Rebind(FID_COMBINEREG, NativePriorityProc);
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesNative.TestCombineMem;
begin
  BlendRegistry.Rebind(FID_EMMS, NativePriorityProc);
  BlendRegistry.Rebind(FID_COMBINEMEM, NativePriorityProc);
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesNative.TestCombineLine;
begin
  BlendRegistry.Rebind(FID_EMMS, NativePriorityProc);
  BlendRegistry.Rebind(FID_COMBINELINE, NativePriorityProc);
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesNative.TestMergeReg;
begin
  BlendRegistry.Rebind(FID_EMMS, NativePriorityProc);
  BlendRegistry.Rebind(FID_MERGEREG, NativePriorityProc);
  FColorDiff := 9;
  inherited;
end;

procedure TTestBlendModesNative.TestMergeRegEx;
begin
  BlendRegistry.Rebind(FID_EMMS, NativePriorityProc);
  BlendRegistry.Rebind(FID_MERGEREG, NativePriorityProc);
  BlendRegistry.Rebind(FID_MERGEREGEX, NativePriorityProc);
  FColorDiff := 9;
  inherited;
end;

procedure TTestBlendModesNative.TestMergeMem;
begin
  BlendRegistry.Rebind(FID_EMMS, NativePriorityProc);
  BlendRegistry.Rebind(FID_MERGEMEM, NativePriorityProc);
  FColorDiff := 9;
  inherited;
end;

procedure TTestBlendModesNative.TestMergeMemEx;
begin
  BlendRegistry.Rebind(FID_EMMS, NativePriorityProc);
  BlendRegistry.Rebind(FID_MERGEREG, NativePriorityProc);
  BlendRegistry.Rebind(FID_MERGEMEM, NativePriorityProc);
  BlendRegistry.Rebind(FID_MERGEMEMEX, NativePriorityProc);
  FColorDiff := 9;
  inherited;
end;

procedure TTestBlendModesNative.TestMergeLine;
begin
  BlendRegistry.Rebind(FID_EMMS, NativePriorityProc);
  BlendRegistry.Rebind(FID_MERGEREG, NativePriorityProc);
  BlendRegistry.Rebind(FID_MERGELINE, NativePriorityProc);
  FColorDiff := 9;
  inherited;
end;

procedure TTestBlendModesNative.TestMergeLineEx;
begin
  BlendRegistry.Rebind(FID_EMMS, NativePriorityProc);
  BlendRegistry.Rebind(FID_MERGELINEEX, NativePriorityProc);
  FColorDiff := 9;
  inherited;
end;

{$IFNDEF FPC}
procedure TTestBlendModesNative.PerformanceTest;
begin
  BlendRegistry.RebindAll(NativePriorityProc);
  inherited;
end;
{$ENDIF}


{ TTestBlendModesAsm }

function AsmPriorityProc(Info: PFunctionInfo): Integer;
begin
  Result := IfThen(Info^.CPUFeatures <= [], 0, INVALID_PRIORITY);
end;

procedure TTestBlendModesAsm.TestBlendReg;
begin
  BlendRegistry.Rebind(FID_EMMS, AsmPriorityProc);
  BlendRegistry.Rebind(FID_BLENDREG, AsmPriorityProc);
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesAsm.TestBlendRegEx;
begin
  BlendRegistry.Rebind(FID_EMMS, AsmPriorityProc);
  BlendRegistry.Rebind(FID_BLENDREGEX, AsmPriorityProc);
  FColorDiff := 2;
  inherited;
end;

procedure TTestBlendModesAsm.TestBlendMem;
begin
  BlendRegistry.Rebind(FID_EMMS, AsmPriorityProc);
  BlendRegistry.Rebind(FID_BLENDMEM, AsmPriorityProc);
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesAsm.TestBlendMemEx;
begin
  BlendRegistry.Rebind(FID_EMMS, AsmPriorityProc);
  BlendRegistry.Rebind(FID_BLENDMEMEX, AsmPriorityProc);
  FColorDiff := 2;
  inherited;
end;

procedure TTestBlendModesAsm.TestBlendLine;
begin
  BlendRegistry.Rebind(FID_EMMS, AsmPriorityProc);
  BlendRegistry.Rebind(FID_BLENDLINE, AsmPriorityProc);
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesAsm.TestBlendLine1;
begin
  BlendRegistry.Rebind(FID_EMMS, AsmPriorityProc);
  BlendRegistry.Rebind(FID_BLENDLINE1, AsmPriorityProc);
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesAsm.TestBlendLineEx;
begin
  BlendRegistry.Rebind(FID_EMMS, AsmPriorityProc);
  BlendRegistry.Rebind(FID_BLENDLINEEX, AsmPriorityProc);
  FColorDiff := 2;
  inherited;
end;

procedure TTestBlendModesAsm.TestCombineReg;
begin
  BlendRegistry.Rebind(FID_EMMS, AsmPriorityProc);
  BlendRegistry.Rebind(FID_COMBINEREG, AsmPriorityProc);
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesAsm.TestCombineMem;
begin
  BlendRegistry.Rebind(FID_EMMS, AsmPriorityProc);
  BlendRegistry.Rebind(FID_COMBINEMEM, AsmPriorityProc);
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesAsm.TestCombineLine;
begin
  BlendRegistry.Rebind(FID_EMMS, AsmPriorityProc);
  BlendRegistry.Rebind(FID_COMBINELINE, AsmPriorityProc);
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesAsm.TestMergeReg;
begin
  BlendRegistry.Rebind(FID_EMMS, AsmPriorityProc);
  BlendRegistry.Rebind(FID_MERGEREG, AsmPriorityProc);
  FColorDiff := 9;
  inherited;
end;

procedure TTestBlendModesAsm.TestMergeRegEx;
begin
  BlendRegistry.Rebind(FID_EMMS, AsmPriorityProc);
  BlendRegistry.Rebind(FID_MERGEREG, AsmPriorityProc);
  BlendRegistry.Rebind(FID_MERGEREGEX, AsmPriorityProc);
  FColorDiff := 9;
  inherited;
end;

procedure TTestBlendModesAsm.TestMergeMem;
begin
  BlendRegistry.Rebind(FID_EMMS, AsmPriorityProc);
  BlendRegistry.Rebind(FID_MERGEMEM, AsmPriorityProc);
  FColorDiff := 9;
  inherited;
end;

procedure TTestBlendModesAsm.TestMergeMemEx;
begin
  BlendRegistry.Rebind(FID_EMMS, AsmPriorityProc);
  BlendRegistry.Rebind(FID_MERGEREG, AsmPriorityProc);
  BlendRegistry.Rebind(FID_MERGEMEM, AsmPriorityProc);
  BlendRegistry.Rebind(FID_MERGEMEMEX, AsmPriorityProc);
  FColorDiff := 9;
  inherited;
end;

procedure TTestBlendModesAsm.TestMergeLine;
begin
  BlendRegistry.Rebind(FID_EMMS, AsmPriorityProc);
  BlendRegistry.Rebind(FID_MERGEREG, AsmPriorityProc);
  BlendRegistry.Rebind(FID_MERGELINE, AsmPriorityProc);
  FColorDiff := 9;
  inherited;
end;

procedure TTestBlendModesAsm.TestMergeLineEx;
begin
  BlendRegistry.Rebind(FID_EMMS, AsmPriorityProc);
  BlendRegistry.Rebind(FID_MERGELINEEX, AsmPriorityProc);
  FColorDiff := 9;
  inherited;
end;

{$IFNDEF FPC}
procedure TTestBlendModesAsm.PerformanceTest;
begin
  BlendRegistry.RebindAll(AsmPriorityProc);
  inherited;
end;
{$ENDIF}


{ TTestBlendModesMMX }

function MMXPriorityProc(Info: PFunctionInfo): Integer;
begin
  Result := IfThen(Info^.CPUFeatures <= [ciMMX], 0, INVALID_PRIORITY);
end;

procedure TTestBlendModesMMX.TestBlendReg;
begin
  BlendRegistry.Rebind(FID_EMMS, MMXPriorityProc);
  BlendRegistry.Rebind(FID_BLENDREG, MMXPriorityProc);
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesMMX.TestBlendRegEx;
begin
  BlendRegistry.Rebind(FID_EMMS, MMXPriorityProc);
  BlendRegistry.Rebind(FID_BLENDREGEX, MMXPriorityProc);
  FColorDiff := 2;
  inherited;
end;

procedure TTestBlendModesMMX.TestBlendMem;
begin
  BlendRegistry.Rebind(FID_EMMS, MMXPriorityProc);
  BlendRegistry.Rebind(FID_BLENDMEM, MMXPriorityProc);
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesMMX.TestBlendMemEx;
begin
  BlendRegistry.Rebind(FID_EMMS, MMXPriorityProc);
  BlendRegistry.Rebind(FID_BLENDMEMEX, MMXPriorityProc);
  FColorDiff := 2;
  inherited;
end;

procedure TTestBlendModesMMX.TestBlendLine;
begin
  BlendRegistry.Rebind(FID_EMMS, MMXPriorityProc);
  BlendRegistry.Rebind(FID_BLENDLINE, MMXPriorityProc);
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesMMX.TestBlendLine1;
begin
  BlendRegistry.Rebind(FID_EMMS, MMXPriorityProc);
  BlendRegistry.Rebind(FID_BLENDLINE1, MMXPriorityProc);
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesMMX.TestBlendLineEx;
begin
  BlendRegistry.Rebind(FID_EMMS, MMXPriorityProc);
  BlendRegistry.Rebind(FID_BLENDLINEEX, MMXPriorityProc);
  FColorDiff := 2;
  inherited;
end;

procedure TTestBlendModesMMX.TestCombineReg;
begin
  BlendRegistry.Rebind(FID_EMMS, MMXPriorityProc);
  BlendRegistry.Rebind(FID_COMBINEREG, MMXPriorityProc);
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesMMX.TestCombineMem;
begin
  BlendRegistry.Rebind(FID_EMMS, MMXPriorityProc);
  BlendRegistry.Rebind(FID_COMBINEMEM, MMXPriorityProc);
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesMMX.TestCombineLine;
begin
  BlendRegistry.Rebind(FID_EMMS, MMXPriorityProc);
  BlendRegistry.Rebind(FID_COMBINELINE, MMXPriorityProc);
  FColorDiff := 1;
  inherited;
end;

procedure TTestBlendModesMMX.TestMergeReg;
begin
  BlendRegistry.Rebind(FID_EMMS, MMXPriorityProc);
  BlendRegistry.Rebind(FID_MERGEREG, MMXPriorityProc);
  FColorDiff := 9;
  inherited;
end;

procedure TTestBlendModesMMX.TestMergeRegEx;
begin
  BlendRegistry.Rebind(FID_EMMS, MMXPriorityProc);
  BlendRegistry.Rebind(FID_MERGEREGEX, MMXPriorityProc);
  FColorDiff := 9;
  inherited;
end;

procedure TTestBlendModesMMX.TestMergeMem;
begin
  BlendRegistry.Rebind(FID_EMMS, MMXPriorityProc);
  BlendRegistry.Rebind(FID_MERGEMEM, MMXPriorityProc);
  FColorDiff := 9;
  inherited;
end;

procedure TTestBlendModesMMX.TestMergeMemEx;
begin
  BlendRegistry.Rebind(FID_EMMS, MMXPriorityProc);
  BlendRegistry.Rebind(FID_MERGEMEMEX, MMXPriorityProc);
  FColorDiff := 9;
  inherited;
end;

procedure TTestBlendModesMMX.TestMergeLine;
begin
  BlendRegistry.Rebind(FID_EMMS, MMXPriorityProc);
  BlendRegistry.Rebind(FID_MERGELINE, MMXPriorityProc);
  inherited;
end;

procedure TTestBlendModesMMX.TestMergeLineEx;
begin
  BlendRegistry.Rebind(FID_EMMS, MMXPriorityProc);
  BlendRegistry.Rebind(FID_MERGELINEEX, MMXPriorityProc);
  inherited;
end;

{$IFNDEF FPC}
procedure TTestBlendModesMMX.PerformanceTest;
begin
  BlendRegistry.RebindAll(MMXPriorityProc);
  inherited;
end;
{$ENDIF}


{ TTestBlendModesSSE2 }

function SSE2PriorityProc(Info: PFunctionInfo): Integer;
begin
  Result := IfThen(Info^.CPUFeatures <= [ciSSE2], 0, INVALID_PRIORITY);
end;

procedure TTestBlendModesSSE2.TestBlendReg;
begin
  BlendRegistry.Rebind(FID_EMMS, SSE2PriorityProc);
  BlendRegistry.Rebind(FID_BLENDREG, SSE2PriorityProc);
  inherited;
end;

procedure TTestBlendModesSSE2.TestBlendRegEx;
begin
  BlendRegistry.Rebind(FID_EMMS, SSE2PriorityProc);
  BlendRegistry.Rebind(FID_BLENDREGEX, SSE2PriorityProc);
  FColorDiff := 2;
  inherited;
end;

procedure TTestBlendModesSSE2.TestBlendMem;
begin
  BlendRegistry.Rebind(FID_EMMS, SSE2PriorityProc);
  BlendRegistry.Rebind(FID_BLENDMEM, SSE2PriorityProc);
  inherited;
end;

procedure TTestBlendModesSSE2.TestBlendMemEx;
begin
  BlendRegistry.Rebind(FID_EMMS, SSE2PriorityProc);
  BlendRegistry.Rebind(FID_BLENDMEMEX, SSE2PriorityProc);
  FColorDiff := 2;
  inherited;
end;

procedure TTestBlendModesSSE2.TestBlendLine;
begin
  BlendRegistry.Rebind(FID_EMMS, SSE2PriorityProc);
  BlendRegistry.Rebind(FID_BLENDLINE, SSE2PriorityProc);
  inherited;
end;

procedure TTestBlendModesSSE2.TestBlendLine1;
begin
  BlendRegistry.Rebind(FID_EMMS, SSE2PriorityProc);
  BlendRegistry.Rebind(FID_BLENDLINE1, SSE2PriorityProc);
  inherited;
end;

procedure TTestBlendModesSSE2.TestBlendLineEx;
begin
  BlendRegistry.Rebind(FID_EMMS, SSE2PriorityProc);
  BlendRegistry.Rebind(FID_BLENDLINEEX, SSE2PriorityProc);
  FColorDiff := 2;
  inherited;
end;

procedure TTestBlendModesSSE2.TestCombineReg;
begin
  BlendRegistry.Rebind(FID_EMMS, SSE2PriorityProc);
  BlendRegistry.Rebind(FID_COMBINEREG, SSE2PriorityProc);
  inherited;
end;

procedure TTestBlendModesSSE2.TestCombineMem;
begin
  BlendRegistry.Rebind(FID_EMMS, SSE2PriorityProc);
  BlendRegistry.Rebind(FID_COMBINEMEM, SSE2PriorityProc);
  inherited;
end;

procedure TTestBlendModesSSE2.TestCombineLine;
begin
  BlendRegistry.Rebind(FID_EMMS, SSE2PriorityProc);
  BlendRegistry.Rebind(FID_COMBINELINE, SSE2PriorityProc);
  inherited;
end;

procedure TTestBlendModesSSE2.TestMergeReg;
begin
  BlendRegistry.Rebind(FID_EMMS, SSE2PriorityProc);
  BlendRegistry.Rebind(FID_MERGEREG, SSE2PriorityProc);
  inherited;
end;

procedure TTestBlendModesSSE2.TestMergeRegEx;
begin
  BlendRegistry.Rebind(FID_EMMS, SSE2PriorityProc);
  BlendRegistry.Rebind(FID_MERGEREGEX, SSE2PriorityProc);
  inherited;
end;

procedure TTestBlendModesSSE2.TestMergeMem;
begin
  BlendRegistry.Rebind(FID_EMMS, SSE2PriorityProc);
  BlendRegistry.Rebind(FID_MERGEMEM, SSE2PriorityProc);
  inherited;
end;

procedure TTestBlendModesSSE2.TestMergeMemEx;
begin
  BlendRegistry.Rebind(FID_EMMS, SSE2PriorityProc);
  BlendRegistry.Rebind(FID_MERGEMEMEX, SSE2PriorityProc);
  inherited;
end;

procedure TTestBlendModesSSE2.TestMergeLine;
begin
  BlendRegistry.Rebind(FID_EMMS, SSE2PriorityProc);
  BlendRegistry.Rebind(FID_MERGELINE, SSE2PriorityProc);
  inherited;
end;

procedure TTestBlendModesSSE2.TestMergeLineEx;
begin
  BlendRegistry.Rebind(FID_EMMS, SSE2PriorityProc);
  BlendRegistry.Rebind(FID_MERGELINEEX, SSE2PriorityProc);
  FColorDiff := 2;
  inherited;
end;

{$IFNDEF FPC}
procedure TTestBlendModesSSE2.PerformanceTest;
begin
  BlendRegistry.Rebind(FID_EMMS, SSE2PriorityProc);
  BlendRegistry.RebindAll(SSE2PriorityProc);
  inherited;
end;
{$ENDIF}


initialization
  RegisterTest(TTestBlendModesNative.Suite);
  RegisterTest(TTestBlendModesAsm.Suite);
  if ciMMX in CPUFeatures then
    RegisterTest(TTestBlendModesMMX.Suite);
  if ciSSE2 in CPUFeatures then
    RegisterTest(TTestBlendModesSSE2.Suite);

end.
