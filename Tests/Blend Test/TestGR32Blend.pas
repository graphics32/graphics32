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
  protected
    FColorDiff  : Byte;
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
  if Abs(Expected.A - Actual.A) > Difference then Exit;
  if Abs(Expected.R - Actual.R) > Difference then Exit;
  if Abs(Expected.G - Actual.G) > Difference then Exit;
  if Abs(Expected.B - Actual.B) > Difference then Exit;
  Result := True;
end;


{ TTestBlendModes }

procedure TCustomTestBlendModes.SetUp;
begin
  inherited;
  FColorDiff := 1;
  GetMem(FForeground, 256 * SizeOf(TColor32));
  GetMem(FBackground, 256 * SizeOf(TColor32));
end;

procedure TCustomTestBlendModes.TearDown;
begin
  inherited;
  Dispose(FForeground);
  Dispose(FBackground);
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
  CheckTrue(CompareColors(ExpectedColor32, CombinedColor32, FColorDiff),
    'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
    ', but was: ' + IntToHex(CombinedColor32.ARGB, 8));

  for RefIndex := 0 to High(Byte) do begin
    BlendColor32.B := RefIndex;
    BlendColor32.G := RefIndex shr 1;
    BlendColor32.R := RefIndex shr 2;
    for Index := 0 to High(Byte) do begin
      BlendColor32.A := Index;
      ExpectedColor32.ARGB := BlendReg_Reference(BlendColor32.ARGB, clBlack32);
      CombinedColor32.ARGB := BlendReg(BlendColor32.ARGB, clBlack32);
      EMMS;
      CombinedColor32.A := $FF;
      ExpectedColor32.A := $FF;
      CheckTrue(CompareColors(ExpectedColor32, CombinedColor32, FColorDiff),
        'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
        ', but was: ' + IntToHex(CombinedColor32.ARGB, 8));
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
  EMMS;
  CombinedColor32.A := $FF;
  ExpectedColor32.A := $FF;
  CheckTrue(CompareColors(ExpectedColor32, CombinedColor32, FColorDiff),
    'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
    ', but was: ' + IntToHex(CombinedColor32.ARGB, 8));

  for RefIndex := 0 to High(Byte) do begin
    BlendColor32.B := RefIndex;
    BlendColor32.G := RefIndex shr 1;
    BlendColor32.R := RefIndex shr 2;
    for Index := 0 to High(Byte) do begin
      for MasterIndex := 0 to 7 do begin
        BlendColor32.A := Index;
        ExpectedColor32.ARGB := BlendRegEx_Reference(BlendColor32.ARGB,
          clBlack32, TColor32(MasterIndex shl 5));
        CombinedColor32.ARGB := BlendRegEx(BlendColor32.ARGB, clBlack32,
          TColor32(MasterIndex shl 5));
        EMMS;
        CombinedColor32.A := $FF;
        ExpectedColor32.A := $FF;
        CheckTrue(CompareColors(ExpectedColor32, CombinedColor32, FColorDiff),
          'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
          ', but was: ' + IntToHex(CombinedColor32.ARGB, 8));
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
  for RefIndex := 0 to High(Byte) do begin
    BlendColor32.B := RefIndex;
    BlendColor32.G := RefIndex shr 1;
    BlendColor32.R := RefIndex shr 2;
    for Index := 0 to High(Byte) do begin
      BlendColor32.A := Index;
      ExpectedColor32.ARGB := clBlack32;
      BlendMem_Reference(BlendColor32.ARGB, ExpectedColor32.ARGB);
      CombinedColor32.ARGB := clBlack32;
      BlendMem(BlendColor32.ARGB, CombinedColor32.ARGB);
      EMMS;
      CombinedColor32.A := $FF;
      CheckTrue(CompareColors(ExpectedColor32, CombinedColor32, FColorDiff),
        'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
        ', but was: ' + IntToHex(CombinedColor32.ARGB, 8));
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
  for RefIndex := 0 to High(Byte) do begin
    BlendColor32.B := RefIndex;
    BlendColor32.G := RefIndex shr 1;
    BlendColor32.R := RefIndex shr 2;
    for Index := 0 to High(Byte) do begin
      for MasterIndex := 0 to 7 do begin
        BlendColor32.A := Index;
        ExpectedColor32.ARGB := clBlack32;
        BlendMemEx_Reference(BlendColor32.ARGB, ExpectedColor32.ARGB,
          TColor32(MasterIndex shl 5));
        CombinedColor32.ARGB := clBlack32;
        BlendMemEx(BlendColor32.ARGB, CombinedColor32.ARGB,
          TColor32(MasterIndex shl 5));
        EMMS;
        CombinedColor32.A := $FF;
        CheckTrue(CompareColors(ExpectedColor32, CombinedColor32, FColorDiff),
          'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
          ', but was: ' + IntToHex(CombinedColor32.ARGB, 8));
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
    CheckTrue(CompareColors(ExpectedColor32, CombinedColor32, FColorDiff),
      'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
      ', but was: ' + IntToHex(CombinedColor32.ARGB, 8));
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

    CheckTrue(CompareColors(ExpectedColor32, CombinedColor32, FColorDiff),
      'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
      ', but was: ' + IntToHex(CombinedColor32.ARGB, 8));
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
    FBackground^[Index] := clBlack32;
    FForeground^[Index] := clWhite32;
    TColor32Entry(FForeground^[Index]).A := Index;
  end;

  for MasterIndex := 0 to 7 do
  begin
    for Index := 0 to High(Byte) do
      FBackground^[Index] := clBlack32;

    BlendLineEx(PColor32(FForeground), PColor32(FBackground), 256,
      TColor32(MasterIndex shl 5));
    EMMS;

    for Index := 0 to High(Byte) do
      begin
        ExpectedColor32.ARGB := clBlack32;
        BlendMemEx_Reference(FForeground^[Index], ExpectedColor32.ARGB,
          TColor32(MasterIndex shl 5));
        ExpectedColor32.A := 0;
        CombinedColor32.ARGB := FBackground^[Index];
        CombinedColor32.A := 0;
        CheckTrue(CompareColors(ExpectedColor32, CombinedColor32, FColorDiff),
          'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
          ', but was: ' + IntToHex(CombinedColor32.ARGB, 8));
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
  CheckTrue(CompareColors(ExpectedColor32, CombinedColor32, FColorDiff),
    'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
    ', but was: ' + IntToHex(CombinedColor32.ARGB, 8));

  BlendColor32.ARGB := $FF000000;
  ExpectedColor32.ARGB := CombineReg_Reference(BlendColor32.ARGB, clBlack32, 255);
  CombinedColor32.ARGB := CombineReg(BlendColor32.ARGB, clBlack32, 255);
  EMMS;
  CheckTrue(CompareColors(ExpectedColor32, CombinedColor32, FColorDiff),
    'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
    ', but was: ' + IntToHex(CombinedColor32.ARGB, 8));

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
      if not CompareColors(ExpectedColor32, CombinedColor32, FColorDiff) then
      CheckTrue(CompareColors(ExpectedColor32, CombinedColor32, FColorDiff),
        'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
        ', but was: ' + IntToHex(CombinedColor32.ARGB, 8));
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
  for RefIndex := 0 to High(Byte) do begin
    BlendColor32.A := $FF;
    BlendColor32.B := RefIndex;
    BlendColor32.G := RefIndex shr 1;
    BlendColor32.R := RefIndex shr 2;
    for Index := 0 to High(Byte) do begin
      ExpectedColor32.ARGB := clBlack32;
      CombineMem_Reference(BlendColor32.ARGB, ExpectedColor32.ARGB, Index);
      CombinedColor32.ARGB := clBlack32;
      CombineMem(BlendColor32.ARGB, CombinedColor32.ARGB, Index);
      EMMS;
      CheckTrue(CompareColors(ExpectedColor32, CombinedColor32, FColorDiff),
        'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
        ', but was: ' + IntToHex(CombinedColor32.ARGB, 8));
    end;
  end;
end;

procedure TCustomTestBlendModes.TestCombineLine;
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

  CombineLine(PColor32(FForeground), PColor32(FBackground), 256, $FF);

  for Index := 0 to High(Byte) do
  begin
    ExpectedColor32.ARGB := (Index shl 24) or $FFFFFF;
    EMMS;
    CheckTrue(CompareColors(ExpectedColor32,
      TColor32Entry(FBackground^[Index]), FColorDiff),
      'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
      ', but was: ' + IntToHex(CombinedColor32.ARGB, 8));
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
  CAlphaValues : array [0..14] of Byte = ($00, $01, $20, $40, $41, $60, $7F,
    $80, $9F, $BF, $C0, $C1, $DF, $FE, $FF);
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
        CheckTrue(CompareColors(ExpectedColor32, CombinedColor32, FColorDiff),
          'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
          ', but was: ' + IntToHex(CombinedColor32.ARGB, 8) +
          ' (Blend Color: ' + IntToHex(BlendColor32.ARGB, 8) +
          ' Merge Color: ' + IntToHex(MergeColor32 .ARGB, 8) + ')');

  // sample test
  MergeColor32.ARGB := clBlack32;
  for RefIndex := 0 to High(Byte) do begin
    BlendColor32.B := RefIndex;
    BlendColor32.G := RefIndex shr 1;
    BlendColor32.R := RefIndex shr 2;
    for AlphaIndex := 0 to Length(CAlphaValues) - 1 do begin
      BlendColor32.A := AlphaIndex shl 4;
      for Index := 0 to High(Byte) do begin
        MergeColor32.A := Index;
        ExpectedColor32.ARGB := MergeReg_Reference(BlendColor32.ARGB,
          MergeColor32.ARGB);
        CombinedColor32.ARGB := MergeReg(BlendColor32.ARGB, MergeColor32.ARGB);
        EMMS;
        CheckTrue(CompareColors(ExpectedColor32, CombinedColor32, FColorDiff),
          'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
          ', but was: ' + IntToHex(CombinedColor32.ARGB, 8) +
          ' (RefIndex: ' + IntToStr(RefIndex) + ', ' +
          ' AlphaIndex: ' + IntToStr(AlphaIndex) + #10#13 +
          ' Blend Color: ' + IntToHex(BlendColor32.ARGB, 8) +
          ' Merge Color: ' + IntToHex(MergeColor32 .ARGB, 8) + ')');
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
  CAlphaValues : array [0..14] of Byte = ($00, $01, $20, $40, $41, $60, $7F,
    $80, $9F, $BF, $C0, $C1, $DF, $FE, $FF);
begin
  BlendColor32.ARGB := TColor32($1002050B);
  MergeColor32.ARGB := TColor32($01000000);
  ExpectedColor32.ARGB := MergeRegEx_Reference(BlendColor32.ARGB,
    MergeColor32.ARGB, 128);
  CombinedColor32.ARGB := MergeRegEx(BlendColor32.ARGB,
    MergeColor32.ARGB, 128);
  EMMS;
  CheckTrue(CompareColors(ExpectedColor32, CombinedColor32, FColorDiff),
    'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
    ', but was: ' + IntToHex(CombinedColor32.ARGB, 8) +
    ' (Blend Color: ' + IntToHex(BlendColor32.ARGB, 8) +
    ' Merge Color: ' + IntToHex(MergeColor32 .ARGB, 8) + ')');

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
          ExpectedColor32.ARGB := MergeRegEx_Reference(BlendColor32.ARGB,
            MergeColor32.ARGB, TColor32(MasterIndex shl 5));
          CombinedColor32.ARGB := MergeRegEx(BlendColor32.ARGB,
            MergeColor32.ARGB, TColor32(MasterIndex shl 5));
          EMMS;
          CheckTrue(CompareColors(ExpectedColor32, CombinedColor32, FColorDiff),
            'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
            ', but was: ' + IntToHex(CombinedColor32.ARGB, 8) +
            ' (RefIndex: ' + IntToStr(RefIndex) + ', ' +
            ' Alpha Index: ' + IntToStr(AlphaIndex) + ', ' +
            ' Master: ' + IntToStr(MasterIndex shl 5) + #10#13 +
            ' Blend Color: ' + IntToHex(BlendColor32.ARGB, 8) +
            ' Merge Color: ' + IntToHex(MergeColor32 .ARGB, 8) + ')');
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
  CAlphaValues : array [0..14] of Byte = ($00, $01, $20, $40, $41, $60, $7F,
    $80, $9F, $BF, $C0, $C1, $DF, $FE, $FF);
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
        CheckTrue(CompareColors(ExpectedColor32, CombinedColor32, FColorDiff),
          'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
          ', but was: ' + IntToHex(CombinedColor32.ARGB, 8) +
          ' (RefIndex: ' + IntToStr(RefIndex) + ', ' +
          ' AlphaIndex: ' + IntToStr(AlphaIndex) + #10#13 +
          ' Blend Color: ' + IntToHex(BlendColor32.ARGB, 8) +
          ' Merge Color: ' + IntToHex(MergeColor32 .ARGB, 8) + ')');
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
  CAlphaValues : array [0..14] of Byte = ($00, $01, $20, $40, $41, $60, $7F,
    $80, $9F, $BF, $C0, $C1, $DF, $FE, $FF);
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
          CheckTrue(CompareColors(ExpectedColor32, CombinedColor32, FColorDiff),
            'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
            ', but was: ' + IntToHex(CombinedColor32.ARGB, 8) +
            ' (RefIndex: ' + IntToStr(RefIndex) + ', ' +
            ' AlphaIndex: ' + IntToStr(AlphaIndex) + #10#13 +
            ' Blend Color: ' + IntToHex(BlendColor32.ARGB, 8) +
            ' Merge Color: ' + IntToHex(MergeColor32 .ARGB, 8) + ')');
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

      for Index := 0 to High(Byte) do
      begin
        BlendColor32.ARGB := clBlack32;
        BlendColor32.A := CAlphaValues[AlphaIndex];
        ExpectedColor32.ARGB := MergeReg_Reference(FForeground^[Index], BlendColor32.ARGB);
        MergedColor32.ARGB := FBackground^[Index];
        EMMS;
        CheckTrue(CompareColors(ExpectedColor32, MergedColor32, FColorDiff),
          'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
          ', but was: ' + IntToHex(MergedColor32.ARGB, 8));
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
  CAlphaValues : array [0..14] of Byte = ($00, $01, $20, $40, $41, $60, $7F,
    $80, $9F, $BF, $C0, $C1, $DF, $FE, $FF);
begin
  // static test

  // sample test
  for MasterIndex := 0 to 7 do begin
    for RefIndex := 0 to High(Byte) do begin
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
          ExpectedColor32.ARGB := MergeRegEx_Reference(FForeground^[Index],
            BlendColor32.ARGB, TColor32(MasterIndex shl 5));
          EMMS;
          CheckTrue(CompareColors(ExpectedColor32, MergedColor32, FColorDiff),
            'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
            ', but was: ' + IntToHex(MergedColor32.ARGB, 8) + ', ' +
            ' (RefIndex: ' + IntToStr(RefIndex) + ', ' +
            ' Index: ' + IntToStr(Index) + ', ' +
            ' Master Alpha: ' + IntToStr(MasterIndex shl 5) + ', ' +
            ' Alpha Index: ' + IntToStr(AlphaIndex) + ')');
        end;
      end;
    end;
  end;
end;

{$IFNDEF FPC}
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

  for Index := 0 to $7FFFFFF do begin
    BlendReg(BlendColor32.ARGB, clBlack32);
    EMMS;
  end;

  QueryPerformanceCounter(Stop);

  Fail('Performance: ' + FloatToStr(1000 * (Stop - Start) / Freq));
end;
{$ENDIF}


{ TTestBlendModesNative }

function NativePriorityProc(Info: PFunctionInfo): Integer;
begin
  Result := IfThen(Info^.CPUFeatures <= [], 0, INVALID_PRIORITY);
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
  if ciMMX in CPUFeatures then
    RegisterTest(TTestBlendModesMMX.Suite);
  if ciSSE2 in CPUFeatures then
    RegisterTest(TTestBlendModesSSE2.Suite);

end.
