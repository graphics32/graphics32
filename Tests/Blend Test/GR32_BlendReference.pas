unit GR32_BlendReference;

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
 * Contributor(s):
 *   Christian-W. Budde
 *      - 2011/07/07 - MMX Blendmodes
 *
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I ..\..\Source\GR32.inc}

// Define UseLookupTables to have the reference implementations use the division and div255 lookup tables.
// This is not recommended as it would make the reference implementations replicate any errors caused by those tables.
{-$DEFINE UseLookupTables}

// Contrary to the documentation the result alpha of a Blend operation is either left as-is or set to $FF.
// Define BlendAlpha255 to have the result alpha forced to 255.
{$DEFINE BlendAlpha255}

// Define USE_DIV255 to have the reference implementations use the Div255 function instead of Round(x/255).
{-$define USE_DIV255}

uses
  SysUtils,
  GR32,
  GR32_Bindings,
  GR32_Blend;

function BlendReg_Reference(Foreground, Background: TColor32): TColor32;
procedure BlendMem_Reference(Foreground: TColor32; var Background: TColor32);
function BlendRegEx_Reference(Foreground, Background: TColor32; Master: Cardinal): TColor32;
procedure BlendMemEx_Reference(Foreground: TColor32; var Background: TColor32; Master: Cardinal);
procedure BlendLine_Reference(Source, Destination: PColor32; Count: Integer);
procedure BlendLineEx_Reference(Source, Destination: PColor32; Count: Integer; Master: Cardinal);

function CombineReg_Reference(ForeGround, Background: TColor32; Weight: Cardinal): TColor32;
procedure CombineMem_Reference(ForeGround: TColor32; var Background: TColor32; Weight: Cardinal);
procedure CombineLine_Reference(Source, Destination: PColor32; Count: Integer; Weight: Cardinal);

function MergeReg_Reference(Foreground, Background: TColor32): TColor32;
procedure MergeMem_Reference(Foreground: TColor32; var Background: TColor32);
function MergeRegEx_Reference(Foreground, Background: TColor32; Master: Cardinal): TColor32;
procedure MergeMemEx_Reference(Foreground: TColor32; var Background: TColor32; Master: Cardinal);
procedure MergeLine_Reference(Source, Destination: PColor32; Count: Cardinal);
procedure MergeLineEx_Reference(Source, Destination: PColor32; Count: Cardinal; Master: Cardinal);

implementation

uses
  Math,
  GR32_LowLevel;

{$IFDEF UseLookupTables}
var
  GRcTable: array [Byte, Byte] of Byte;
  GDivTable: array [Byte, Byte] of Byte;
{$ENDIF}

const
  COne255th : Double = 1 / 255;

function BlendReg_Reference(Foreground, Background: TColor32): TColor32;
var
  ForegroundColor : TColor32Entry absolute Foreground;
  BackgroundColor : TColor32Entry absolute Background;
{$IFDEF UseLookupTables}
  AlphaForeground : PByteArray;
  AlphaBackground : PByteArray;
{$ELSE}
  Scale : array [0..1] of Double;
{$ENDIF}
begin
  if ForegroundColor.A = 0 then
  begin
    Result := Background;
    Exit;
  end;

  if ForegroundColor.A = $FF then
  begin
    Result := Foreground;
    Exit;
  end;

{$IFDEF UseLookupTables}
  AlphaForeground := @GDivTable[ForegroundColor.A];
  AlphaBackground := @GDivTable[not ForegroundColor.A];
{$ifdef BlendAlpha255}
  BackgroundColor.A := $FF;
{$else BlendAlpha255}
  BackgroundColor.A := AlphaForeground[ForegroundColor.A] + AlphaBackground[BackgroundColor.A];
{$endif BlendAlpha255}
  BackgroundColor.R := AlphaForeground[ForegroundColor.R] + AlphaBackground[BackgroundColor.R];
  BackgroundColor.G := AlphaForeground[ForegroundColor.G] + AlphaBackground[BackgroundColor.G];
  BackgroundColor.B := AlphaForeground[ForegroundColor.B] + AlphaBackground[BackgroundColor.B];
{$ELSE}
  Scale[0] := ForegroundColor.A * COne255th;
  Scale[1] := 1 - Scale[0];
{$ifdef BlendAlpha255}
  BackgroundColor.A := $FF;
{$else BlendAlpha255}
  BackgroundColor.A := EnsureRange(Round(Scale[1] * BackgroundColor.A + Scale[0] * ForegroundColor.A), 0, $FF);
{$endif BlendAlpha255}
  BackgroundColor.R := EnsureRange(Round(Scale[1] * BackgroundColor.R + Scale[0] * ForegroundColor.R), 0, $FF);
  BackgroundColor.G := EnsureRange(Round(Scale[1] * BackgroundColor.G + Scale[0] * ForegroundColor.G), 0, $FF);
  BackgroundColor.B := EnsureRange(Round(Scale[1] * BackgroundColor.B + Scale[0] * ForegroundColor.B), 0, $FF);
{$ENDIF}

  Result := Background;
end;

procedure BlendMem_Reference(Foreground: TColor32; var Background: TColor32);
var
  ForegroundColor : TColor32Entry absolute Foreground;
  BackgroundColor : TColor32Entry absolute Background;
{$IFDEF UseLookupTables}
  AlphaForeground : PByteArray;
  AlphaBackground : PByteArray;
{$ELSE}
  Scale : array [0..1] of Double;
{$ENDIF}
begin
  if ForegroundColor.A = 0 then
    Exit;

  if ForegroundColor.A = $FF then
  begin
    Background := Foreground;
    Exit;
  end;

{$IFDEF UseLookupTables}
  AlphaForeground := @GDivTable[ForegroundColor.A];
  AlphaBackground := @GDivTable[not ForegroundColor.A];
{$ifdef BlendAlpha255}
  BackgroundColor.A := $FF;
{$else BlendAlpha255}
  BackgroundColor.A := AlphaForeground[ForegroundColor.A] + AlphaBackground[BackgroundColor.A];
{$endif BlendAlpha255}
  BackgroundColor.R := AlphaForeground[ForegroundColor.R] + AlphaBackground[BackgroundColor.R];
  BackgroundColor.G := AlphaForeground[ForegroundColor.G] + AlphaBackground[BackgroundColor.G];
  BackgroundColor.B := AlphaForeground[ForegroundColor.B] + AlphaBackground[BackgroundColor.B];
{$ELSE}
  Scale[0] := ForegroundColor.A * COne255th;
  Scale[1] := 1.0 - Scale[0];
{$ifdef BlendAlpha255}
  BackgroundColor.A := $FF;
{$else BlendAlpha255}
  BackgroundColor.A := EnsureRange(Round(Scale[1] * BackgroundColor.A + Scale[0] * ForegroundColor.A), 0, $FF);
{$endif BlendAlpha255}
  BackgroundColor.R := EnsureRange(Round(Scale[1] * BackgroundColor.R + Scale[0] * ForegroundColor.R), 0, $FF);
  BackgroundColor.G := EnsureRange(Round(Scale[1] * BackgroundColor.G + Scale[0] * ForegroundColor.G), 0, $FF);
  BackgroundColor.B := EnsureRange(Round(Scale[1] * BackgroundColor.B + Scale[0] * ForegroundColor.B), 0, $FF);
{$ENDIF}
end;

function BlendRegEx_Reference(Foreground, Background: TColor32; Master: Cardinal): TColor32;
var
  ForegroundColor : TColor32Entry absolute Foreground;
  BackgroundColor : TColor32Entry absolute Background;
  MasterAlpha     : Byte absolute Master;
{$IFDEF UseLookupTables}
  AlphaForeground : PByteArray;
  AlphaBackground : PByteArray;
{$ELSE}
  Scale : array [0..1] of Double;
{$ENDIF}
begin
  if (ForegroundColor.A = 0) or (MasterAlpha = 0) then
  begin
    Result := Background;
    Exit;
  end;

  if (ForegroundColor.A = $FF) and (MasterAlpha = $FF) then
  begin
    Result := Foreground;
    Exit;
  end;

{$IFDEF UseLookupTables}
  AlphaForeground := @GDivTable[MasterAlpha];
  MasterAlpha := AlphaForeground[ForegroundColor.A];
  AlphaForeground := @GDivTable[MasterAlpha];
  AlphaBackground := @GDivTable[not MasterAlpha];

{$ifdef BlendAlpha255}
  BackgroundColor.A := $FF;
{$else BlendAlpha255}
  BackgroundColor.A := AlphaForeground[ForegroundColor.A] + AlphaBackground[BackgroundColor.A];
{$endif BlendAlpha255}
  BackgroundColor.R := AlphaForeground[ForegroundColor.R] + AlphaBackground[BackgroundColor.R];
  BackgroundColor.G := AlphaForeground[ForegroundColor.G] + AlphaBackground[BackgroundColor.G];
  BackgroundColor.B := AlphaForeground[ForegroundColor.B] + AlphaBackground[BackgroundColor.B];
{$ELSE}
  Scale[0] := MasterAlpha * ForegroundColor.A * Sqr(COne255th);
  Scale[1] := 1 - Scale[0];
{$ifdef BlendAlpha255}
  BackgroundColor.A := $FF;
{$else BlendAlpha255}
  BackgroundColor.A := EnsureRange(Round(Scale[1] * BackgroundColor.A + Scale[0] * ForegroundColor.A), 0, $FF);
{$endif BlendAlpha255}
  BackgroundColor.R := EnsureRange(Round(Scale[1] * BackgroundColor.R + Scale[0] * ForegroundColor.R), 0, $FF);
  BackgroundColor.G := EnsureRange(Round(Scale[1] * BackgroundColor.G + Scale[0] * ForegroundColor.G), 0, $FF);
  BackgroundColor.B := EnsureRange(Round(Scale[1] * BackgroundColor.B + Scale[0] * ForegroundColor.B), 0, $FF);
{$ENDIF}
  Result := Background;
end;

procedure BlendMemEx_Reference(Foreground: TColor32; var Background: TColor32; Master: Cardinal);
var
  ForegroundColor : TColor32Entry absolute Foreground;
  BackgroundColor : TColor32Entry absolute Background;
  MasterAlpha     : Byte absolute Master;
{$IFDEF UseLookupTables}
  AlphaForeground : PByteArray;
  AlphaBackground : PByteArray;
{$ELSE}
  Scale : array [0..1] of Double;
{$ENDIF}
begin
  if (MasterAlpha = 0) or (ForegroundColor.A = 0) then
    Exit;

  if (MasterAlpha = $FF) and (ForegroundColor.A = $FF) then
  begin
    Background := Foreground;
    Exit;
  end;

{$IFDEF UseLookupTables}
  AlphaForeground := @GDivTable[MasterAlpha];
  MasterAlpha := AlphaForeground[ForegroundColor.A];
  AlphaForeground := @GDivTable[MasterAlpha];
  AlphaBackground := @GDivTable[not MasterAlpha];

{$ifdef BlendAlpha255}
  BackgroundColor.A := $FF;
{$else BlendAlpha255}
  BackgroundColor.A := AlphaForeground[ForegroundColor.A] + AlphaBackground[BackgroundColor.A];
{$endif BlendAlpha255}
  BackgroundColor.R := AlphaForeground[ForegroundColor.R] + AlphaBackground[BackgroundColor.R];
  BackgroundColor.G := AlphaForeground[ForegroundColor.G] + AlphaBackground[BackgroundColor.G];
  BackgroundColor.B := AlphaForeground[ForegroundColor.B] + AlphaBackground[BackgroundColor.B];
{$ELSE}
  Scale[0] := MasterAlpha * ForegroundColor.A * Sqr(COne255th);
  Scale[1] := 1.0 - Scale[0];
{$ifdef BlendAlpha255}
  BackgroundColor.A := $FF;
{$else BlendAlpha255}
  BackgroundColor.A := EnsureRange(Round(Scale[1] * BackgroundColor.A + Scale[0] * ForegroundColor.A), 0, $FF);
{$endif BlendAlpha255}
  BackgroundColor.R := EnsureRange(Round(Scale[1] * BackgroundColor.R + Scale[0] * ForegroundColor.R), 0, $FF);
  BackgroundColor.G := EnsureRange(Round(Scale[1] * BackgroundColor.G + Scale[0] * ForegroundColor.G), 0, $FF);
  BackgroundColor.B := EnsureRange(Round(Scale[1] * BackgroundColor.B + Scale[0] * ForegroundColor.B), 0, $FF);
{$ENDIF}
end;

procedure BlendLine_Reference(Source, Destination: PColor32; Count: Integer);
begin
  while Count > 0 do
  begin
    BlendMem_Reference(Source^, Destination^);
    Inc(Source);
    Inc(Destination);
    Dec(Count);
  end;
end;

procedure BlendLineEx_Reference(Source, Destination: PColor32; Count: Integer; Master: Cardinal);
begin
  while Count > 0 do
  begin
    BlendMemEx_Reference(Source^, Destination^, Master);
    Inc(Source);
    Inc(Destination);
    Dec(Count);
  end;
end;

function CombineReg_Reference(ForeGround, Background: TColor32; Weight: Cardinal): TColor32;
var
  ForegroundColor : TColor32Entry absolute Foreground;
  BackgroundColor : TColor32Entry absolute Background;
{$IFDEF UseLookupTables}
  AlphaForeground : PByteArray;
  AlphaBackground : PByteArray;
{$ELSE}
{$ifndef USE_DIV255}
var
  ScaleFG: Double;
  ScaleBG: Double;
{$endif}
{$ENDIF}
begin
  if Weight = 0 then
  begin
    Result := Background;
    Exit;
  end;

  if Weight >= $FF then
  begin
    Result := ForeGround;
    Exit;
  end;

{$IFDEF UseLookupTables}
  AlphaForeground := @GDivTable[Weight];
  AlphaBackground := @GDivTable[255 - Weight];
  BackgroundColor.R := AlphaBackground[BackgroundColor.R] + AlphaForeground[ForegroundColor.R];
  BackgroundColor.G := AlphaBackground[BackgroundColor.G] + AlphaForeground[ForegroundColor.G];
  BackgroundColor.B := AlphaBackground[BackgroundColor.B] + AlphaForeground[ForegroundColor.B];
  BackgroundColor.A := AlphaBackground[BackgroundColor.A] + AlphaForeground[ForegroundColor.A];
{$ELSE}
{$ifdef USE_DIV255}
  BackgroundColor.A := Div255(SmallInt(Weight) * (ForegroundColor.A - BackgroundColor.A)) + BackgroundColor.A;
  BackgroundColor.B := Div255(SmallInt(Weight) * (ForegroundColor.B - BackgroundColor.B)) + BackgroundColor.B;
  BackgroundColor.G := Div255(SmallInt(Weight) * (ForegroundColor.G - BackgroundColor.G)) + BackgroundColor.G;
  BackgroundColor.R := Div255(SmallInt(Weight) * (ForegroundColor.R - BackgroundColor.R)) + BackgroundColor.R;
{$else USE_DIV255}
  ScaleFG := Weight * COne255th;
  ScaleBG := 1 - ScaleFG;
  BackgroundColor.R := EnsureRange(Round(ScaleBG * BackgroundColor.R + ScaleFG * ForegroundColor.R), 0, $FF);
  BackgroundColor.G := EnsureRange(Round(ScaleBG * BackgroundColor.G + ScaleFG * ForegroundColor.G), 0, $FF);
  BackgroundColor.B := EnsureRange(Round(ScaleBG * BackgroundColor.B + ScaleFG * ForegroundColor.B), 0, $FF);
  BackgroundColor.A := EnsureRange(Round(ScaleBG * BackgroundColor.A + ScaleFG * ForegroundColor.A), 0, $FF);
{$endif USE_DIV255}
{$ENDIF}

  Result := Background;
end;

procedure CombineMem_Reference(ForeGround: TColor32; var Background: TColor32; Weight: Cardinal);
var
  ForegroundColor : TColor32Entry absolute Foreground;
  BackgroundColor : TColor32Entry absolute Background;
{$IFDEF UseLookupTables}
  AlphaForeground : PByteArray;
  AlphaBackground : PByteArray;
{$ELSE}
{$ifndef USE_DIV255}
var
  ScaleFG: Double;
  ScaleBG: Double;
{$endif}
{$ENDIF}
begin
  if Weight = 0 then
    Exit;

  if Weight >= $FF then
  begin
    Background := ForeGround;
    Exit;
  end;

{$IFDEF UseLookupTables}
  AlphaForeground := @GDivTable[Weight];
  AlphaBackground := @GDivTable[255 - Weight];
  BackgroundColor.R := AlphaBackground[BackgroundColor.R] + AlphaForeground[ForegroundColor.R];
  BackgroundColor.G := AlphaBackground[BackgroundColor.G] + AlphaForeground[ForegroundColor.G];
  BackgroundColor.B := AlphaBackground[BackgroundColor.B] + AlphaForeground[ForegroundColor.B];
  BackgroundColor.A := AlphaBackground[BackgroundColor.A] + AlphaForeground[ForegroundColor.A];
{$ELSE}
{$ifdef USE_DIV255}
  BackgroundColor.A := Div255(SmallInt(Weight) * (ForegroundColor.A - BackgroundColor.A)) + BackgroundColor.A;
  BackgroundColor.B := Div255(SmallInt(Weight) * (ForegroundColor.B - BackgroundColor.B)) + BackgroundColor.B;
  BackgroundColor.G := Div255(SmallInt(Weight) * (ForegroundColor.G - BackgroundColor.G)) + BackgroundColor.G;
  BackgroundColor.R := Div255(SmallInt(Weight) * (ForegroundColor.R - BackgroundColor.R)) + BackgroundColor.R;
{$else USE_DIV255}
  ScaleFG := Weight * COne255th;
  ScaleBG := 1 - ScaleFG;
  BackgroundColor.R := EnsureRange(Round(ScaleBG * BackgroundColor.R + ScaleFG * ForegroundColor.R), 0, $FF);
  BackgroundColor.G := EnsureRange(Round(ScaleBG * BackgroundColor.G + ScaleFG * ForegroundColor.G), 0, $FF);
  BackgroundColor.B := EnsureRange(Round(ScaleBG * BackgroundColor.B + ScaleFG * ForegroundColor.B), 0, $FF);
  BackgroundColor.A := EnsureRange(Round(ScaleBG * BackgroundColor.A + ScaleFG * ForegroundColor.A), 0, $FF);
{$endif USE_DIV255}
{$ENDIF}
end;

procedure CombineLine_Reference(Source, Destination: PColor32; Count: Integer;
  Weight: Cardinal);
begin
  while Count > 0 do
  begin
    CombineMem(Source^, Destination^, Weight);
    Inc(Source);
    Inc(Destination);
    Dec(Count);
  end;
end;


function MergeReg_Reference(Foreground, Background: TColor32): TColor32;
var
  ForegroundColor  : TColor32Entry absolute Foreground;
  BackgroundColor  : TColor32Entry absolute Background;
  ResultColor      : TColor32Entry absolute Result;
{$IFDEF UseLookupTables}
  ForegroundWeight : PByteArray;
  BackgroundWeight : PByteArray;
  WeightAlpha      : Byte;
{$ELSE}
var
  Alpha : Double;
  Scale : Double;
  AlphaFG : Double;
//  AlphaBG : Double;
{$ENDIF}
begin
  if ForegroundColor.A = $FF then
    Result := Foreground
  else
  if ForegroundColor.A = $0 then
    Result := Background
  else
  if BackgroundColor.A = $0 then
    Result := Foreground
  else
  if BackgroundColor.A = $FF then
    Result := BlendReg_Reference(Foreground, Background)
  else
  begin
{$IFDEF UseLookupTables}
    ResultColor.A := GDivTable[ForegroundColor.A xor 255, BackgroundColor.A xor 255] xor 255;
    WeightAlpha := GRcTable[ResultColor.A, ForegroundColor.A];
    ForegroundWeight := @GDivTable[WeightAlpha];
    BackgroundWeight := @GDivTable[WeightAlpha xor $FF];
    ResultColor.R := ForegroundWeight[ForegroundColor.R] + BackgroundWeight[BackgroundColor.R];
    ResultColor.G := ForegroundWeight[ForegroundColor.G] + BackgroundWeight[BackgroundColor.G];
    ResultColor.B := ForegroundWeight[ForegroundColor.B] + BackgroundWeight[BackgroundColor.B];
{$ELSE}
    Alpha := 1.0 - (1.0 - ForegroundColor.A * COne255th) * (1.0 - BackgroundColor.A * COne255th);
    ResultColor.A := Round(255 * Alpha);

    if ResultColor.A = 0 then
      Scale := 0 // Even though we test for FG.A=BG.A=0 above this can occur due to rounding
    else
      Scale := 1 / Alpha;

    AlphaFG := ForegroundColor.A * COne255th;

    (*
      Given that:

        Ra = Fa + Ba * (1 - Fa)
        Rc = (Fa * (Fc - Bc * Ba) + Bc * Ba) / Ra

      ...can be rewritten as:

        Ra := 1 - (1 - Fa) * (1 - Ba);
        Wa := Fa / Ra;
        Rc := Bc + Wa * (Fc - Bc);

    *)

    (*
    This:

    AlphaBG := BackgroundColor.A * COne255th;
    ResultColor.R := Round(((AlphaFG * ForegroundColor.R) + (AlphaBG * (1 - AlphaFG) * BackgroundColor.R)) * Scale);
    ResultColor.G := Round(((AlphaFG * ForegroundColor.G) + (AlphaBG * (1 - AlphaFG) * BackgroundColor.G)) * Scale);
    ResultColor.B := Round(((AlphaFG * ForegroundColor.B) + (AlphaBG * (1 - AlphaFG) * BackgroundColor.B)) * Scale);

    becomes:
    *)

    Scale := AlphaFG * Scale;

    ResultColor.R := BackgroundColor.R + Round((ForegroundColor.R - BackgroundColor.R) * Scale);
    ResultColor.G := BackgroundColor.G + Round((ForegroundColor.G - BackgroundColor.G) * Scale);
    ResultColor.B := BackgroundColor.B + Round((ForegroundColor.B - BackgroundColor.B) * Scale);

{$ENDIF}
  end;
end;

procedure MergeMem_Reference(Foreground: TColor32; var Background: TColor32);
begin
  Background := MergeReg_Reference(Foreground, Background);
end;

procedure MergeLine_Reference(Source, Destination: PColor32; Count: Cardinal);
begin
  while Count > 0 do
  begin
    Destination^ := MergeReg_Reference(Source^, Destination^);
    Inc(Source);
    Inc(Destination);
    Dec(Count);
  end;
end;

procedure MergeLineEx_Reference(Source, Destination: PColor32; Count: Cardinal; Master: Cardinal);
begin
  while Count > 0 do
  begin
    Destination^ := MergeRegEx_Reference(Source^, Destination^, Master);
    Inc(Source);
    Inc(Destination);
    Dec(Count);
  end;
end;

function MergeRegEx_Reference(Foreground, Background: TColor32; Master: Cardinal): TColor32;
var
  TempColor       : TColor32Entry;
  ForegroundColor : TColor32Entry absolute Foreground;
  MasterAlpha     : Byte absolute Master;
begin
  TempColor.ARGB := Foreground;
  TempColor.A := Round(TempColor.A * MasterAlpha / 255);
  Result := MergeReg_Reference(TempColor.ARGB, Background);
end;

procedure MergeMemEx_Reference(Foreground: TColor32; var Background: TColor32; Master: Cardinal);
var
  TempColor       : TColor32Entry;
  ForegroundColor : TColor32Entry absolute Foreground;
  MasterAlpha     : Byte absolute Master;
begin
  TempColor.ARGB := Foreground;
  TempColor.A := Round(TempColor.A * MasterAlpha / 255);
  Background := MergeReg_Reference(TempColor.ARGB, Background);
end;


{ Global Functions }

{$IFDEF UseLookupTables}
procedure CreateTables;
var
  I, J : Integer;
begin
  for J := 0 to 255 do
    for I := 0 to 255 do
    begin
      GDivTable[I, J] := Round(I * J * COne255th);
      if I > 0 then
        GRcTable[I, J] := Round(J * 255 / I)
      else
        GRcTable[I, J] := 0;
    end;
end;

procedure FreeTables;
begin
end;
{$ENDIF}

{$IFDEF UseLookupTables}
initialization
  CreateTables;

finalization
  FreeTables;
{$ENDIF}

end.
