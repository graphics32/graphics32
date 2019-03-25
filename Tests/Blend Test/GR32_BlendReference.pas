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
{-$DEFINE UseLookupTables}

uses
  SysUtils, GR32, GR32_Bindings, GR32_Blend;

function BlendReg_Reference(Foreground, Background: TColor32): TColor32;
procedure BlendMem_Reference(Foreground: TColor32; var Background: TColor32);
function BlendRegEx_Reference(Foreground, Background, Master: TColor32): TColor32;
procedure BlendMemEx_Reference(Foreground: TColor32; var Background: TColor32; Master: TColor32);
procedure BlendLine_Reference(Source, Destination: PColor32; Count: Integer);
procedure BlendLineEx_Reference(Source, Destination: PColor32; Count: Integer; Master: TColor32);
function CombineReg_Reference(ForeGround, Background: TColor32; Weight: Cardinal): TColor32;
procedure CombineMem_Reference(ForeGround: TColor32; var Background: TColor32; Weight: Cardinal);
procedure CombineLine_Reference(Source, Destination: PColor32; Count: Integer; Weight: Cardinal);
function MergeReg_Reference(Foreground, Background: TColor32): TColor32;
procedure MergeMem_Reference(Foreground: TColor32; var Background: TColor32);
function MergeRegEx_Reference(Foreground, Background, Master: TColor32): TColor32;
procedure MergeMemEx_Reference(Foreground: TColor32; var Background: TColor32; Master: TColor32);
procedure MergeLine_Reference(Source, Destination: PColor32; Count: Cardinal);
procedure MergeLineEx_Reference(Source, Destination: PColor32; Count: Cardinal; Master: TColor32);

implementation

uses
  Math;

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
var
  Scale : array [0..1] of Double;
{$ENDIF}
begin
  if ForegroundColor.A =   0 then
  begin
    Result := Background;
    Exit;
  end;

  if ForegroundColor.A = $FF then
  begin
    Result := Foreground;
    Exit;
  end;

  with BackgroundColor do begin
    {$IFDEF UseLookupTables}
    AlphaForeground := @GDivTable[ForegroundColor.A];
    AlphaBackground := @GDivTable[not ForegroundColor.A];
    A := AlphaForeground[ForegroundColor.A] + AlphaBackground[A];
    R := AlphaForeground[ForegroundColor.R] + AlphaBackground[R];
    G := AlphaForeground[ForegroundColor.G] + AlphaBackground[G];
    B := AlphaForeground[ForegroundColor.B] + AlphaBackground[B];
    {$ELSE}
    Scale[0] := ForegroundColor.A * COne255th;
    Scale[1] := 1 - Scale[0];
    A := EnsureRange(Round(Scale[1] * A + Scale[0] * ForegroundColor.A), 0, $FF);
    R := EnsureRange(Round(Scale[1] * R + Scale[0] * ForegroundColor.R), 0, $FF);
    G := EnsureRange(Round(Scale[1] * G + Scale[0] * ForegroundColor.G), 0, $FF);
    B := EnsureRange(Round(Scale[1] * B + Scale[0] * ForegroundColor.B), 0, $FF);
    {$ENDIF}
  end;
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
var
  Scale : array [0..1] of Double;
{$ENDIF}
begin
  if ForegroundColor.A = 0 then Exit;

  if ForegroundColor.A = $FF then
  begin
    Background := Foreground;
    Exit;
  end;

  with BackgroundColor do begin
    {$IFDEF UseLookupTables}
    AlphaForeground := @GDivTable[Foreground.A];
    AlphaBackground := @GDivTable[not Foreground.A];
    A := AlphaForeground[Foreground.A] + AlphaBackground[A];
    R := AlphaForeground[Foreground.R] + AlphaBackground[R];
    G := AlphaForeground[Foreground.G] + AlphaBackground[G];
    B := AlphaForeground[Foreground.B] + AlphaBackground[B];
    {$ELSE}
    Scale[0] := ForegroundColor.A * COne255th;
    Scale[1] := 1 - Scale[0];
    A := EnsureRange(Round(Scale[1] * A + Scale[0] * ForegroundColor.A), 0, $FF);
    R := EnsureRange(Round(Scale[1] * R + Scale[0] * ForegroundColor.R), 0, $FF);
    G := EnsureRange(Round(Scale[1] * G + Scale[0] * ForegroundColor.G), 0, $FF);
    B := EnsureRange(Round(Scale[1] * B + Scale[0] * ForegroundColor.B), 0, $FF);
    {$ENDIF}
  end;
end;

function BlendRegEx_Reference(Foreground, Background, Master: TColor32): TColor32;
var
  ForegroundColor : TColor32Entry absolute Foreground;
  BackgroundColor : TColor32Entry absolute Background;
  MasterAlpha     : Byte absolute Master;
{$IFDEF UseLookupTables}
  AlphaForeground : PByteArray;
  AlphaBackground : PByteArray;
{$ELSE}
var
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

  with BackgroundColor do begin
    {$IFDEF UseLookupTables}
    AlphaForeground := @GDivTable[Master.A];
    AlphaBackground := @GDivTable[not AlphaForeground.A];
    AlphaForeground := @GDivTable[AlphaForeground.A];
    A := AlphaForeground[ForegroundColor.A] + AlphaBackground[A];
    R := AlphaForeground[ForegroundColor.R] + AlphaBackground[R];
    G := AlphaForeground[ForegroundColor.G] + AlphaBackground[G];
    B := AlphaForeground[ForegroundColor.B] + AlphaBackground[B];
    {$ELSE}
    Scale[0] := MasterAlpha * ForegroundColor.A * Sqr(COne255th);
    Scale[1] := 1 - Scale[0];
    A := EnsureRange(Round(Scale[1] * A + Scale[0] * ForegroundColor.A), 0, $FF);
    R := EnsureRange(Round(Scale[1] * R + Scale[0] * ForegroundColor.R), 0, $FF);
    G := EnsureRange(Round(Scale[1] * G + Scale[0] * ForegroundColor.G), 0, $FF);
    B := EnsureRange(Round(Scale[1] * B + Scale[0] * ForegroundColor.B), 0, $FF);
    {$ENDIF}
  end;
  Result := Background;
end;

procedure BlendMemEx_Reference(Foreground: TColor32; var Background: TColor32;
  Master: TColor32);
var
  ForegroundColor : TColor32Entry absolute Foreground;
  BackgroundColor : TColor32Entry absolute Background;
  MasterAlpha     : Byte absolute Master;
{$IFDEF UseLookupTables}
  AlphaForeground : PByteArray;
  AlphaBackground : PByteArray;
{$ELSE}
var
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

  with BackgroundColor do begin
    {$IFDEF UseLookupTables}
    AlphaForeground := @GDivTable[Master.A];
    AlphaBackground := @GDivTable[not AlphaForeground.A];
    AlphaForeground := @GDivTable[AlphaForeground.A];
    A := AlphaForeground[ForegroundColor.A] + AlphaBackground[A];
    R := AlphaForeground[ForegroundColor.R] + AlphaBackground[R];
    G := AlphaForeground[ForegroundColor.G] + AlphaBackground[G];
    B := AlphaForeground[ForegroundColor.B] + AlphaBackground[B];
    {$ELSE}
    Scale[0] := MasterAlpha * ForegroundColor.A * Sqr(COne255th);
    Scale[1] := 1 - Scale[0];
    A := EnsureRange(Round(Scale[1] * A + Scale[0] * ForegroundColor.A), 0, $FF);
    R := EnsureRange(Round(Scale[1] * R + Scale[0] * ForegroundColor.R), 0, $FF);
    G := EnsureRange(Round(Scale[1] * G + Scale[0] * ForegroundColor.G), 0, $FF);
    B := EnsureRange(Round(Scale[1] * B + Scale[0] * ForegroundColor.B), 0, $FF);
    {$ENDIF}
  end;
end;

procedure BlendLine_Reference(Source, Destination: PColor32; Count: Integer);
begin
  while Count > 0 do begin
    BlendMem_Reference(Source^, Destination^);
    Inc(Source);
    Inc(Destination);
    Dec(Count);
  end;
end;

procedure BlendLineEx_Reference(Source, Destination: PColor32; Count: Integer; Master: TColor32);
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
var
  Scale : array [0..1] of Double;
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

  with ForegroundColor do begin
    {$IFDEF UseLookupTables}
    AlphaForeground := @GDivTable[Weight];
    AlphaBackground := @GDivTable[255 - Weight];
    R := AlphaBackground[Background.R] + AlphaForeground[R];
    G := AlphaBackground[Background.G] + AlphaForeground[G];
    B := AlphaBackground[Background.B] + AlphaForeground[B];
    A := AlphaBackground[Background.A] + AlphaForeground[A];
    {$ELSE}
    Scale[0] := Weight * COne255th;
    Scale[1] := 1 - Scale[0];
    R := EnsureRange(Round(Scale[1] * BackgroundColor.R + Scale[0] * R), 0, $FF);
    G := EnsureRange(Round(Scale[1] * BackgroundColor.G + Scale[0] * G), 0, $FF);
    B := EnsureRange(Round(Scale[1] * BackgroundColor.B + Scale[0] * B), 0, $FF);
    A := EnsureRange(Round(Scale[1] * BackgroundColor.A + Scale[0] * A), 0, $FF);
    {$ENDIF}
  end;
  Result := ForeGround;
end;

procedure CombineMem_Reference(ForeGround: TColor32; var Background: TColor32; Weight: Cardinal);
var
  ForegroundColor : TColor32Entry absolute Foreground;
  BackgroundColor : TColor32Entry absolute Background;
{$IFDEF UseLookupTables}
  AlphaForeground : PByteArray;
  AlphaBackground : PByteArray;
{$ELSE}
var
  Scale : array [0..1] of Double;
{$ENDIF}
begin
  if Weight = 0 then Exit;

  if Weight >= $FF then
  begin
    Background := ForeGround;
    Exit;
  end;

  with ForegroundColor do begin
    {$IFDEF UseLookupTables}
    AlphaForeground := @GDivTable[Weight];
    AlphaBackground := @GDivTable[255 - Weight];
    R := AlphaBackground[Background.R] + AlphaForeground[R];
    G := AlphaBackground[Background.G] + AlphaForeground[G];
    B := AlphaBackground[Background.B] + AlphaForeground[B];
    A := AlphaBackground[Background.A] + AlphaForeground[A];
    {$ELSE}
    Scale[0] := Weight * COne255th;
    Scale[1] := 1 - Scale[0];
    R := EnsureRange(Round(Scale[1] * BackgroundColor.R + Scale[0] * R), 0, $FF);
    G := EnsureRange(Round(Scale[1] * BackgroundColor.G + Scale[0] * G), 0, $FF);
    B := EnsureRange(Round(Scale[1] * BackgroundColor.B + Scale[0] * B), 0, $FF);
    A := EnsureRange(Round(Scale[1] * BackgroundColor.A + Scale[0] * A), 0, $FF);
    {$ENDIF}
  end;
  Background := ForeGround;
end;

procedure CombineLine_Reference(Source, Destination: PColor32; Count: Integer;
  Weight: Cardinal);
begin
  while Count > 0 do begin
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
  Temp  : Double;
  Scale : Double;
{$ENDIF}
begin
  if ForegroundColor.A = $FF then Result := Foreground else
  if ForegroundColor.A = $0  then Result := Background else
  if BackgroundColor.A = $0  then Result := Foreground else
  if BackgroundColor.A = $FF then
    Result := BlendReg_Reference(Foreground, Background)
  else
    with BackgroundColor do
    begin
      {$IFDEF UseLookupTables}
      Result.A := GDivTable[Foreground.A xor 255, Background.A xor 255] xor 255;
      WeightAlpha := GRcTable[Result.A, Foreground.A];
      ForegroundWeight := @GDivTable[WeightAlpha];
      BackgroundWeight := @GDivTable[WeightAlpha xor $FF];
      Result.R := ForegroundWeight[ForegroundColor.R] + BackgroundWeight[R];
      Result.G := ForegroundWeight[ForegroundColor.G] + BackgroundWeight[G];
      Result.B := ForegroundWeight[ForegroundColor.B] + BackgroundWeight[B];
      {$ELSE}
      Temp := $FF - ($FF - ForegroundColor.A) * (1 - A * COne255th);
      ResultColor.A := Round(Temp);
      if ForegroundColor.A = 0 then
        Scale := 0
      else
        Scale := ForegroundColor.A / Temp;

      ResultColor.R := Round(R + Scale * (ForegroundColor.R - R));
      ResultColor.G := Round(G + Scale * (ForegroundColor.G - G));
      ResultColor.B := Round(B + Scale * (ForegroundColor.B - B));
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

procedure MergeLineEx_Reference(Source, Destination: PColor32; Count: Cardinal;
  Master: TColor32);
begin
  while Count > 0 do
  begin
    Destination^ := MergeRegEx_Reference(Source^, Destination^, Master);
    Inc(Source);
    Inc(Destination);
    Dec(Count);
  end;
end;

function MergeRegEx_Reference(Foreground, Background, Master: TColor32): TColor32;
var
  TempColor       : TColor32Entry;
  ForegroundColor : TColor32Entry absolute Foreground;
  MasterAlpha     : Byte absolute Master;
begin
  TempColor.ARGB := Foreground;
  TempColor.A := (TempColor.A * MasterAlpha) div 255;
  Result := MergeReg_Reference(TempColor.ARGB, Background);
end;

procedure MergeMemEx_Reference(Foreground: TColor32; var Background: TColor32;
  Master: TColor32);
var
  TempColor       : TColor32Entry;
  ForegroundColor : TColor32Entry absolute Foreground;
  MasterAlpha     : Byte absolute Master;
begin
  TempColor.ARGB := Foreground;
  TempColor.A := (TempColor.A * MasterAlpha) div 255;
  Background := MergeReg_Reference(TempColor.ARGB, Background);
end;


{ Global Functions }

{$IFDEF UseLookupTables}
procedure CreateTables;
var
  I, J : Integer;
begin
  for J := 0 to 255 do
    for I := 0 to 255 do begin
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
