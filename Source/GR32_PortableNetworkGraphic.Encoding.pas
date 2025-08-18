unit GR32_PortableNetworkGraphic.Encoding;

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
 * The Original Code is GR32PNG for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Christian-W. Budde
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2009
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}
{$include GR32_PngCompilerSwitches.inc}

uses
  Classes,
  SysUtils,
  GR32_PortableNetworkGraphic.Types,
  GR32_PortableNetworkGraphic.Chunks,
  GR32_PortableNetworkGraphic.Chunks.PLTE,
  GR32_PortableNetworkGraphic.Chunks.gAMA,
  GR32_PortableNetworkGraphic.Chunks.tRNS;

//------------------------------------------------------------------------------
//
//      TCustomPngCoder
//
//------------------------------------------------------------------------------
type
  TCustomPngCoder = class abstract
  protected
    FStream       : TStream;
    FHeader       : TPngChunkImageHeader;
    FGamma        : TPngChunkGamma;
    FPalette      : TPngChunkPalette;
    FTransparency : TCustomPngTransparency;

    FRowBuffer    : array [0..1] of PByteArray;
    FAlphaTable   : PByteArray;
    FMappingTable : PByteArray;
    procedure BuildMappingTables; virtual;

    procedure EncodeFilterSub(CurrentRow, PreviousRow, OutputRow: PByteArray; BytesPerRow, PixelByteSize: Integer);
    procedure EncodeFilterUp(CurrentRow, PreviousRow, OutputRow: PByteArray; BytesPerRow, PixelByteSize: Integer);
    procedure EncodeFilterAverage(CurrentRow, PreviousRow, OutputRow: PByteArray; BytesPerRow, PixelByteSize: Integer);
    procedure EncodeFilterPaeth(CurrentRow, PreviousRow, OutputRow: PByteArray; BytesPerRow, PixelByteSize: Integer);

    procedure DecodeFilterSub(CurrentRow, PreviousRow: PByteArray; BytesPerRow, PixelByteSize: NativeInt);
    procedure DecodeFilterUp(CurrentRow, PreviousRow: PByteArray; BytesPerRow, PixelByteSize: NativeInt);
    procedure DecodeFilterAverage(CurrentRow, PreviousRow: PByteArray; BytesPerRow, PixelByteSize: NativeInt);
    procedure DecodeFilterPaeth(CurrentRow, PreviousRow: PByteArray; BytesPerRow, PixelByteSize: NativeInt);

    procedure EncodeFilterRow(CurrentRow, PreviousRow, OutputRow, TempBuffer: PByteArray; BytesPerRow, PixelByteSize: Integer); virtual; abstract;
    procedure DecodeFilterRow(FilterMethod: TAdaptiveFilterMethod; CurrentRow, PreviousRow: PByteArray; BytesPerRow, PixelByteSize: Integer); virtual; abstract;
  public
    constructor Create(Stream: TStream; Header: TPngChunkImageHeader;
      Gamma: TPngChunkGamma = nil; Palette: TPngChunkPalette = nil;
      Transparency : TCustomPngTransparency = nil); virtual;
    destructor Destroy; override;
  end;

type
  TScanLineCallback = function(Bitmap: TObject; Y: Integer): Pointer of object;


//------------------------------------------------------------------------------
//
//      TCustomPngDecoder
//
//------------------------------------------------------------------------------
type
  TCustomPngDecoder = class abstract(TCustomPngCoder)
  protected
    procedure EncodeFilterRow(CurrentRow, PreviousRow, OutputRow, TempBuffer: PByteArray; BytesPerRow, PixelByteSize: Integer); override;
    procedure DecodeFilterRow(FilterMethod: TAdaptiveFilterMethod; CurrentRow, PreviousRow: PByteArray; BytesPerRow, PixelByteSize: Integer); override;
  public
    procedure DecodeToScanline(Bitmap: TObject; ScanLineCallback: TScanLineCallback); virtual; abstract;
  end;

  TCustomPngDecoderClass = class of TCustomPngDecoder;


//------------------------------------------------------------------------------
//
//      TCustomPngEncoder
//
//------------------------------------------------------------------------------
type
  TCustomPngEncoder = class abstract(TCustomPngCoder)
  protected
    procedure EncodeFilterRow(CurrentRow, PreviousRow, OutputRow, TempBuffer: PByteArray; BytesPerRow, PixelByteSize: Integer); override;
    procedure DecodeFilterRow(FilterMethod: TAdaptiveFilterMethod; CurrentRow, PreviousRow: PByteArray; BytesPerRow, PixelByteSize: Integer); override;
  public
    procedure EncodeFromScanline(Bitmap: TObject; ScanLineCallback: TScanLineCallback); virtual; abstract;
  end;

  TCustomPngEncoderClass = class of TCustomPngEncoder;


//------------------------------------------------------------------------------
//
//      Utilities
//
//------------------------------------------------------------------------------
function CalculateRowSum(CurrentRow: PByteArray; BytesPerRow: Integer): Cardinal;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  Math;


//------------------------------------------------------------------------------
//
//      Utilities
//
//------------------------------------------------------------------------------
function CalculateRowSum(CurrentRow: PByteArray; BytesPerRow: Integer): Cardinal;
var
  Index : Integer;
begin
  Result := 0;
  for Index := 1 to BytesPerRow do
    Result := Result + Cardinal(Abs(SmallInt(CurrentRow[Index])));
end;


//------------------------------------------------------------------------------
//
//      TCustomPngCoder
//
//------------------------------------------------------------------------------
constructor TCustomPngCoder.Create(Stream: TStream;
  Header: TPngChunkImageHeader; Gamma: TPngChunkGamma = nil;
  Palette: TPngChunkPalette = nil; Transparency : TCustomPngTransparency = nil);
begin
  FStream       := Stream;
  FHeader       := Header;
  FGamma        := Gamma;
  FPalette      := Palette;
  FTransparency := Transparency;
  FMappingTable := nil;
  FAlphaTable   := nil;
  BuildMappingTables;
  inherited Create;
end;

destructor TCustomPngCoder.Destroy;
begin
  Dispose(FMappingTable);
  Dispose(FAlphaTable);
  inherited;
end;

procedure TCustomPngCoder.BuildMappingTables;
var
  Index        : Integer;
  Palette      : PRGB24Array;
  FracVal      : Single;
  Color        : TRGB24;
  MaxByte      : Byte;
  PreCalcGamma : Extended;
const
  COne255th : Extended = 1 / 255;
begin
  if FHeader.HasPalette then
  begin
    if (FPalette <> nil) then
    begin
      GetMem(FMappingTable, FPalette.Count * SizeOf(TRGB24));
      Palette := PRGB24Array(FMappingTable);

      if (FGamma <> nil) then
      begin
        PreCalcGamma := 1 / (FGamma.Gamma * 2.2E-5);
        for Index := 0 to FPalette.Count - 1 do
        begin
          Color := FPalette.PaletteEntry[Index];
          Palette[Index].R := Round(Power((Color.R * COne255th), PreCalcGamma) * 255);
          Palette[Index].G := Round(Power((Color.G * COne255th), PreCalcGamma) * 255);
          Palette[Index].B := Round(Power((Color.B * COne255th), PreCalcGamma) * 255);
        end;
      end
      else
        for Index := 0 to FPalette.Count - 1 do
          Palette[Index] := FPalette.PaletteEntry[Index];
    end
    else
    begin
      // create gray scale palette
      GetMem(FMappingTable, 256 * SizeOf(TRGB24));
      Palette := PRGB24Array(FMappingTable);
      MaxByte := ((1 shl FHeader.BitDepth) - 1) and $FF;
      FracVal := 1 / MaxByte;

      if (FGamma <> nil) then
      begin
        PreCalcGamma := 1 / (FGamma.Gamma * 2.2E-5);
        for Index := 0 to FPalette.Count - 1 do
        begin
          Palette[Index].R := Round(Power(Index * FracVal, PreCalcGamma) * 255);
          Palette[Index].G := Palette[Index].R;
          Palette[Index].B := Palette[Index].B;
        end;
      end
      else
      begin
        for Index := 0 to MaxByte do
        begin
          Palette[Index].R := Round(255 * (Index * FracVal));
          Palette[Index].G := Palette[Index].R;
          Palette[Index].B := Palette[Index].R;
        end;
      end;
    end;

   // build alpha table
   GetMem(FAlphaTable, 256);
   FillChar(FAlphaTable^, 256, $FF);

   // eventually fill alpha table
   if FTransparency is TPngTransparencyFormat3 then
     with TPngTransparencyFormat3(FTransparency) do
       for Index := 0 to Count - 1 do
         FAlphaTable[Index] := Transparency[Index];
  end
  else
  begin
    GetMem(FMappingTable, 256);
    if (FGamma <> nil) and (FGamma.Gamma <> 0) then
    begin
      PreCalcGamma := 1 / (FGamma.Gamma * 2.2E-5);
      for Index := 0 to $FF do
        FMappingTable[Index] := Round(Power((Index * COne255th), PreCalcGamma) * 255);
    end
    else
      for Index := 0 to $FF do
        FMappingTable[Index] := Index;
  end;
end;

procedure TCustomPngCoder.DecodeFilterSub(CurrentRow, PreviousRow: PByteArray;
  BytesPerRow, PixelByteSize: NativeInt);
{$IFDEF PUREPASCAL}
var
  Index : Integer;
begin
  for Index := PixelByteSize + 1 to BytesPerRow do
    CurrentRow[Index] := (CurrentRow[Index] + CurrentRow[Index - PixelByteSize]) and $FF;
{$ELSE}
asm
{$IFDEF Target_x64}
  // RCX = Self
  // RDX = CurrentRow
  // R9  = BytesPerRow
        ADD     RDX, 1
        MOV     RAX, RDX
        MOV     RCX, BytesPerRow
        ADD     RAX, PixelByteSize
        SUB     RCX, PixelByteSize
        LEA     RAX, [RAX + RCX]
        LEA     RDX, [RDX + RCX]
        NEG     RCX
        JNL     @Done

@Start:
        MOV     R8B, [RAX + RCX].Byte
        ADD     R8B, [RDX + RCX].Byte
        MOV     [RAX + RCX].Byte, R8B

        ADD     RCX, 1
        JS      @Start

@Done:
{$ENDIF}
{$IFDEF Target_x86}
        ADD     EDX, 1
        MOV     EAX, EDX
        MOV     ECX, BytesPerRow.DWORD
        ADD     EAX, PixelByteSize.DWORD
        SUB     ECX, PixelByteSize.DWORD
        LEA     EAX, [EAX + ECX]
        LEA     EDX, [EDX + ECX]
        NEG     ECX
        JNL     @Done

        PUSH    EBX

@Start:
        MOV     BL, [EAX + ECX].Byte
        ADD     BL, [EDX + ECX].Byte
        MOV     [EAX + ECX].Byte, BL

        ADD     ECX, 1
        JS      @Start

        POP     EBX

@Done:
{$ENDIF}
{$ENDIF}
end;

procedure TCustomPngCoder.DecodeFilterUp(CurrentRow, PreviousRow: PByteArray;
  BytesPerRow, PixelByteSize: NativeInt);
{$IFDEF PUREPASCAL}
var
  Index : Integer;
begin
  for Index := 1 to BytesPerRow do
    CurrentRow[Index] := (CurrentRow[Index] + PreviousRow[Index]) and $FF;
{$ELSE}
asm
{$IFDEF Target_x64}
        // RCX = Self
        // RDX = CurrentRow
        // R8  = PreviousRow
        // R9  = BytesPerRow
        MOV     RAX, RDX
        MOV     RDX, R8
        MOV     RCX, BytesPerRow
        LEA     RAX, [RAX + RCX + 1]
        LEA     RDX, [RDX + RCX + 1]
        NEG     RCX
        JNL     @Done

@Start:
        MOV     R8B, [RAX + RCX].Byte
        ADD     R8B, [RDX + RCX].Byte
        MOV     [RAX + RCX].Byte, R8B

        ADD     RCX, 1
        JS      @Start

@Done:
{$ENDIF}
{$IFDEF Target_x86}
        MOV     EAX, EDX
        MOV     EDX, ECX
        MOV     ECX, BytesPerRow.DWORD
        LEA     EAX, [EAX + ECX + 1]
        LEA     EDX, [EDX + ECX + 1]
        NEG     ECX
        JNL     @Done

        PUSH    EBX

@Start:
        MOV     BL, [EAX + ECX].Byte
        ADD     BL, [EDX + ECX].Byte
        MOV     [EAX + ECX].Byte, BL

        ADD     ECX, 1
        JS      @Start

        POP     EBX

@Done:
{$ENDIF}
{$ENDIF}
end;

procedure TCustomPngCoder.DecodeFilterAverage(CurrentRow, PreviousRow: PByteArray;
  BytesPerRow, PixelByteSize: NativeInt);
var
  Index : Integer;
begin
  for Index := 1 to PixelByteSize do
    CurrentRow[Index] := (CurrentRow[Index] + PreviousRow[Index] shr 1) and $FF;

  for Index := PixelByteSize + 1 to BytesPerRow do
    CurrentRow[Index] := (CurrentRow[Index] +
      (CurrentRow[Index - PixelByteSize] + PreviousRow[Index]) shr 1) and $FF;
end;

function PaethPredictor(a, b, c: Byte): Integer; {$IFNDEF TARGET_x64} pascal; {$ENDIF}
{$IFDEF PUREPASCAL}
var
  DistA, DistB, DistC: Integer;
begin
  DistA := Abs(b - c);
  DistB := Abs(a - c);
  DistC := Abs(a + b - c * 2);

  if (DistA <= DistB) and (DistA <= DistC) then Result := a else
  if DistB <= DistC then
    Result := b
  else
    Result := c;
{$ELSE}
asm
{$IFDEF TARGET_x64}
        // RCX = a
        // RDX = b
        // R8  = c

        // calculate DistA = Abs(b - c)
        MOVZX   RAX, DL          // RAX = b
        SUB     RAX, R8          // RAX = b - c
        MOV     R10, RAX         // R10 = b - c
        JAE     @PositiveDistA   // if  R10 >= 0 then
        NOT     RAX              //   ...
        INC     RAX              //   RAX = Abs(b - c) = DistA

        @PositiveDistA:

        // calculate DistB = Abs(a - c)
        MOVZX   R11, CL          // R11 = a
        SUB     R11, R8          // R11 = a - c
        MOV     R9, R11          // R9 = a - c
        JAE     @PositiveDistB   // if  R9 >= 0 then
        NOT     R11              //   ...
        INC     R11              //   R11 = Abs(a - c) = DistB

        @PositiveDistB:

        // calculate DistC = Abs(a + b - c * 2)
        ADD     R10, R9          // R10 = b - c + a - c = a + b - 2 * c
        JNL     @PositiveDistC   // if R10 >= 0 then
        NOT     R10              //   ...
        INC     R10              //   R10 = Abs(a + b - c * 2) = DistC

        @PositiveDistC:

        MOV     R9, RAX          // R9 = DistA
        SUB     R9, R11          // R9 = DistA - DistB
        JA      @NextCheck       // if (DistA <= DistB) then
        MOV     R9, RAX          // R9 = DistA
        SUB     R9, R10          // R9 = DistA - DistC
        JA      @NextCheck       // if (DistA <= DistC) then

        MOV     RAX, RCX         // RAX = a
        JMP     @Done            // Exit

        @NextCheck:
        MOV     R9, R11          // R9 = DistB
        SUB     R9, R10          // R9 = DistB - DistC
        JA      @ResultC         // if (DistB <= DistC) then

        MOV     RAX, RDX         // RAX = b
        JMP     @Done

        @ResultC:
        MOV     RAX, R8          // RAX = c

        @Done:
{$ELSE}
        MOVZX   EDX, c
        PUSH    EBX
        MOVZX   EAX, b
        SUB     EAX, EDX
        JAE     @PositiveDistA
        NOT     EAX
        INC     EAX

@PositiveDistA:
        MOVZX   EBX, a
        SUB     EBX, EDX
        JAE     @PositiveDistB
        NOT     EBX
        INC     EBX

@PositiveDistB:
        MOVZX   ECX, a
        SUB     ECX, EDX
        MOVZX   EDX, b
        ADD     ECX, EDX
        MOVZX   EDX, c
        SUB     ECX, EDX
        JAE     @PositiveDistC
        NOT     ECX
        INC     ECX

@PositiveDistC:
        MOV     EDX, EAX
        SUB     EDX, EBX
        JA      @NextCheck
        MOV     EDX, EAX
        SUB     EDX, ECX
        JA      @NextCheck

        MOVZX   EDX, a
        MOV     Result, EDX
        JMP     @Done

@NextCheck:
        MOV     EDX, EBX
        SUB     EDX, ECX
        JA      @ResultC

        MOVZX   EDX, b
        MOV     Result, EDX
        JMP     @Done

@ResultC:
        MOVZX   EDX, c
        MOV     Result, EDX

@Done:
        POP     EBX
{$ENDIF}
{$ENDIF}
end;

procedure TCustomPngCoder.DecodeFilterPaeth(CurrentRow, PreviousRow: PByteArray;
  BytesPerRow, PixelByteSize: NativeInt);
var
  Index : Integer;
begin
  DecodeFilterUp(CurrentRow, PreviousRow, PixelByteSize, PixelByteSize);

  for Index := PixelByteSize + 1 to BytesPerRow do
    CurrentRow[Index] := (CurrentRow[Index] +
      PaethPredictor(CurrentRow[Index - PixelByteSize], PreviousRow[Index],
      PreviousRow[Index - PixelByteSize])) and $FF;
end;

procedure TCustomPngCoder.EncodeFilterSub(CurrentRow, PreviousRow, OutputRow: PByteArray;
  BytesPerRow, PixelByteSize: Integer);
var
  Index : Integer;
begin
  // copy first pixel
  Move(CurrentRow[1], OutputRow[1], PixelByteSize);

  for Index := PixelByteSize + 1 to BytesPerRow do
    OutputRow[Index] := (CurrentRow[Index] - CurrentRow[Index - PixelByteSize]) and $FF;
end;

procedure TCustomPngCoder.EncodeFilterUp(CurrentRow, PreviousRow, OutputRow: PByteArray;
  BytesPerRow, PixelByteSize: Integer);
var
  Index : Integer;
begin
  for Index := 1 to BytesPerRow do
    OutputRow[Index] := (CurrentRow[Index] - PreviousRow[Index]) and $FF;
end;

procedure TCustomPngCoder.EncodeFilterAverage(CurrentRow, PreviousRow, OutputRow: PByteArray;
  BytesPerRow, PixelByteSize: Integer);
var
  Index : Integer;
begin
  for Index := 1 to PixelByteSize do
    OutputRow[Index] := (CurrentRow[Index] - PreviousRow[Index] shr 1) and $FF;

  for Index := PixelByteSize + 1 to BytesPerRow do
    OutputRow[Index] := (CurrentRow[Index] - (CurrentRow[Index - PixelByteSize] + PreviousRow[Index]) shr 1) and $FF;
end;

procedure TCustomPngCoder.EncodeFilterPaeth(CurrentRow, PreviousRow, OutputRow: PByteArray;
  BytesPerRow, PixelByteSize: Integer);
var
  Index : Integer;
begin
  EncodeFilterUp(CurrentRow, PreviousRow, OutputRow, PixelByteSize, PixelByteSize);

  for Index := PixelByteSize + 1 to BytesPerRow do
    OutputRow[Index] := (CurrentRow[Index] -
      PaethPredictor(CurrentRow[Index - PixelByteSize], PreviousRow[Index],
      PreviousRow[Index - PixelByteSize])) and $FF;
end;


//------------------------------------------------------------------------------
//
//      TCustomPngDecoder
//
//------------------------------------------------------------------------------
procedure TCustomPngDecoder.DecodeFilterRow(FilterMethod: TAdaptiveFilterMethod;
  CurrentRow, PreviousRow: PByteArray; BytesPerRow, PixelByteSize: Integer);
begin
  case FilterMethod of
    afmNone    : ;

    afmSub     : DecodeFilterSub(CurrentRow, PreviousRow, BytesPerRow, PixelByteSize);

    afmUp      : DecodeFilterUp(CurrentRow, PreviousRow, BytesPerRow, PixelByteSize);

    afmAverage : DecodeFilterAverage(CurrentRow, PreviousRow, BytesPerRow, PixelByteSize);

    afmPaeth   : DecodeFilterPaeth(CurrentRow, PreviousRow, BytesPerRow, PixelByteSize);
  else
    raise EPngError.Create(RCStrUnsupportedFilter);
  end;
end;

procedure TCustomPngDecoder.EncodeFilterRow(CurrentRow, PreviousRow, OutputRow,
  TempBuffer: PByteArray; BytesPerRow, PixelByteSize: Integer);
begin
  raise Exception.Create('Class is only meant for decoding');
end;


//------------------------------------------------------------------------------
//
//      TCustomPngEncoder
//
//------------------------------------------------------------------------------
procedure TCustomPngEncoder.EncodeFilterRow(CurrentRow, PreviousRow,
  OutputRow, TempBuffer: PByteArray; BytesPerRow, PixelByteSize: Integer);
var
  PixelIndex : Integer;
  CurrentSum : Cardinal;
  BestSum    : Cardinal;
begin
  BestSum := 0;
  OutputRow^[0] := 0;
  for PixelIndex := 1 to BytesPerRow do
    BestSum := BestSum + CurrentRow[PixelIndex];
  Move(CurrentRow^[1], OutputRow^[1], BytesPerRow);

  // check whether sub pre filter shall be used
  if aafmSub in FHeader.AdaptiveFilterMethods then
  begin
    // calculate sub filter
    EncodeFilterSub(CurrentRow, PreviousRow, TempBuffer, BytesPerRow, PixelByteSize);
    CurrentSum := CalculateRowSum(TempBuffer, BytesPerRow);

    // check if sub filter is the current best filter
    if CurrentSum < BestSum then
    begin
      BestSum := CurrentSum;
      Move(TempBuffer^[1], OutputRow^[1], BytesPerRow);
      OutputRow^[0] := 1;
    end;
  end;

  // check whether up pre filter shall be used
  if aafmUp in FHeader.AdaptiveFilterMethods then
  begin
    // calculate up filter
    EncodeFilterUp(CurrentRow, PreviousRow, TempBuffer, BytesPerRow, PixelByteSize);
    CurrentSum := CalculateRowSum(TempBuffer, BytesPerRow);

    // check if up filter is the current best filter
    if CurrentSum < BestSum then
    begin
      BestSum := CurrentSum;
      Move(TempBuffer^[1], OutputRow^[1], BytesPerRow);
      OutputRow^[0] := 2;
    end;
  end;

  // check whether average pre filter shall be used
  if aafmAverage in FHeader.AdaptiveFilterMethods then
  begin
    // calculate average filter
    EncodeFilterAverage(CurrentRow, PreviousRow, TempBuffer, BytesPerRow, PixelByteSize);
    CurrentSum := CalculateRowSum(TempBuffer, BytesPerRow);

    // check if average filter is the current best filter
    if CurrentSum < BestSum then
    begin
      BestSum := CurrentSum;
      Move(TempBuffer^[1], OutputRow^[1], BytesPerRow);
      OutputRow^[0] := 3;
    end;
  end;

  // check whether paeth pre filter shall be used
  if aafmPaeth in FHeader.AdaptiveFilterMethods then
  begin
    // calculate paeth filter
    EncodeFilterPaeth(CurrentRow, PreviousRow, TempBuffer, BytesPerRow, PixelByteSize);
    CurrentSum := CalculateRowSum(TempBuffer, BytesPerRow);

    // check if paeth filter is the current best filter
    if CurrentSum < BestSum then
    begin
      Move(TempBuffer^[1], OutputRow^[1], BytesPerRow);
      OutputRow^[0] := 4;
    end;
  end;
end;

procedure TCustomPngEncoder.DecodeFilterRow(FilterMethod: TAdaptiveFilterMethod;
  CurrentRow, PreviousRow: PByteArray; BytesPerRow, PixelByteSize: Integer);
begin
  raise Exception.Create('Class is only meant for encoding');
end;


end.
