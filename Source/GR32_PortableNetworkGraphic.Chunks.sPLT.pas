unit GR32_PortableNetworkGraphic.Chunks.sPLT;

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
  Generics.Collections,
  Classes,
  GR32_PortableNetworkGraphic.Types,
  GR32_PortableNetworkGraphic.Chunks;

//------------------------------------------------------------------------------
//
//      TPngChunkSuggestedPalette
//
//------------------------------------------------------------------------------
type
  TSuggestedPalette8ByteEntry = record
    Red       : Byte;
    Green     : Byte;
    Blue      : Byte;
    Alpha     : Byte;
    Frequency : Word;
  end;
  PSuggestedPalette8ByteEntry = ^TSuggestedPalette8ByteEntry;
  TSuggestedPalette8ByteArray = array [0..0] of TSuggestedPalette8ByteEntry;
  PSuggestedPalette8ByteArray = ^TSuggestedPalette8ByteArray;

  TSuggestedPalette16ByteEntry = record
    Red       : Word;
    Green     : Word;
    Blue      : Word;
    Alpha     : Word;
    Frequency : Word;
  end;
  PSuggestedPalette16ByteEntry = ^TSuggestedPalette16ByteEntry;
  TSuggestedPalette16ByteArray = array [0..0] of TSuggestedPalette16ByteEntry;
  PSuggestedPalette16ByteArray = ^TSuggestedPalette16ByteArray;

{$ifdef PNG_CHUNK_SUGGESTED_PALETTE}
  TPngChunkSuggestedPalette = class(TCustomDefinedChunkWithHeader)
  private
    FPaletteName : AnsiString;
    FData        : Pointer;
    FCount       : Cardinal;
    FSampleDepth : Byte;
    function GetCount: Cardinal;
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;
  public
    constructor Create(Header: TPngChunkImageHeader); override;

    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;

    property Count: Cardinal read GetCount;
  end;
{$endif PNG_CHUNK_SUGGESTED_PALETTE}

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------
//
//      TPngChunkSuggestedPalette
//
//------------------------------------------------------------------------------
{$ifdef PNG_CHUNK_SUGGESTED_PALETTE}
constructor TPngChunkSuggestedPalette.Create(Header: TPngChunkImageHeader);
begin
  inherited;
  FData := nil;
  FCount := 0;
end;

class function TPngChunkSuggestedPalette.GetClassChunkName: TChunkName;
begin
  Result := 'sPLT';
end;

function TPngChunkSuggestedPalette.GetCount: Cardinal;
begin
  Result := FCount;
end;

function TPngChunkSuggestedPalette.GetChunkSize: Cardinal;
begin
  Result := Cardinal(Length(FPaletteName)) + 2 +
    (4 * (FSampleDepth shr 3) + 2) * Count;
end;

procedure TPngChunkSuggestedPalette.ReadFromStream(Stream: TStream;
  ChunkSize: Cardinal);
var
  Index      : Integer;
  DataSize   : Integer;
begin
  if (Stream.Position+ChunkSize > Stream.Size) or (GetChunkSize > ChunkSize) then
    raise EPngError.Create(RCStrChunkSizeTooSmall);

  // read palette name
  Index := 1;
  SetLength(FPaletteName, 80);
  while (Stream.Position < ChunkSize) do
  begin
    Stream.Read(FPaletteName[Index], SizeOf(Byte));
    if FPaletteName[Index] = #0 then
    begin
      SetLength(FPaletteName, Index - 1);
      Break;
    end;
    Inc(Index);
  end;

  // read sample depth
  Stream.Read(FSampleDepth, 1);

  DataSize := Integer(ChunkSize) - Length(FPaletteName) - 2;
  Assert(DataSize >= 0);
  Assert(DataSize mod 2 = 0);
  Assert(DataSize mod (4 * (FSampleDepth shr 3) + 2) = 0);
  FCount := DataSize div (4 * (FSampleDepth shr 3) + 2);
  ReallocMem(FData, DataSize);

  if FSampleDepth = 8 then
    for Index := 0 to FCount - 1 do
      with PSuggestedPalette8ByteArray(FData)^[Index] do
      begin
        Stream.Read(Red, 1);
        Stream.Read(Green, 1);
        Stream.Read(Blue, 1);
        Stream.Read(Alpha, 1);
        Frequency := BigEndian.ReadWord(Stream);
      end
  else if FSampleDepth = 16 then
    for Index := 0 to FCount - 1 do
      with PSuggestedPalette16ByteArray(FData)^[Index] do
      begin
        Red := BigEndian.ReadWord(Stream);
        Green := BigEndian.ReadWord(Stream);
        Blue := BigEndian.ReadWord(Stream);
        Alpha := BigEndian.ReadWord(Stream);
        Frequency := BigEndian.ReadWord(Stream);
      end;
end;

procedure TPngChunkSuggestedPalette.WriteToStream(Stream: TStream);
begin
  // TODO
  raise EPngError.CreateFmt(RCStrChunkNotImplemented, [ChunkNameAsString]);
end;
{$endif PNG_CHUNK_SUGGESTED_PALETTE}


initialization
{$ifdef PNG_CHUNK_SUGGESTED_PALETTE}
  RegisterPngChunk(TPngChunkSuggestedPalette);
{$endif PNG_CHUNK_SUGGESTED_PALETTE}
end.
