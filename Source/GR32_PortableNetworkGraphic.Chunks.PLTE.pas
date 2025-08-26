unit GR32_PortableNetworkGraphic.Chunks.PLTE;

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
  Graphics,
  SysUtils,
  GR32_PortableNetworkGraphic.Types,
  GR32_PortableNetworkGraphic.Chunks;

//------------------------------------------------------------------------------
//
//      TPngChunkPalette
//
//------------------------------------------------------------------------------
type
  TPngChunkPalette = class(TCustomDefinedChunkWithHeader)
  private
    FPaletteEntries : array of TRGB24;
    function GetPaletteEntry(Index: Cardinal): TRGB24;
    function GetCount: Cardinal;
    procedure SetCount(const Value: Cardinal);
    procedure SetPaletteEntry(Index: Cardinal; const Value: TRGB24);
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;
    function GetChunkData: pointer; override;
    procedure PaletteEntriesChanged; virtual;
  public
    procedure Assign(Source: TPersistent); override;

    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;

    property PaletteEntry[Index: Cardinal]: TRGB24 read GetPaletteEntry write SetPaletteEntry; default;
    property Count: Cardinal read GetCount write SetCount;
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------
//
//      TPngChunkPalette
//
//------------------------------------------------------------------------------
procedure TPngChunkPalette.Assign(Source: TPersistent);
begin
  inherited;

  if (Source is TPngChunkPalette) then
    FPaletteEntries := Copy(TPngChunkPalette(Source).FPaletteEntries)
end;

class function TPngChunkPalette.GetClassChunkName: TChunkName;
begin
  Result := 'PLTE';
end;

function TPngChunkPalette.GetPaletteEntry(Index: Cardinal): TRGB24;
begin
  if (Index < Count) then
    Result := FPaletteEntries[Index]
  else
    raise EPngError.Create(RCStrIndexOutOfBounds);
end;

procedure TPngChunkPalette.SetPaletteEntry(Index: Cardinal; const Value: TRGB24);
begin
  if (Index < Count) then
    FPaletteEntries[Index] := Value
  else
    raise EPngError.Create(RCStrIndexOutOfBounds);
end;

function TPngChunkPalette.GetCount: Cardinal;
begin
  Result := Length(FPaletteEntries);
end;

function TPngChunkPalette.GetChunkData: pointer;
begin
  Result := @FPaletteEntries[0];
end;

function TPngChunkPalette.GetChunkSize: Cardinal;
begin
  Result := Length(FPaletteEntries) * SizeOf(TRGB24);
end;

procedure TPngChunkPalette.ReadFromStream(Stream: TStream; ChunkSize: Cardinal);
begin
  if (ChunkSize mod SizeOf(TRGB24)) <> 0 then
    raise EPngError.Create(RCStrIncompletePalette);

  SetLength(FPaletteEntries, ChunkSize div SizeOf(TRGB24));

  Stream.Read(FPaletteEntries[0], Length(FPaletteEntries) * SizeOf(TRGB24));
end;

procedure TPngChunkPalette.WriteToStream(Stream: TStream);
begin
  Stream.Write(FPaletteEntries[0], ChunkSize);
end;

procedure TPngChunkPalette.PaletteEntriesChanged;
begin
  // nothing todo here yet
end;

procedure TPngChunkPalette.SetCount(const Value: Cardinal);
begin
  if Value > 256 then
    raise EPngError.Create(RCStrPaletteLimited);

  if Value <> Cardinal(Length(FPaletteEntries)) then
  begin
    SetLength(FPaletteEntries, Value);
    PaletteEntriesChanged;
  end;
end;

initialization
  RegisterPngChunk(TPngChunkPalette);
end.
