unit GR32_PortableNetworkGraphic.Chunks.oFFs;

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
//      TPngChunkImageOffset
//
//------------------------------------------------------------------------------
type
  TPngChunkImageOffset = class(TCustomDefinedChunkWithHeader)
  private
    FImagePositionX : Integer;
    FImagePositionY : Integer;
    FUnitSpecifier  : Byte;
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;

    property UnitSpecifier: Byte read FUnitSpecifier write FUnitSpecifier;
    property ImagePositionX: Integer read FImagePositionX write FImagePositionX;
    property ImagePositionY: Integer read FImagePositionY write FImagePositionY;
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  GR32.BigEndian;

//------------------------------------------------------------------------------
//
//      TPngChunkImageOffset
//
//------------------------------------------------------------------------------
procedure TPngChunkImageOffset.AssignTo(Dest: TPersistent);
begin
  if Dest is TPngChunkImageOffset then
    with TPngChunkImageOffset(Dest) do
    begin
      FImagePositionX := Self.FImagePositionX;
      FImagePositionY := Self.FImagePositionY;
      FUnitSpecifier  := Self.FUnitSpecifier;
    end
  else
    inherited;
end;

class function TPngChunkImageOffset.GetClassChunkName: TChunkName;
begin
  Result := 'oFFs';
end;

function TPngChunkImageOffset.GetChunkSize: Cardinal;
begin
  Result := 9;
end;

procedure TPngChunkImageOffset.ReadFromStream(Stream: TStream; ChunkSize: Cardinal);
begin
  if (Stream.Position+ChunkSize > Stream.Size) or (GetChunkSize > ChunkSize) then
    raise EPngError.Create(RCStrChunkSizeTooSmall);

  // read image positions
  FImagePositionX := BigEndian.ReadCardinal(Stream);
  FImagePositionY := BigEndian.ReadCardinal(Stream);

  // read unit specifier
  Stream.Read(FUnitSpecifier, 1);
end;

procedure TPngChunkImageOffset.WriteToStream(Stream: TStream);
begin
  // write image positions
  BigEndian.WriteCardinal(Stream, FImagePositionX);
  BigEndian.WriteCardinal(Stream, FImagePositionY);

  // write unit specifier
  Stream.Write(FUnitSpecifier, 1);
end;

initialization
  RegisterPngChunk(TPngChunkImageOffset);
end.

