unit GR32_PortableNetworkGraphic.Chunks.sCAL;

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
//      TPngChunkPhysicalScale
//
//------------------------------------------------------------------------------
type
  TPngChunkPhysicalScale = class(TCustomDefinedChunkWithHeader)
  private
    FUnitSpecifier  : Byte;
    FUnitsPerPixelX : Single;
    FUnitsPerPixelY : Single;
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;

  public
    procedure Assign(Source: TPersistent); override;

    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;

    property UnitSpecifier: Byte read FUnitSpecifier write FUnitSpecifier;
    property UnitsPerPixelX: Single read FUnitsPerPixelX write FUnitsPerPixelX;
    property UnitsPerPixelY: Single read FUnitsPerPixelY write FUnitsPerPixelY;
  end;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------
//
//      TPngChunkPhysicalScale
//
//------------------------------------------------------------------------------
class function TPngChunkPhysicalScale.GetClassChunkName: TChunkName;
begin
  Result := 'sCAL';
end;

procedure TPngChunkPhysicalScale.Assign(Source: TPersistent);
begin
  inherited;

  if (Source is TPngChunkPhysicalScale) then
  begin
    FUnitSpecifier  := TPngChunkPhysicalScale(Source).UnitSpecifier;
    FUnitsPerPixelX := TPngChunkPhysicalScale(Source).UnitsPerPixelX;
    FUnitsPerPixelY := TPngChunkPhysicalScale(Source).UnitsPerPixelY;
  end;
end;

function TPngChunkPhysicalScale.GetChunkSize: Cardinal;
begin
  Result := 4;
end;

procedure TPngChunkPhysicalScale.ReadFromStream(Stream: TStream;
  ChunkSize: Cardinal);
begin
  if (Stream.Position+ChunkSize > Stream.Size) or (GetChunkSize > ChunkSize) then
    raise EPngError.Create(RCStrChunkSizeTooSmall);

  // read unit specifier
  Stream.Read(FUnitSpecifier, 1);

  // yet todo, see http://www.libpng.org/pub/png/book/chapter11.html#png.ch11.div.9
end;

procedure TPngChunkPhysicalScale.WriteToStream(Stream: TStream);
begin
  raise EPngError.CreateFmt(RCStrChunkNotImplemented, [ChunkNameAsString]);
  // yet todo, see http://www.libpng.org/pub/png/book/chapter11.html#png.ch11.div.9
end;


initialization
//  RegisterPngChunk(TPngChunkPhysicalScale);
end.

