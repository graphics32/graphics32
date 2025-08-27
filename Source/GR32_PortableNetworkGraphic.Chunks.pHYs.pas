unit GR32_PortableNetworkGraphic.Chunks.pHYs;

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
  GR32_PortableNetworkGraphic.Types,
  GR32_PortableNetworkGraphic.Chunks;

//------------------------------------------------------------------------------
//
//      TPngChunkPhysicalPixelDimensions
//
//------------------------------------------------------------------------------
type
  TPngChunkPhysicalPixelDimensions = class(TCustomDefinedChunkWithHeader)
  private
    FPixelsPerUnitX : Cardinal;
    FPixelsPerUnitY : Cardinal;
    FUnit           : Byte;
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;

  public
    procedure Assign(Source: TPersistent); override;

    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;

    property PixelsPerUnitX: Cardinal read FPixelsPerUnitX write FPixelsPerUnitX;
    property PixelsPerUnitY: Cardinal read FPixelsPerUnitY write FPixelsPerUnitY;
    property PixelUnit: Byte read FUnit write FUnit;
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  GR32.BigEndian;

//------------------------------------------------------------------------------
//
//      TPngChunkPhysicalPixelDimensions
//
//------------------------------------------------------------------------------
procedure TPngChunkPhysicalPixelDimensions.Assign(Source: TPersistent);
begin
  inherited;

  if (Source is TPngChunkPhysicalPixelDimensions) then
  begin
    FPixelsPerUnitX := TPngChunkPhysicalPixelDimensions(Source).PixelsPerUnitX;
    FPixelsPerUnitY := TPngChunkPhysicalPixelDimensions(Source).PixelsPerUnitY;
    FUnit := TPngChunkPhysicalPixelDimensions(Source).PixelUnit;
  end;
end;

class function TPngChunkPhysicalPixelDimensions.GetClassChunkName: TChunkName;
begin
  Result := 'pHYs';
end;

function TPngChunkPhysicalPixelDimensions.GetChunkSize: Cardinal;
begin
  Result := 9;
end;

procedure TPngChunkPhysicalPixelDimensions.ReadFromStream(Stream: TStream;
  ChunkSize: Cardinal);
begin
  if (Stream.Position+ChunkSize > Stream.Size) or (GetChunkSize > ChunkSize) then
    raise EPngError.Create(RCStrChunkSizeTooSmall);

  // read pixels per unit, X axis
  FPixelsPerUnitX := BigEndian.ReadCardinal(Stream);

  // read pixels per unit, Y axis
  FPixelsPerUnitY := BigEndian.ReadCardinal(Stream);

  // read unit
  Stream.Read(FUnit, 1);
end;

procedure TPngChunkPhysicalPixelDimensions.WriteToStream(Stream: TStream);
begin
  // write pixels per unit, X axis
  BigEndian.WriteCardinal(Stream, FPixelsPerUnitX);

  // write pixels per unit, Y axis
  BigEndian.WriteCardinal(Stream, FPixelsPerUnitY);

  // write unit
  Stream.Write(FUnit, 1);
end;


initialization
  RegisterPngChunk(TPngChunkPhysicalPixelDimensions);
end.
