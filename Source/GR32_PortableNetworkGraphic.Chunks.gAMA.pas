unit GR32_PortableNetworkGraphic.Chunks.gAMA;

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
//      TPngChunkGamma
//
//------------------------------------------------------------------------------
type
  TPngChunkGamma = class(TCustomDefinedChunkWithHeader)
  private
    FGamma : Cardinal;
    function GetGammaAsSingle: Single;
    procedure SetGammaAsSingle(const Value: Single);
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;

  public
    procedure Assign(Source: TPersistent); override;

    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;

    property Gamma: Cardinal read FGamma write FGamma;
    property GammaAsSingle: Single read GetGammaAsSingle write SetGammaAsSingle;
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  GR32.BigEndian;

//------------------------------------------------------------------------------
//
//      TPngChunkGamma
//
//------------------------------------------------------------------------------
procedure TPngChunkGamma.Assign(Source: TPersistent);
begin
  inherited;

  if (Source is TPngChunkGamma) then
    FGamma := TPngChunkGamma(Source).Gamma;
end;

class function TPngChunkGamma.GetClassChunkName: TChunkName;
begin
  Result := 'gAMA';
end;

function TPngChunkGamma.GetGammaAsSingle: Single;
begin
  Result := FGamma * 1E-5;
end;

procedure TPngChunkGamma.SetGammaAsSingle(const Value: Single);
begin
  FGamma := Round(Value * 1E5);
end;

function TPngChunkGamma.GetChunkSize: Cardinal;
begin
  Result := 4;
end;

procedure TPngChunkGamma.ReadFromStream(Stream: TStream; ChunkSize: Cardinal);
begin
  if (Stream.Position+ChunkSize > Stream.Size) or (GetChunkSize > ChunkSize) then
    raise EPngError.Create(RCStrChunkSizeTooSmall);

  // read gamma
  FGamma := BigEndian.ReadCardinal(Stream);
end;

procedure TPngChunkGamma.WriteToStream(Stream: TStream);
begin
  // write gamma
  BigEndian.WriteCardinal(Stream, FGamma);
end;

initialization
  RegisterPngChunk(TPngChunkGamma);
end.
