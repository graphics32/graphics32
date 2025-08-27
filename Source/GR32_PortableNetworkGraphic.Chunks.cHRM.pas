unit GR32_PortableNetworkGraphic.Chunks.cHRM;

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
//      TPngChunkPrimaryChromaticities
//
//------------------------------------------------------------------------------
type
  TPngChunkPrimaryChromaticities = class(TCustomDefinedChunkWithHeader)
  private
    FWhiteX : Integer;
    FWhiteY : Integer;
    FRedX   : Integer;
    FRedY   : Integer;
    FGreenX : Integer;
    FGreenY : Integer;
    FBlueX  : Integer;
    FBlueY  : Integer;
    function GetBlueX: Single;
    function GetBlueY: Single;
    function GetGreenX: Single;
    function GetGreenY: Single;
    function GetRedX: Single;
    function GetRedY: Single;
    function GetWhiteX: Single;
    function GetWhiteY: Single;
    procedure SetBlueX(const Value: Single);
    procedure SetBlueY(const Value: Single);
    procedure SetGreenX(const Value: Single);
    procedure SetGreenY(const Value: Single);
    procedure SetRedX(const Value: Single);
    procedure SetRedY(const Value: Single);
    procedure SetWhiteX(const Value: Single);
    procedure SetWhiteY(const Value: Single);
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;

  public
    procedure Assign(Source: TPersistent); override;

    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;

    property WhiteX: Integer read FWhiteX write FWhiteX;
    property WhiteY: Integer read FWhiteY write FWhiteY;
    property RedX: Integer read FRedX write FRedX;
    property RedY: Integer read FRedY write FRedY;
    property GreenX: Integer read FGreenX write FGreenX;
    property GreenY: Integer read FGreenY write FGreenY;
    property BlueX: Integer read FBlueX write FBlueX;
    property BlueY: Integer read FBlueY write FBlueY;

    property WhiteXAsSingle: Single read GetWhiteX write SetWhiteX;
    property WhiteYAsSingle: Single read GetWhiteY write SetWhiteY;
    property RedXAsSingle: Single read GetRedX write SetRedX;
    property RedYAsSingle: Single read GetRedY write SetRedY;
    property GreenXAsSingle: Single read GetGreenX write SetGreenX;
    property GreenYAsSingle: Single read GetGreenY write SetGreenY;
    property BlueXAsSingle: Single read GetBlueX write SetBlueX;
    property BlueYAsSingle: Single read GetBlueY write SetBlueY;
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  GR32.BigEndian;

//------------------------------------------------------------------------------
//
//      TPngChunkPrimaryChromaticities
//
//------------------------------------------------------------------------------
class function TPngChunkPrimaryChromaticities.GetClassChunkName: TChunkName;
begin
  Result := 'cHRM';
end;

procedure TPngChunkPrimaryChromaticities.Assign(Source: TPersistent);
begin
  inherited;

  if (Source is TPngChunkPrimaryChromaticities) then
  begin
    FWhiteX := TPngChunkPrimaryChromaticities(Source).WhiteX;
    FWhiteY := TPngChunkPrimaryChromaticities(Source).WhiteY;
    FRedX   := TPngChunkPrimaryChromaticities(Source).RedX;
    FRedY   := TPngChunkPrimaryChromaticities(Source).RedY;
    FGreenX := TPngChunkPrimaryChromaticities(Source).GreenX;
    FGreenY := TPngChunkPrimaryChromaticities(Source).GreenY;
    FBlueX  := TPngChunkPrimaryChromaticities(Source).BlueX;
    FBlueY  := TPngChunkPrimaryChromaticities(Source).BlueY;
  end;
end;

function TPngChunkPrimaryChromaticities.GetBlueX: Single;
begin
  Result := FBlueX * 1E-5;
end;

function TPngChunkPrimaryChromaticities.GetBlueY: Single;
begin
  Result := FBlueY * 1E-5;
end;

function TPngChunkPrimaryChromaticities.GetGreenX: Single;
begin
  Result := FGreenX * 1E-5;
end;

function TPngChunkPrimaryChromaticities.GetGreenY: Single;
begin
  Result := FGreenY * 1E-5;
end;

function TPngChunkPrimaryChromaticities.GetRedX: Single;
begin
  Result := FRedX * 1E-5;
end;

function TPngChunkPrimaryChromaticities.GetRedY: Single;
begin
  Result := FRedY * 1E-5;
end;

function TPngChunkPrimaryChromaticities.GetWhiteX: Single;
begin
  Result := FWhiteX * 1E-5;
end;

function TPngChunkPrimaryChromaticities.GetWhiteY: Single;
begin
  Result := FWhiteY * 1E-5;
end;

function TPngChunkPrimaryChromaticities.GetChunkSize: Cardinal;
begin
  Result := 32;
end;

procedure TPngChunkPrimaryChromaticities.ReadFromStream(Stream: TStream;
  ChunkSize: Cardinal);
begin
  if (Stream.Position+ChunkSize > Stream.Size) or (GetChunkSize > ChunkSize) then
    raise EPngError.Create(RCStrChunkSizeTooSmall);

  // read white point x
  FWhiteX := BigEndian.ReadCardinal(Stream);

  // read white point y
  FWhiteY := BigEndian.ReadCardinal(Stream);

  // read red x
  FRedX := BigEndian.ReadCardinal(Stream);

  // read red y
  FRedY := BigEndian.ReadCardinal(Stream);

  // read green x
  FGreenX := BigEndian.ReadCardinal(Stream);

  // read green y
  FGreenY := BigEndian.ReadCardinal(Stream);

  // read blue x
  FBlueX := BigEndian.ReadCardinal(Stream);

  // read blue y
  FBlueY := BigEndian.ReadCardinal(Stream);
end;

procedure TPngChunkPrimaryChromaticities.WriteToStream(Stream: TStream);
begin
  // write white point x
  BigEndian.WriteCardinal(Stream, FWhiteX);

  // write white point y
  BigEndian.WriteCardinal(Stream, FWhiteY);

  // write red x
  BigEndian.WriteCardinal(Stream, FRedX);

  // write red y
  BigEndian.WriteCardinal(Stream, FRedY);

  // write green x
  BigEndian.WriteCardinal(Stream, FGreenX);

  // write green y
  BigEndian.WriteCardinal(Stream, FGreenY);

  // write blue x
  BigEndian.WriteCardinal(Stream, FBlueX);

  // write blue y
  BigEndian.WriteCardinal(Stream, FBlueY);
end;

procedure TPngChunkPrimaryChromaticities.SetBlueX(const Value: Single);
begin
  FBlueX := Round(Value * 1E5);
end;

procedure TPngChunkPrimaryChromaticities.SetBlueY(const Value: Single);
begin
  FBlueY := Round(Value * 1E5);
end;

procedure TPngChunkPrimaryChromaticities.SetGreenX(const Value: Single);
begin
  FGreenX := Round(Value * 1E5);
end;

procedure TPngChunkPrimaryChromaticities.SetGreenY(const Value: Single);
begin
  FGreenY := Round(Value * 1E5);
end;

procedure TPngChunkPrimaryChromaticities.SetRedX(const Value: Single);
begin
  FRedX := Round(Value * 1E5);
end;

procedure TPngChunkPrimaryChromaticities.SetRedY(const Value: Single);
begin
  FRedY := Round(Value * 1E5);
end;

procedure TPngChunkPrimaryChromaticities.SetWhiteX(const Value: Single);
begin
  FWhiteX := Round(Value * 1E5);
end;

procedure TPngChunkPrimaryChromaticities.SetWhiteY(const Value: Single);
begin
  FWhiteY := Round(Value * 1E5);
end;

initialization
  RegisterPngChunk(TPngChunkPrimaryChromaticities);
end.
