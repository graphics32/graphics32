unit GR32_PortableNetworkGraphic.Chunks.pCAL;

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
//      TPngChunkPixelCalibrator
//
//------------------------------------------------------------------------------
type
  TPngChunkPixelCalibrator = class(TCustomDefinedChunkWithHeader)
  private
    FCalibratorName : AnsiString;
    FOriginalZeroes : array [0..1] of Integer;
    FEquationType   : Byte;
    FNumberOfParams : Byte;
    FUnitName       : AnsiString;
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;

  public
    procedure Assign(Source: TPersistent); override;

    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;

    property CalibratorName: AnsiString read FCalibratorName write FCalibratorName;
    property OriginalZeroMin: Integer read FOriginalZeroes[0] write FOriginalZeroes[0];
    property OriginalZeroMax: Integer read FOriginalZeroes[1] write FOriginalZeroes[1];
    property EquationType: Byte read FEquationType write FEquationType;
    property NumberOfParams: Byte read FNumberOfParams write FNumberOfParams;
    property UnitName: AnsiString read FUnitName write FUnitName;
  end;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  GR32.BigEndian;

//------------------------------------------------------------------------------
//
//      TPngChunkPixelCalibrator
//
//------------------------------------------------------------------------------
procedure TPngChunkPixelCalibrator.Assign(Source: TPersistent);
begin
  inherited;

  if (Source is TPngChunkPixelCalibrator) then
  begin
    FCalibratorName    := TPngChunkPixelCalibrator(Source).CalibratorName;
    FOriginalZeroes[0] := TPngChunkPixelCalibrator(Source).OriginalZeroMin;
    FOriginalZeroes[1] := TPngChunkPixelCalibrator(Source).OriginalZeroMax;
    FEquationType      := TPngChunkPixelCalibrator(Source).EquationType;
    FNumberOfParams    := TPngChunkPixelCalibrator(Source).NumberOfParams;
    FUnitName          := TPngChunkPixelCalibrator(Source).UnitName;
  end;
end;

class function TPngChunkPixelCalibrator.GetClassChunkName: TChunkName;
begin
  Result := 'pCAL';
end;

function TPngChunkPixelCalibrator.GetChunkSize: Cardinal;
begin
  Result := 9;
end;

procedure TPngChunkPixelCalibrator.ReadFromStream(Stream: TStream;
  ChunkSize: Cardinal);
var
  Index      : Integer;
  ParamIndex : Integer;
begin
  // read keyword
  Index := 1;
  SetLength(FCalibratorName, 80);
  while (Stream.Position < Stream.Size) do
  begin
    Stream.Read(FCalibratorName[Index], SizeOf(Byte));
    if FCalibratorName[Index] = #0 then
    begin
      SetLength(FCalibratorName, Index - 1);
      Break;
    end;
    Inc(Index);
  end;

  // read original zeros
  FOriginalZeroes[0] := BigEndian.ReadCardinal(Stream);
  FOriginalZeroes[1] := BigEndian.ReadCardinal(Stream);

  // read equation type
  Stream.Read(FEquationType, 1);

  // read number of parameters
  Stream.Read(FNumberOfParams, 1);

  // read keyword
  Index := 1;
  SetLength(FUnitName, 80);
  while (Stream.Position < Stream.Size) do
  begin
    Stream.Read(FUnitName[Index], SizeOf(Byte));
    if FUnitName[Index] = #0 then
    begin
      SetLength(FUnitName, Index - 1);
      Break;
    end;
    Inc(Index);
  end;

  for ParamIndex := 0 to FNumberOfParams - 2 do
  begin
    // yet todo
  end;
end;

procedure TPngChunkPixelCalibrator.WriteToStream(Stream: TStream);
begin
  inherited;

end;



initialization
//  RegisterPngChunk(TPngChunkPixelCalibrator);
end.

