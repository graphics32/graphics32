unit GR32_PortableNetworkGraphic.Chunks.IDAT;

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
//      TPngChunkImageData
//
//------------------------------------------------------------------------------
{$A4}
type
  TPngChunkImageData = class(TCustomDefinedChunkWithHeader)
  private
    FData : TMemoryStream;
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;
    function GetChunkData: pointer; override;

  public
    constructor Create(Header: TPngChunkImageHeader); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;

    property Data: TMemoryStream read FData;
  end;

  TChunkImageDataList = TCustomDefinedChunkWithHeaderList<TPngChunkImageData>;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------
//
//      TPngChunkImageData
//
//------------------------------------------------------------------------------
constructor TPngChunkImageData.Create;
begin
  inherited;
  FData := TMemoryStream.Create;
end;

destructor TPngChunkImageData.Destroy;
begin
  FData.Free;
  inherited;
end;

procedure TPngChunkImageData.Assign(Source: TPersistent);
begin
  inherited;

  if (Source is TPngChunkImageData) then
  begin
    FData.Position := 0;
    FData.CopyFrom(TPngChunkImageData(Source).Data, 0);
    FData.Position := 0;
  end;
end;

class function TPngChunkImageData.GetClassChunkName: TChunkName;
begin
  Result := 'IDAT';
end;

function TPngChunkImageData.GetChunkData: pointer;
begin
  Result := FData.Memory;
end;

function TPngChunkImageData.GetChunkSize: Cardinal;
begin
  Result := FData.Size;
end;

procedure TPngChunkImageData.ReadFromStream(Stream: TStream; ChunkSize: Cardinal);
begin
  inherited;

  FData.CopyFrom(Stream, ChunkSize);
end;

procedure TPngChunkImageData.WriteToStream(Stream: TStream);
begin
  Stream.CopyFrom(FData, 0);
end;

initialization
  RegisterPngChunk(TPngChunkImageData);
end.
