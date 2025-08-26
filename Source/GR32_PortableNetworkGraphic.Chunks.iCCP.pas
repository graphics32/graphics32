unit GR32_PortableNetworkGraphic.Chunks.iCCP;

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
//      TPngChunkEmbeddedIccProfile
//
//------------------------------------------------------------------------------
type
  TPngChunkEmbeddedIccProfile = class(TCustomDefinedChunkWithHeader)
  private
    FProfileName       : AnsiString;
    FCompressionMethod : Byte;
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;

  public
    procedure Assign(Source: TPersistent); override;

    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;

    property ProfileName: AnsiString read FProfileName write FProfileName;
    property CompressionMethod: Byte read FCompressionMethod write FCompressionMethod;
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------
//
//      TPngChunkEmbeddedIccProfile
//
//------------------------------------------------------------------------------
procedure TPngChunkEmbeddedIccProfile.Assign(Source: TPersistent);
begin
  inherited;

  if (Source is TPngChunkEmbeddedIccProfile) then
  begin
    FProfileName       := TPngChunkEmbeddedIccProfile(Source).ProfileName;
    FCompressionMethod := TPngChunkEmbeddedIccProfile(Source).CompressionMethod;
  end;
end;

class function TPngChunkEmbeddedIccProfile.GetClassChunkName: TChunkName;
begin
  Result := 'iCCP';
end;

function TPngChunkEmbeddedIccProfile.GetChunkSize: Cardinal;
begin
  Result := Length(FProfileName) + 2;
end;

procedure TPngChunkEmbeddedIccProfile.ReadFromStream(Stream: TStream;
  ChunkSize: Cardinal);
var
  Index : Integer;
begin
  // read keyword
  Index := 1;
  SetLength(FProfileName, 80);
  while (Stream.Position < Stream.Size) do
  begin
    Stream.Read(FProfileName[Index], SizeOf(Byte));
    if FProfileName[Index] = #0 then
    begin
      SetLength(FProfileName, Index - 1);
      Break;
    end;
    Inc(Index);
  end;

  // read compression method
  Stream.Read(FCompressionMethod, 1);

  // not yet completed
end;

procedure TPngChunkEmbeddedIccProfile.WriteToStream(Stream: TStream);
var
  Temp  : Byte;
begin
  // write keyword
  Stream.Write(FProfileName[1], Length(FProfileName));

  // write separator
  Temp := 0;
  Stream.Write(Temp, 1);

  // write compression method
  Stream.Write(FCompressionMethod, 1);
end;

initialization
  RegisterPngChunk(TPngChunkEmbeddedIccProfile);
end.
