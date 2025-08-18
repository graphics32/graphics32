unit GR32_PortableNetworkGraphic.Chunks.sRGB;

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
//      TPngChunkStandardColorSpaceRGB
//
//------------------------------------------------------------------------------
type
  TPngChunkStandardColorSpaceRGB = class(TCustomDefinedChunkWithHeader)
  private
    FRenderingIntent : Byte;
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;

    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;

    property RenderingIntent: Byte read FRenderingIntent write FRenderingIntent;
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------
//
//      TPngChunkStandardColorSpaceRGB
//
//------------------------------------------------------------------------------
procedure TPngChunkStandardColorSpaceRGB.AssignTo(Dest: TPersistent);
begin
  if Dest is TPngChunkStandardColorSpaceRGB then
    with TPngChunkStandardColorSpaceRGB(Dest) do
    begin
      FRenderingIntent := Self.FRenderingIntent;
    end
  else
    inherited;
end;

class function TPngChunkStandardColorSpaceRGB.GetClassChunkName: TChunkName;
begin
  Result := 'sRGB';
end;

function TPngChunkStandardColorSpaceRGB.GetChunkSize: Cardinal;
begin
  Result := 1;
end;

procedure TPngChunkStandardColorSpaceRGB.ReadFromStream(Stream: TStream;
  ChunkSize: Cardinal);
begin
  if (Stream.Position+ChunkSize > Stream.Size) or (GetChunkSize > ChunkSize) then
    raise EPngError.Create(RCStrChunkSizeTooSmall);

  // read rendering intent
  Stream.Read(FRenderingIntent, SizeOf(Byte));
end;

procedure TPngChunkStandardColorSpaceRGB.WriteToStream(Stream: TStream);
begin
  // write rendering intent
  Stream.Write(FRenderingIntent, SizeOf(Byte));
end;

initialization
  RegisterPngChunk(TPngChunkStandardColorSpaceRGB);
end.
