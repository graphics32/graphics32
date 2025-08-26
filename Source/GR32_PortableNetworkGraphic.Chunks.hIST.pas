unit GR32_PortableNetworkGraphic.Chunks.hIST;

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
//      TPngChunkImageHistogram
//
//------------------------------------------------------------------------------
type
  TPngChunkImageHistogram = class(TCustomDefinedChunkWithHeader)
  private
    FHistogram : array of Word;
    function GetCount: Cardinal;
    function GetFrequency(Index: Cardinal): Word;
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;
  public
    // TODO : procedure Assign(Source: TPersistent); override;

    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;

    property Count: Cardinal read GetCount;
    property Frequency[Index: Cardinal]: Word read GetFrequency;
  end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  SysUtils,
  GR32.BigEndian;

//------------------------------------------------------------------------------
//
//      TPngChunkImageHistogram
//
//------------------------------------------------------------------------------
class function TPngChunkImageHistogram.GetClassChunkName: TChunkName;
begin
  Result := 'hIST';
end;

function TPngChunkImageHistogram.GetCount: Cardinal;
begin
  Result := Length(FHistogram);
end;

function TPngChunkImageHistogram.GetFrequency(Index: Cardinal): Word;
begin
  if Index < Count then
    Result := FHistogram[Index]
  else
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TPngChunkImageHistogram.GetChunkSize: Cardinal;
begin
  Result := Count * SizeOf(Word);
end;

procedure TPngChunkImageHistogram.ReadFromStream(Stream: TStream; ChunkSize: Cardinal);
var
  Index : Integer;
begin
  // check size
  if (Stream.Position+ChunkSize > Stream.Size) or (GetChunkSize > ChunkSize) then
    raise EPngError.Create(RCStrChunkSizeTooSmall);

  // adjust histogram array size
  SetLength(FHistogram, ChunkSize div 2);

  // read histogram data
  for Index := 0 to Length(FHistogram) - 1 do
    FHistogram[Index] := BigEndian.ReadWord(Stream);
end;

procedure TPngChunkImageHistogram.WriteToStream(Stream: TStream);
var
  Index : Integer;
begin
  // write histogram data
  for Index := 0 to Length(FHistogram) - 1 do
    BigEndian.WriteWord(Stream, FHistogram[Index]);
end;

initialization
  RegisterPngChunk(TPngChunkImageHistogram);
end.
