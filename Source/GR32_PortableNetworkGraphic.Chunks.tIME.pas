unit GR32_PortableNetworkGraphic.Chunks.tIME;

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
//      TPngChunkTime
//
//------------------------------------------------------------------------------
type
  TPngChunkTime = class(TCustomDefinedChunkWithHeader)
  private
    FYear   : Word;
    FMonth  : Byte;
    FDay    : Byte;
    FHour   : Byte;
    FMinute : Byte;
    FSecond : Byte;
    function GetModifiedDateTime: TDateTime;
    procedure SetModifiedDateTime(const Value: TDateTime);
  protected
    class function GetClassChunkName: TChunkName; override;
    function GetChunkSize: Cardinal; override;

  public
    procedure Assign(Source: TPersistent); override;

    procedure ReadFromStream(Stream: TStream; ChunkSize: Cardinal); override;
    procedure WriteToStream(Stream: TStream); override;

    property Year: Word read FYear write FYear;
    property Month: Byte read FMonth write FMonth;
    property Day: Byte read FDay write FDay;
    property Hour: Byte read FHour write FHour;
    property Minute: Byte read FMinute write FMinute;
    property Second: Byte read FSecond write FSecond;
    property ModifiedDateTime: TDateTime read GetModifiedDateTime write SetModifiedDateTime;
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
//      TPngChunkTime
//
//------------------------------------------------------------------------------
procedure TPngChunkTime.Assign(Source: TPersistent);
begin
  inherited;

  if (Source is TPngChunkTime) then
  begin
    FYear   := TPngChunkTime(Source).Year;
    FMonth  := TPngChunkTime(Source).Month;
    FDay    := TPngChunkTime(Source).Day;
    FHour   := TPngChunkTime(Source).Hour;
    FMinute := TPngChunkTime(Source).Minute;
    FSecond := TPngChunkTime(Source).Second;
  end;
end;

class function TPngChunkTime.GetClassChunkName: TChunkName;
begin
  Result := 'tIME';
end;

function TPngChunkTime.GetModifiedDateTime: TDateTime;
begin
  Result := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Minute, Second, 0);
end;

function TPngChunkTime.GetChunkSize: Cardinal;
begin
  Result := 7;
end;

procedure TPngChunkTime.ReadFromStream(Stream: TStream; ChunkSize: Cardinal);
begin
  if (Stream.Position+ChunkSize > Stream.Size) or (GetChunkSize > ChunkSize) then
    raise EPngError.Create(RCStrChunkSizeTooSmall);

  // read year
  FYear := BigEndian.ReadWord(Stream);

  // read month
  Stream.Read(FMonth, SizeOf(Byte));

  // read day
  Stream.Read(FDay, SizeOf(Byte));

  // read hour
  Stream.Read(FHour, SizeOf(Byte));

  // read minute
  Stream.Read(FMinute, SizeOf(Byte));

  // read second
  Stream.Read(FSecond, SizeOf(Byte));
end;

procedure TPngChunkTime.WriteToStream(Stream: TStream);
begin
  // write year
  BigEndian.WriteWord(Stream, FYear);

  // write month
  Stream.Write(FMonth, SizeOf(Byte));

  // write day
  Stream.Write(FDay, SizeOf(Byte));

  // write hour
  Stream.Write(FHour, SizeOf(Byte));

  // write minute
  Stream.Write(FMinute, SizeOf(Byte));

  // write second
  Stream.Write(FSecond, SizeOf(Byte));
end;

procedure TPngChunkTime.SetModifiedDateTime(const Value: TDateTime);
var
  mnth : Word;
  day  : Word;
  hour : Word;
  min  : Word;
  sec  : Word;
  msec : Word;
begin
  DecodeDate(Value, FYear, mnth, day);
  FMonth := mnth;
  FDay := day;
  DecodeTime(Value, hour, min, sec, msec);
  FHour := hour;
  FMinute := min;
  FSecond := sec;
end;

initialization
  RegisterPngChunk(TPngChunkTime);
end.
