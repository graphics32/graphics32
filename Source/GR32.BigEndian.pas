unit GR32.BigEndian;

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
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Anders Melander <anders@melander.dk>
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2009
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

uses
  Classes,
  SysUtils;

// Define CHECKED_STREAM_READS to have stream read operations raise an exception
// (of type EBigEndian) if they fail to read the required amount of data.
{$define CHECKED_STREAM_READS}

type
  EBigEndian = class(Exception);

//------------------------------------------------------------------------------
//
//      Big Endian I/O
//
//------------------------------------------------------------------------------
type
  BigEndian = record
    class function ReadByte(Stream: TStream): Byte; static; {$IFDEF INLINING_ENHANCED_RECORDS} inline; {$ENDIF}
    class function ReadWord(Stream: TStream): Word; static; {$IFDEF INLINING_ENHANCED_RECORDS} inline; {$ENDIF}
    class function ReadSmallInt(Stream: TStream): SmallInt; static; {$IFDEF INLINING_ENHANCED_RECORDS} inline; {$ENDIF}
    class function ReadCardinal(Stream: TStream): Cardinal; static; {$IFDEF INLINING_ENHANCED_RECORDS} inline; {$ENDIF}
    class function ReadInt64(Stream: TStream): Int64; static; {$IFDEF INLINING_ENHANCED_RECORDS} inline; {$ENDIF}

    class procedure WriteByte(Stream: TStream; Value: Byte); static; {$IFDEF INLINING_ENHANCED_RECORDS} inline; {$ENDIF}
    class procedure WriteWord(Stream: TStream; Value: Word); static; {$IFDEF INLINING_ENHANCED_RECORDS} inline; {$ENDIF}
    class procedure WriteSmallInt(Stream: TStream; Value: SmallInt); static; {$IFDEF INLINING_ENHANCED_RECORDS} inline; {$ENDIF}
    class procedure WriteCardinal(Stream: TStream; Value: Cardinal); static; {$IFDEF INLINING_ENHANCED_RECORDS} inline; {$ENDIF}
    class procedure WriteInt64(Stream: TStream; Value: Int64); static; {$IFDEF INLINING_ENHANCED_RECORDS} inline; {$ENDIF}
  end;

resourcestring
  sStreamReadError = 'Stream read error';

implementation

uses
  GR32_LowLevel;

//------------------------------------------------------------------------------
//
//      Big Endian I/O
//
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Big endian stream writers
//------------------------------------------------------------------------------
class procedure BigEndian.WriteByte(Stream: TStream; Value: Byte);
begin
  Stream.Write(Value, SizeOf(Byte));
end;

class procedure BigEndian.WriteWord(Stream: TStream; Value: Word);
begin
  Value := Swap16(Value);
  Stream.Write(Value, SizeOf(Word));
end;

class procedure BigEndian.WriteSmallInt(Stream: TStream; Value: SmallInt);
begin
  Value := Swap16(Value);
  Stream.Write(Value, SizeOf(SmallInt));
end;

class procedure BigEndian.WriteCardinal(Stream: TStream; Value: Cardinal);
begin
  Value := Swap32(Value);
  Stream.Write(Value, SizeOf(Cardinal));
end;

class procedure BigEndian.WriteInt64(Stream: TStream; Value: Int64);
begin
  Value := Swap64(Value);
  Stream.Write(Value, SizeOf(Int64));
end;

//------------------------------------------------------------------------------
// Big endian stream readers
//------------------------------------------------------------------------------
class function BigEndian.ReadByte(Stream: TStream): Byte;
begin
{$IFDEF CHECKED_STREAM_READS}
  if Stream.Read(Result, SizeOf(Byte)) <> SizeOf(Byte) then
    raise EBigEndian.Create(sStreamReadError);
{$ELSE}
  Stream.Read(Result, SizeOf(Byte));
{$ENDIF}
end;

class function BigEndian.ReadWord(Stream: TStream): Word;
begin
{$IFDEF CHECKED_STREAM_READS}
  if Stream.Read(Result, SizeOf(Word)) <> SizeOf(Word) then
    raise EBigEndian.Create(sStreamReadError);
{$ELSE}
  Stream.Read(Result, SizeOf(Word));
{$ENDIF}
  Result := Swap16(Result);
end;

class function BigEndian.ReadSmallInt(Stream: TStream): SmallInt;
begin
{$IFDEF CHECKED_STREAM_READS}
  if Stream.Read(Result, SizeOf(SmallInt)) <> SizeOf(SmallInt) then
    raise EBigEndian.Create(sStreamReadError);
{$ELSE}
  Stream.Read(Result, SizeOf(SmallInt));
{$ENDIF}
  Result := SmallInt(Swap16(Word(Result)));
end;

class function BigEndian.ReadCardinal(Stream: TStream): Cardinal;
begin
{$IFDEF CHECKED_STREAM_READS}
  Assert(SizeOf(Cardinal) = 4);
  if Stream.Read(Result, SizeOf(Cardinal)) <> SizeOf(Cardinal) then
    raise EBigEndian.Create(sStreamReadError);
{$ELSE}
  Stream.Read(Result, SizeOf(Cardinal));
{$ENDIF}
  Result := Swap32(Result);
end;

class function BigEndian.ReadInt64(Stream: TStream): Int64;
begin
{$IFDEF CHECKED_STREAM_READS}
  if Stream.Read(Result, SizeOf(Int64)) <> SizeOf(Int64) then
    raise EBigEndian.Create(sStreamReadError);
{$ELSE}
  Stream.Read(Result, SizeOf(Int64));
{$ENDIF}
  Result := Swap64(Result);
end;

end.

