unit GR32.SVG.Utf8;

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
 * The Original Code is SVG reader for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Anders Melander <anders@melander.dk>
 *
 * Portions created by the Initial Developer are Copyright (C) 2026
 * the Initial Developer. All Rights Reserved.
 *
 * The code is this unit was extracted and adapted from mORMot 2:
 * https://github.com/synopse/mORMot2/src/core/mormot.core.fmt.pas
 * Commit SHA: 3af914ac15eab28c41411a1c449da2cf5b1e3a67
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

type
  RawUtf8 = UTF8String;
  PRawUtf8 = ^RawUtf8;
  PUtf8Char = PAnsiChar;
  PPUtf8Char = ^PUtf8Char;
{$ifdef CPU64}
  PtrInt = NativeInt;
  PtrUInt = NativeUInt;
{$else}
  PtrInt = integer;
  PtrUInt = cardinal;
{$endif CPU64}

  /// points to one value as raw memory buffer pointer and length
  TValuePointer = record
    /// a pointer to the actual UTF-8 or binary content
    Buffer: Pointer;
    /// how many bytes are stored in Buffer
    Len: PtrInt;
  end;

  /// points to one value of raw UTF-8 content, decoded from e.g. an XML buffer
  TValuePUtf8Char = record
  private type
    TAnsiSet = set of AnsiChar;
  public
    /// a pointer to the actual UTF-8 text
    Text: PUtf8Char;
    /// how many UTF-8 bytes are stored in Value
    Len: PtrInt;
    /// convert the value into a UTF-8 string
    procedure ToUtf8(var Value: RawUtf8); overload; {$ifdef HASINLINE}inline;{$endif}
    /// convert the value into a UTF-8 string
    function ToUtf8: RawUtf8; overload; {$ifdef HASINLINE}inline;{$endif}
    /// convert the value into a RTL string
    function ToString: string; {$ifdef HASINLINE}inline;{$endif}
    /// convert the value into a signed integer
    function ToInteger: PtrInt; {$ifdef HASINLINE}inline;{$endif}
    /// convert the value into an unsigned integer
    function ToCardinal: PtrUInt; overload; {$ifdef HASINLINE}inline;{$endif}
    /// convert the value into an unsigned integer
    function ToCardinal(Def: PtrUInt): PtrUInt; overload; {$ifdef HASINLINE}inline;{$endif}
    /// convert the value into a 64-bit signed integer
    function ToInt64: Int64; {$ifdef HASINLINE}inline;{$endif}
    /// returns true if Value is either '1' or 'true'
    function ToBoolean: Boolean;
    /// convert the value into a floating point number
    function ToDouble: Double; {$ifdef HASINLINE}inline;{$endif}
    /// case-sensitive comparison with the stored text Value
    function Equal(const Value: RawUtf8): Boolean; overload; {$ifdef HASINLINE}inline;{$endif}
    /// case-sensitive comparison with the stored text Value
    function Equal(Value: PUtf8Char; ValueLen: PtrInt): Boolean; overload;

    /// case-insensitive comparison with the stored text Value
    function CompareText(const AValue: ansistring): Boolean;
    function StartsText(const AValue: ansistring; ASkip: boolean = False): Boolean;

    function ToCardinalAndSkip: Cardinal;

    procedure Trim; overload;
    procedure Trim(ASkip: TAnsiSet); overload;
    procedure Skip(Count: integer = 1);
    function SkipUntil(ASkip: TAnsiSet; AAfter: boolean = False): boolean;
    function Split(AChar: AnsiChar; ASkip: boolean = False): TValuePUtf8Char;
  end;

/// extract a 64-bit unsigned integer from a UTF-8 text buffer
function GetCardinal(P: PUtf8Char; PEnd: PUtf8Char = nil): PtrUInt; overload;
function GetCardinal(P: PUtf8Char; Len: PtrInt): PtrUInt; overload;

/// extract a 64-bit signed integer from a UTF-8 text buffer
function GetInt64(P: PUtf8Char; PEnd: PUtf8Char = nil): Int64; overload;
function GetInt64(P: PUtf8Char; Len: PtrInt): Int64; overload;

/// extract a signed integer from a UTF-8 text buffer
function GetInteger(P: PUtf8Char; PEnd: PUtf8Char = nil): PtrInt; overload;
function GetInteger(P: PUtf8Char; Len: PtrInt): PtrInt; overload;

/// extract an Extended floating point number from a UTF-8 text buffer
function GetExtended(P: PUtf8Char; out Value: Extended; PEnd: PUtf8Char = nil): PUtf8Char; overload;
function GetExtended(P: PUtf8Char; Len: PtrInt; out Value: Extended): Boolean; overload;
function GetExtended(P: PUtf8Char; Len: PtrInt; out Value: Double): Boolean; overload;

implementation

uses
  Types,
  SysUtils,
  AnsiStrings,
  Math;

function PosChar(Str: PUtf8Char; StrLen: PtrInt; Chr: AnsiChar): PUtf8Char; overload;
begin
  if (Str <> nil) and (StrLen > 0) then
  begin
    while StrLen > 0 do
    begin
      if Str^ = Chr then
      begin
        Result := Str;
        Exit;
      end;
      Inc(Str);
      Dec(StrLen);
    end;
  end;
  Result := nil;
end;

function PosChar(Str: PUtf8Char; Chr: AnsiChar): PUtf8Char; overload;
begin
  if Str <> nil then
  begin
    while Str^ <> #0 do
    begin
      if Str^ = Chr then
      begin
        Result := Str;
        Exit;
      end;
      Inc(Str);
    end;
  end;
  Result := nil;
end;

function PosChar0(Str: PUtf8Char; Chr: AnsiChar): PUtf8Char;
begin
  Result := PosChar(Str, Chr);
  if Result = nil then
    Result := Str + AnsiStrings.StrLen(Str);
end;

function ByteScanIndex(P: PByteArray; Count: PtrInt; Value: byte): PtrInt;
begin
  result := 0;
  if P <> nil then
    repeat
      if result >= Count then
        break;
      if P^[result] = Value then
        exit;
      inc(result);
    until false;
  result := -1;
end;

function CompareMemSmall(P1, P2: Pointer; Length: PtrInt): Boolean;
begin
  Result := CompareMem(P1, P2, Length);
end;

function GetCardinal(P: PUtf8Char; PEnd: PUtf8Char): PtrUInt;
var
  c: PtrUInt;
begin
  Result := 0;
  if P = nil then Exit;
  if PEnd = nil then
    PEnd := P + $7FFFFFFF;
  while (P < PEnd) and (P^ in ['0'..'9']) do
  begin
    c := Ord(P^) - Ord('0');
    Result := Result * 10 + c;
    Inc(P);
  end;
end;

function GetCardinal(P: PUtf8Char; Len: PtrInt): PtrUInt;
begin
  if (P = nil) or (Len <= 0) then
    Result := 0
  else
    Result := GetCardinal(P, P + Len);
end;

function GetInt64(P: PUtf8Char; PEnd: PUtf8Char): Int64;
var
  neg: Boolean;
  c: Int64;
begin
  Result := 0;
  if P = nil then Exit;
  if PEnd = nil then
    PEnd := P + $7FFFFFFF;
  while (P < PEnd) and (P^ in [#1..#32]) do
    Inc(P);
  neg := False;
  if P < PEnd then
  begin
    if P^ = '-' then
    begin
      neg := True;
      Inc(P);
    end
    else if P^ = '+' then
      Inc(P);
  end;
  while (P < PEnd) and (P^ in ['0'..'9']) do
  begin
    c := Ord(P^) - Ord('0');
    Result := Result * 10 + c;
    Inc(P);
  end;
  if neg then
    Result := -Result;
end;

function GetInt64(P: PUtf8Char; Len: PtrInt): Int64;
begin
  if (P = nil) or (Len <= 0) then
    Result := 0
  else
    Result := GetInt64(P, P + Len);
end;

function GetInteger(P: PUtf8Char; PEnd: PUtf8Char): PtrInt;
begin
  Result := PtrInt(GetInt64(P, PEnd));
end;

function GetInteger(P: PUtf8Char; Len: PtrInt): PtrInt;
begin
  Result := PtrInt(GetInt64(P, Len));
end;

function GetExtended(P: PUtf8Char; out Value: Extended; PEnd: PUtf8Char): PUtf8Char;
var
  Sign, ExpSign: Boolean;
  IntVal, FracVal: Int64;
  FracLen, ExpVal: Integer;
  E: Extended;
begin
  Value := 0.0;
  if P = nil then
  begin
    Result := nil;
    Exit;
  end;
  if PEnd = nil then
    PEnd := P + $7FFFFFFF;

  while (P < PEnd) and (P^ in [#1..#32]) do
    Inc(P);

  if P >= PEnd then
  begin
    Result := P;
    Exit;
  end;

  Sign := False;
  if P^ = '-' then
  begin
    Sign := True;
    Inc(P);
  end
  else if P^ = '+' then
    Inc(P);

  IntVal := 0;
  while (P < PEnd) and (P^ in ['0'..'9']) do
  begin
    IntVal := IntVal * 10 + (Ord(P^) - Ord('0'));
    Inc(P);
  end;

  E := IntVal;

  if (P < PEnd) and (P^ in ['.', ',']) then
  begin
    Inc(P);
    FracVal := 0;
    FracLen := 0;
    while (P < PEnd) and (P^ in ['0'..'9']) do
    begin
      if FracLen < 18 then
      begin
        FracVal := FracVal * 10 + (Ord(P^) - Ord('0'));
        Inc(FracLen);
      end;
      Inc(P);
    end;
    if FracLen > 0 then
      E := E + FracVal / Power(10.0, FracLen);
  end;

  if (P < PEnd) and (P^ in ['e', 'E']) then
  begin
    Inc(P);
    ExpSign := False;
    if (P < PEnd) and (P^ = '-') then
    begin
      ExpSign := True;
      Inc(P);
    end
    else if (P < PEnd) and (P^ = '+') then
      Inc(P);

    ExpVal := 0;
    while (P < PEnd) and (P^ in ['0'..'9']) do
    begin
      ExpVal := ExpVal * 10 + (Ord(P^) - Ord('0'));
      Inc(P);
    end;

    if ExpVal <> 0 then
    begin
      if ExpSign then
        E := E / Power(10.0, ExpVal)
      else
        E := E * Power(10.0, ExpVal);
    end;
  end;

  if Sign then
    E := -E;

  Value := E;
  Result := P;
end;

function GetExtended(P: PUtf8Char; Len: PtrInt; out Value: Extended): Boolean;
var
  pRes: PUtf8Char;
begin
  if (P = nil) or (Len <= 0) then
  begin
    Value := 0.0;
    Exit(False);
  end;
  pRes := GetExtended(P, Value, P + Len);
  Result := (pRes <> P);
end;

function GetExtended(P: PUtf8Char; Len: PtrInt; out Value: Double): Boolean;
var
  ext: Extended;
begin
  Result := GetExtended(P, Len, ext);
  Value := ext;
end;

{ TValuePUtf8Char }

procedure TValuePUtf8Char.ToUtf8(var Value: RawUtf8);
begin
  SetString(Value, Text, Len);
end;

function TValuePUtf8Char.ToUtf8: RawUtf8;
begin
  SetString(Result, Text, Len);
end;

procedure TValuePUtf8Char.Trim(ASkip: TAnsiSet);
begin
  while (Len > 0) and (Text^ in ASkip) do
    Skip;
end;

procedure TValuePUtf8Char.Trim;
begin
  while (Len > 0) and (Text^ in [#1..#32]) do
    Skip;
end;

function TValuePUtf8Char.ToString: string;
var
  u: RawUtf8;
begin
  SetString(u, Text, Len);
  Result := string(u);
end;

function TValuePUtf8Char.ToInteger: PtrInt;
begin
  Result := GetInteger(Text, Len);
end;

function TValuePUtf8Char.ToCardinal: PtrUInt;
begin
  Result := ToCardinal(0);
end;

function TValuePUtf8Char.ToCardinal(Def: PtrUInt): PtrUInt;
var
  i: Int64;
begin
  if (Text = nil) or (Len <= 0) then
    Exit(Def);
  i := GetInt64(Text, Len);
  if (i < 0) or (i > High(Cardinal)) then
    Result := Def
  else
    Result := Cardinal(i);
end;

function TValuePUtf8Char.ToInt64: Int64;
begin
  Result := GetInt64(Text, Len);
end;

function TValuePUtf8Char.ToBoolean: Boolean;
begin
  if (Text = nil) or (Len <= 0) then
    Exit(False);
  if (Len = 1) and (Text^ = '1') then
    Exit(True);
  if (Len = 4) and
     ((Text[0] = 't') or (Text[0] = 'T')) and
     ((Text[1] = 'r') or (Text[1] = 'R')) and
     ((Text[2] = 'u') or (Text[2] = 'U')) and
     ((Text[3] = 'e') or (Text[3] = 'E')) then
    Exit(True);
  Result := False;
end;

function TValuePUtf8Char.ToCardinalAndSkip: Cardinal;
begin
  Result := 0;
  while (Len > 0) and (Text^ in ['0'..'9']) do
  begin
    Result := Result * 10 + (Ord(Text^) - Ord('0'));
    Skip;
  end;
end;

function TValuePUtf8Char.ToDouble: Double;
var
  ext: Extended;
begin
  if GetExtended(Text, Len, ext) then
    Result := ext
  else
    Result := 0.0;
end;

function TValuePUtf8Char.Equal(const Value: RawUtf8): Boolean;
begin
  Result := Equal(PUtf8Char(Value), Length(Value));
end;

function TValuePUtf8Char.Equal(Value: PUtf8Char; ValueLen: PtrInt): Boolean;
begin
  Result := (Len = ValueLen) and CompareMem(Text, Value, Len);
end;

procedure TValuePUtf8Char.Skip(Count: integer);
begin
  if (Count > Len) then
    Count := Len;
  Inc(Text, Count);
  Dec(Len, Count);
end;

function TValuePUtf8Char.SkipUntil(ASkip: TAnsiSet; AAfter: boolean): boolean;
begin
  while (Len > 0) and not(Text^ in ASkip) do
  begin
    Skip;
    Result := True;
  end;

  if Result and AAfter then
    Skip;
end;

function TValuePUtf8Char.Split(AChar: AnsiChar; ASkip: boolean): TValuePUtf8Char;
var
  p: PUtf8Char;
  l: PtrInt;
begin
  Result.Text := Text;
  Result.Len := 0;
  p := Text;
  l := Len;
  while (l > 0) and (p^ <> AChar) do
  begin
    Inc(p);
    Dec(l);
    Inc(Result.Len);
  end;

  if (ASkip) then
  begin
    Text := p;
    Len := l;
    if (Len > 0) and (Text^ = AChar) then
      Skip;
  end;
end;

function TValuePUtf8Char.StartsText(const AValue: ansistring; ASkip: boolean): Boolean;
var
  i: Integer;
  p1: PUtf8Char;
  c1, c2: Byte;
begin
  if Len < Length(AValue) then
    Exit(False);

  p1 := Text;

  for i := 1 to Length(AValue) do
  begin
    c1 := Byte(p1^);
    c2 := Byte(AValue[i]);

    // Convert ASCII uppercase A-Z (65..90) to lowercase (97..122) in-place
    if c1 in [65..90] then
      Inc(c1, 32);
    if c2 in [65..90] then
      Inc(c2, 32);

    if c1 <> c2 then
      Exit(False);

    Inc(p1);
  end;

  if (ASkip) then
  begin
    Inc(Text, Length(AValue));
    Dec(Len, Length(AValue));
  end;

  Result := True;
end;

function TValuePUtf8Char.CompareText(const AValue: ansistring): Boolean;
var
  i: Integer;
  p1: PUtf8Char;
  c1, c2: Byte;
begin
  if Len <> Length(AValue) then
    Exit(False);

  p1 := Text;

  for i := 1 to Len do
  begin
    c1 := Byte(p1^);
    c2 := Byte(AValue[i]);

    // Convert ASCII uppercase A-Z (65..90) to lowercase (97..122) in-place
    if c1 in [65..90] then
      Inc(c1, 32);
    if c2 in [65..90] then
      Inc(c2, 32);

    if c1 <> c2 then
      Exit(False);

    Inc(p1);
  end;

  Result := True;
end;

end.
