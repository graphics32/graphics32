unit GR32.SVG.Xml;

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

uses
  SysUtils, Classes, System.Types, Math;

type
  RawUtf8 = UTF8String;
  PRawUtf8 = ^RawUtf8;
  PUtf8Char = PAnsiChar;
  PPUtf8Char = ^PUtf8Char;
  Ucs4CodePoint = Cardinal;
{$ifdef CPU64}
  PtrInt = NativeInt;
  PtrUInt = NativeUInt;
{$else}
  PtrInt = integer;
  PtrUInt = cardinal;
{$endif CPU64}

  TAnsiCharToByte = array[AnsiChar] of Byte;
  PAnsiCharToByte = ^TAnsiCharToByte;

  PBits32 = ^TBits32;
  TBits32 = set of 0..31;

  TQwordRec = packed record
    case Integer of
      0: (Value: UInt64);
      1: (L, H: Cardinal);
      2: (W: array[0..3] of Word);
      3: (B: array[0..7] of Byte);
  end;

  /// points to one value as raw memory buffer pointer and length
  TValuePointer = record
    /// a pointer to the actual UTF-8 or binary content
    Buffer: Pointer;
    /// how many bytes are stored in Buffer
    Len: PtrInt;
  end;

  /// points to one value of raw UTF-8 content, decoded from e.g. an XML buffer
  TValuePUtf8Char = record
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
  end;

  /// exception raised by TXmlParser on invalid or unsupported XML input
  EXmlException = class(Exception);

  /// the kind of tokens returned by TXmlParser.Next
  TXmlToken = (
    xtNotStarted,
    xtEof,
    xtError,
    xtElementStart,
    xtAttribute,
    xtElementEnd,
    xtText,
    xtCData,
    xtComment,
    xtPI);

  /// parsing errors as recognized during TXmlParser process
  TXmlParserError = (
    xpeNone,
    xpeEofInTag,
    xpeSlashInTag,
    xpeUnexpectedTagEnd,
    xpeInvalidAttrName,
    xpeMissingAttrValue,
    xpeMissingAttrQuote,
    xpeEofInAttribute,
    xpeEofElement,
    xpeEofToken,
    xpeVoidEndTag,
    xpeEofEndTag,
    xpeUnexpectedEndTag,
    xpeWrongEndTag,
    xpeEofInComment,
    xpeEofInCdata,
    xpeUnsupportedMarkup,
    xpeVoidPiName,
    xpeEofInPi,
    xpeVoidTagName,
    xpeTagNameTooLong,
    xpeTooMuchNesting,
    xpeXmlUnescapeFailed);

  /// option to refine TXmlParser process
  TXmlParserOption = (
    xpoNoException,
    xpoStripNamespacePrefix,
    xpoDontCheckEndTagName,
    xpoKeepComments,
    xpoKeepPI,
    xpoKeepWhiteSpace,
    xpoVariantGuessType);

  /// options to refine TXmlParser process
  TXmlParserOptions = set of TXmlParserOption;

  /// a pointer to TXmlParser instance
  PXmlParser = ^TXmlParser;

  /// zero-allocation SAX-like parser over an XML UTF-8 memory buffer
  TXmlParser = record
  private
    procedure SetOrRaiseLastError;
    procedure SetOrRaiseError(reason: TXmlParserError);
      {$ifdef HASINLINE} inline; {$endif}
    function ParseName(p, e: PUtf8Char): PUtf8Char;
      {$ifdef HASINLINE} inline; {$endif}
  public
    /// the current token kind, as set by the last ParseNext call
    Kind: TXmlToken;
    /// how many elements are currently opened via ParseNext
    Depth: Byte;
    /// options to refine TXmlParser process
    Options: TXmlParserOptions;
    /// xpeNone if no error, or the xtError associated context - see XML_ERROR[]
    LastError: TXmlParserError;
    /// 0 if LastError=xpeNone, or xtError line number (starting at 1)
    LastErrorLine: Cardinal;
    /// the current token UTF-8 name, pointing within the input buffer
    Name: TValuePUtf8Char;
    /// the current token raw value, pointing within the input buffer
    Value: TValuePointer;
    /// prepare the parsing of a given XML UTF-8 buffer
    procedure Init(Text: PUtf8Char; TextLen: PtrInt;
      ParserOptions: TXmlParserOptions = []); overload;
    /// prepare the parsing of a given XML UTF-8 string content
    function Init(const Text: RawUtf8;
      ParserOptions: TXmlParserOptions = []): PXmlParser; overload;
      {$ifdef HASINLINE} inline; {$endif}
    /// reset the current position to the beginning of the XML supplied to Init()
    function Rewind: PXmlParser;
    /// locate an element using a simplified XPath-like syntax
    function Find(Path: PUtf8Char; Sep: AnsiChar = '/'): Boolean;
    /// iterate over the direct child elements matching a given name
    function ForEach(const ElementName: RawUtf8; LoopSlot: Cardinal): Boolean;
    /// iterate to the next token of the input, returning xtEof when done
    function ParseNext: TXmlToken;
    /// returns the current Name as an allocated UTF-8 string
    procedure NameToUtf8(var result: RawUtf8);
      {$ifdef HASINLINE}inline;{$endif}
    /// decode the current Value as an allocated UTF-8 string
    function ValueToUtf8(var Dest: RawUtf8): Boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// decode and append the current Value to an existing UTF-8 string
    function ValueAppendToUtf8(var Dest: RawUtf8): Boolean;
    /// iterate over the direct child elements matching a given name
    function Next(const ElementName: RawUtf8): Boolean; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// iterate over the direct child elements matching a given name
    function Next(ElementName: PUtf8Char; ElementLen: PtrInt): Boolean; overload;
    /// consume/skip the current element subtree
    function Skip: Boolean;
    /// consume the current element subtree as text
    function ConsumeText(var Dest: RawUtf8): Boolean;
    /// iterate until a given element name is reached anywhere in the content
    function FindAny(ElementName: PUtf8Char; ElementLen: PtrInt): Boolean;
    /// retrieve a text sub-value via Save+Find+ConsumeText+Restore
    function GetU(Path: PUtf8Char; var V: RawUtf8): Boolean;
    /// retrieve an integer sub-value via Save+Find+ConsumeText+Restore+ToInt64
    function GetI(Path: PUtf8Char; var V: Int64): Boolean;
    /// save the current state of the parser (Position, Kind and Depth)
    procedure Save;
    /// restore the previous state of the parser (Position, Kind and Depth)
    procedure Restore;
    /// continue after the element from a previously saved level
    function RestoreAndSkip: Boolean;
    /// the offset of the current token in the input buffer
    function Position: PtrInt;
      {$ifdef HASINLINE}inline;{$endif}
    /// raise the EXmlException corresponding to LastError/LastErrorLine
    procedure RaiseException;
  private
    {$ifndef FPCX86NOTPIC}
    fTab: PAnsiCharToByte;
    {$endif FPCX86NOTPIC}
    fBegin, fCur, fToken, fAfter: PUtf8Char;
    fStackLen: array[Byte] of Byte;     // 255-byte names
    fStackPos: array[Byte] of Cardinal; // 32-bit offsets from fBegin
    fSave: array[0..31] of TQwordRec;   // for Save/Restore (len=fStackLen[255])
  end;

const
  /// text description of all TXmlParser process errors
  XML_ERROR: array[TXmlParserError] of string = (
    '',
    'unexpected end of input within a tag',      // xpeEofInTag
    'invalid "/" within a tag',                  // xpeSlashInTag
    'unexpected tag ending',                     // xpeUnexpectedTagEnd
    'void or invalid attribute name',            // xpeInvalidAttrName
    'attribute expects "="',                     // xpeMissingAttrValue
    'attribute value expects quotes',            // xpeMissingAttrQuote
    'unfinished attribute value',                // xpeEofInAttribute
    'unexpected end of input: unclosed element', // xpeEofElement
    'unexpected end of input',                   // xpeEofToken
    'void end tag',                              // xpeVoidEndTag
    'unexpected end of input within end tag',    // xpeEofEndTag
    'unexpected end tag',                        // xpeUnexpectedEndTag
    'wrong end tag name',                        // xpeWrongEndTag
    'unexpected end of input within comment',    // xpeEofInComment
    'unexpected end of input within CDATA',      // xpeEofInCdata
    'unsupported markup syntax',                 // xpeUnsupportedMarkup
    'void processing instruction name',          // xpeVoidPiName
    'unexpected end of input within PI',         // xpeEofInPi
    'void tag name',                             // xpeVoidTagName
    'tag name is too long',                      // xpeTagNameTooLong
    'nesting level exceeds 255',                 // xpeTooMuchNesting
    'invalid XML entity reference');             // xpeXmlUnescapeFailed

/// decode the five XML predefined entities and numeric character references
function XmlUnescape(Text: PUtf8Char; TextLen: PtrInt; var Dest: RawUtf8;
  amp: PUtf8Char = nil): Boolean;

/// low-level decoding of a '#integer' or '#xhexa' numeric character reference
function NumCharToUcs4(entity: PUtf8Char; len: PtrUInt): Ucs4CodePoint;

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

const
  BOM_UTF8 = $BFBBEF;
  UNICODE_MAX = $10FFFF;
  UTF16_HISURROGATE_MIN = $D800;
  UTF16_LOSURROGATE_MAX = $DFFF;

var
  XML_ESC: TAnsiCharToByte;
  XML_KIND: TAnsiCharToByte;

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
    Result := Str + StrLen(Str);
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

function AddXmlUnescape(var Dest: RawUtf8; p, amp: PUtf8Char; plen: PtrUInt): Boolean;
var
  l: PtrUInt;
  c: Ucs4CodePoint;
  outLen: PtrInt;

  procedure AppendChar(ch: AnsiChar);
  begin
    outLen := Length(Dest);
    SetLength(Dest, outLen + 1);
    Dest[outLen + 1] := ch;
  end;

  procedure AppendBytes(buf: PUtf8Char; count: PtrInt);
  var
    oldLen: PtrInt;
  begin
    if count <= 0 then Exit;
    oldLen := Length(Dest);
    SetLength(Dest, oldLen + count);
    Move(buf^, Dest[oldLen + 1], count);
  end;

  procedure AppendUcs4(code: Ucs4CodePoint);
  begin
    if code <= $7F then
      AppendChar(AnsiChar(code))
    else if code <= $7FF then
    begin
      AppendChar(AnsiChar($C0 or (code shr 6)));
      AppendChar(AnsiChar($80 or (code and $3F)));
    end
    else if code <= $FFFF then
    begin
      AppendChar(AnsiChar($E0 or (code shr 12)));
      AppendChar(AnsiChar($80 or ((code shr 6) and $3F)));
      AppendChar(AnsiChar($80 or (code and $3F)));
    end
    else if code <= $10FFFF then
    begin
      AppendChar(AnsiChar($F0 or (code shr 18)));
      AppendChar(AnsiChar($80 or ((code shr 12) and $3F)));
      AppendChar(AnsiChar($80 or ((code shr 6) and $3F)));
      AppendChar(AnsiChar($80 or (code and $3F)));
    end;
  end;

begin
  repeat
    if amp = nil then
    begin
      amp := PosChar(p, plen, '&');
      if amp = nil then
      begin
        AppendBytes(p, plen);
        Break;
      end;
    end;
    l := amp - p;
    if l <> 0 then
    begin
      AppendBytes(p, l);
      Dec(plen, l);
      p := amp;
    end;
    amp := nil;
    Inc(p);
    Dec(plen);

    l := 0;
    while (l < plen) and (p[l] <> ';') do
      Inc(l);
    c := 0;
    if (l < plen) and (l < 12) and (p[l] = ';') then
    begin
      case p^ of
        '#':
          c := NumCharToUcs4(p, l);
        'l':
          if (l = 2) and (p[1] = 't') then
            c := Ord('<');
        'g':
          if (l = 2) and (p[1] = 't') then
            c := Ord('>');
        'a':
          if (l = 3) and (PCardinal(p)^ and $00FFFFFF = Ord('a') + Ord('m') shl 8 + Ord('p') shl 16) then
            c := Ord('&')
          else if (l = 4) and (PCardinal(p)^ = Ord('a') + Ord('p') shl 8 + Ord('o') shl 16 + Ord('s') shl 24) then
            c := Ord('''');
        'q':
          if (l = 4) and (PCardinal(p)^ = Ord('q') + Ord('u') shl 8 + Ord('o') shl 16 + Ord('t') shl 24) then
            c := Ord('"');
      end;
    end;

    if c = 0 then
    begin
      Result := False;
      Exit;
    end;

    AppendUcs4(c);
    Inc(l);
    Inc(p, l);
    Dec(plen, l);
  until plen = 0;
  Result := True;
end;

function XmlUnescape(Text: PUtf8Char; TextLen: PtrInt; var Dest: RawUtf8;
  amp: PUtf8Char): Boolean;
begin
  if (amp = nil) and (TextLen > 0) then
    amp := PosChar(Text, TextLen, '&');
  if amp = nil then
  begin
    SetString(Dest, Text, TextLen);
    Result := True;
    Exit;
  end;
  Dest := '';
  Result := AddXmlUnescape(Dest, Text, amp, TextLen);
end;

function NumCharToUcs4(entity: PUtf8Char; len: PtrUInt): Ucs4CodePoint;
var
  c, v: Ucs4CodePoint;
  HexDigit: Byte;
begin
  Result := 0;
  Inc(entity);
  Dec(len);
  if len = 0 then
    Exit;
  c := 0;
  if entity^ in ['x', 'X'] then
  begin
    Inc(entity);
    Dec(len);
    if len = 0 then
      Exit;
    repeat
      case entity^ of
        '0'..'9': HexDigit := Ord(entity^) - Ord('0');
        'a'..'f': HexDigit := Ord(entity^) - Ord('a') + 10;
        'A'..'F': HexDigit := Ord(entity^) - Ord('A') + 10;
      else
        Exit;
      end;
      v := HexDigit;
      c := c shl 4 + v;
      if c > UNICODE_MAX then
        Exit;
      Inc(entity);
      Dec(len);
    until len = 0;
  end
  else
  begin
    repeat
      v := Ord(entity^) - Ord('0');
      if v > 9 then
        Exit;
      c := c * 10 + v;
      if c > UNICODE_MAX then
        Exit;
      Inc(entity);
      Dec(len);
    until len = 0;
  end;
  if (c < UTF16_HISURROGATE_MIN) or (c > UTF16_LOSURROGATE_MAX) then
    Result := c;
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

{ TXmlParser }

procedure TXmlParser.RaiseException;
var
  msg: string;
begin
  if LastError = xpeNone then
    Exit;
  msg := XML_ERROR[LastError];
  if LastErrorLine <> 0 then
    msg := Format('%s at line %d', [msg, LastErrorLine]);
  raise EXmlException.Create(msg);
end;

procedure TXmlParser.SetOrRaiseLastError;
var
  p: PUtf8Char;
begin
  if LastError <> xpeNone then
  begin
    if LastErrorLine = 0 then
    begin
      LastErrorLine := 1;
      p := fBegin;
      while p < fToken do
      begin
        if p^ = #10 then
          Inc(LastErrorLine);
        Inc(p);
      end;
    end;
    Kind := xtError;
    if not (xpoNoException in Options) then
      RaiseException;
  end;
end;

procedure TXmlParser.SetOrRaiseError(reason: TXmlParserError);
begin
  LastError := reason;
  SetOrRaiseLastError;
end;

function TXmlParser.ParseName(p, e: PUtf8Char): PUtf8Char;
begin
  Name.Text := p;
  if xpoStripNamespacePrefix in Options then
    while (p < e) and
          ({$ifdef FPCX86NOTPIC} XML_KIND {$else} fTab^ {$endif}[p^] = 0) do
    begin
      if p^ = ':' then
        Name.Text := p + 1;
      Inc(p);
    end
  else
    while (p < e) and
          ({$ifdef FPCX86NOTPIC} XML_KIND {$else} fTab^ {$endif}[p^] = 0) do
      Inc(p);
  Name.Len := p - Name.Text;
  while (p < e) and (p^ <= ' ') do
    Inc(p);
  Result := p;
end;

procedure TXmlParser.Init(Text: PUtf8Char; TextLen: PtrInt;
  ParserOptions: TXmlParserOptions);
begin
  {$ifdef CPU64}
  if TextLen shr 32 <> 0 then
    raise EXmlException.CreateFmt('TXmlParser cannot parse %d bytes', [TextLen]);
  {$endif CPU64}
  if (Text = nil) or (TextLen <= 0) then
  begin
    Text := nil;
    TextLen := 0;
  end;
  Kind := xtNotStarted;
  Depth := 0;
  Options := ParserOptions;
  LastError := xpeNone;
  LastErrorLine := 0;
  Name.Text := nil;
  Name.Len := 0;
  Value.Buffer := nil;
  Value.Len := 0;
  fBegin := Text;
  if (TextLen >= 3) and
     (PWord(Text)^ = BOM_UTF8 and $FFFF) and
     (PByte(Text)[2] = BOM_UTF8 shr 16) then
  begin
    Inc(Text, 3);
    Dec(TextLen, 3);
  end;
  fCur := Text;
  fToken := Text;
  fAfter := Text + TextLen;
  {$ifndef FPCX86NOTPIC}
  fTab := @XML_KIND;
  {$endif FPCX86NOTPIC}
  fStackLen[High(fStackLen)] := 0;
  fStackPos[High(fStackPos)] := 0;
end;

function TXmlParser.Init(const Text: RawUtf8; ParserOptions: TXmlParserOptions): PXmlParser;
begin
  Init(PUtf8Char(Text), Length(Text), ParserOptions);
  Result := @Self;
end;

function TXmlParser.Rewind: PXmlParser;
begin
  if fCur <> fBegin then
  begin
    if Kind = xtEof then
      fCur := fBegin
    else
      Init(fBegin, fAfter - fBegin, Options);
  end;
  Result := @Self;
end;

function TXmlParser.Position: PtrInt;
begin
  Result := fCur - fBegin;
end;

procedure TXmlParser.Save;
var
  i: PtrInt;
  s: ^TQwordRec;
begin
  i := fStackLen[High(fStackLen)];
  if i = High(fSave) then
    raise EXmlException.Create('Too many TXmlParser.Save');
  s := @fSave[i];
  Inc(i);
  fStackLen[High(fStackLen)] := i;
  s^.L := PCardinal(@Kind)^;
  s^.H := fCur - fBegin;
end;

procedure TXmlParser.Restore;
var
  p: PUtf8Char;
  i: PtrInt;
  s: ^TQwordRec;
begin
  i := fStackLen[High(fStackLen)];
  if i = 0 then
    raise EXmlException.Create('Missing TXmlParser.Save');
  Dec(i);
  fStackLen[High(fStackLen)] := i;
  s := @fSave[i];
  PWord(@Kind)^ := s^.L;
  p := fBegin + s^.H;
  if p <= fCur then
    fCur := p
  else
    raise EXmlException.Create('TXmlParser.Restore: no forward possible');
end;

function TXmlParser.RestoreAndSkip: Boolean;
var
  i: PtrInt;
  level: Byte;
begin
  Result := False;
  i := fStackLen[High(fStackLen)];
  if i = 0 then
    Exit;
  Dec(i);
  level := fSave[i].B[1];
  fStackLen[High(fStackLen)] := i;
  while Depth >= level do
    if ParseNext in [xtEof, xtError] then
      Exit;
  Result := True;
end;

function TXmlParser.ForEach(const ElementName: RawUtf8; LoopSlot: Cardinal): Boolean;
var
  flags: PBits32;
begin
  Result := False;
  flags := @fStackPos[High(fStackPos)];
  if LoopSlot in flags^ then
    if not RestoreAndSkip then
      Exit;
  if Next(ElementName) then
  begin
    Include(flags^, LoopSlot);
    Save;
    Result := True;
  end
  else
    Exclude(flags^, LoopSlot);
end;

function TXmlParser.ParseNext: TXmlToken;
var
  p, e: PUtf8Char;
begin
  Name.Text := nil;
  Name.Len := 0;
  Value.Buffer := nil;
  Value.Len := 0;
  p := fCur;
  e := fAfter;
  if Kind <> xtError then
  repeat
    if (Kind = xtElementStart) or
       (Kind = xtAttribute) then
    begin
      while (p < e) and (p^ <= ' ') do
        Inc(p);
      if p < e then
      begin
        fToken := p;
        case p^ of
          '>':
            begin
              Inc(p);
              Kind := xtElementEnd;
              Continue;
            end;
          '/':
            begin
              Inc(p);
              if (p < e) and (p^ = '>') then
                if Depth <> 0 then
                begin
                  Inc(p);
                  Dec(Depth);
                  Name.Text := fBegin + fStackPos[Depth];
                  Name.Len := fStackLen[Depth];
                  Kind := xtElementEnd;
                  Break;
                end
                else
                  LastError := xpeUnexpectedTagEnd
              else
                LastError := xpeSlashInTag;
            end;
        else
          begin
            p := ParseName(p, e);
            if Name.Len <> 0 then
              if (p < e) and (p^ = '=') then
              begin
                repeat
                  Inc(p);
                until (p = e) or (p^ > ' ');
                if (p <> e) and (p^ in ['"', '''']) then
                begin
                  Inc(p);
                  Value.Buffer := p;
                  Value.Len := ByteScanIndex(pointer(p), e - p, Ord(p[-1]));
                  if Value.Len >= 0 then
                  begin
                    Inc(p, Value.Len + 1);
                    Kind := xtAttribute;
                    Break;
                  end;
                  LastError := xpeEofInAttribute;
                end
                else
                  LastError := xpeMissingAttrQuote;
              end
              else
                LastError := xpeMissingAttrValue
            else
              LastError := xpeInvalidAttrName;
          end;
        end;
      end
      else
        LastError := xpeEofInTag;
    end
    else if p < e then
    begin
      if p^ = '<' then
      begin
        fToken := p;
        Inc(p);
        if p < e then
          case p^ of
            '/':
              begin
                Inc(p);
                p := ParseName(p, e);
                if Name.Len <> 0 then
                  if (p < e) and (p^ = '>') then
                    if Depth <> 0 then
                    begin
                      Inc(p);
                      Dec(Depth);
                      if (fStackLen[Depth] = Name.Len) and
                         ((xpoDontCheckEndTagName in Options) or
                          CompareMemSmall(fBegin + fStackPos[Depth],
                            Name.Text, Name.Len)) then
                      begin
                        Kind := xtElementEnd;
                        Break;
                      end;
                      LastError := xpeWrongEndTag;
                    end
                    else
                      LastError := xpeUnexpectedEndTag
                  else
                    LastError := xpeEofEndTag
                else
                  LastError := xpeVoidEndTag;
              end;
            '!':
              begin
                Inc(p);
                if (e - p >= 2) and
                   (PWord(p)^ = Ord('-') + Ord('-') shl 8) then
                begin
                  Inc(p, 2);
                  fCur := p;
                  while (e - p >= 3) and
                        ((p^ <> '-') or
                         (p[1] <> '-') or
                         (p[2] <> '>')) do
                    Inc(p);
                  if e - p < 3 then
                  begin
                    SetOrRaiseError(xpeEofInComment);
                    Break;
                  end;
                  if xpoKeepComments in Options then
                  begin
                    Value.Buffer := fCur;
                    Value.Len := p - fCur;
                    Inc(p, 3);
                    Kind := xtComment;
                    Break;
                  end;
                  Inc(p, 3);
                  Continue;
                end;
                if (e - p >= 7) and
                   (PCardinal(p)^ = Ord('[') + Ord('C') shl 8 +
                                    Ord('D') shl 16 + Ord('A') shl 24) and
                   (PCardinal(p + 3)^ = Ord('A') + Ord('T') shl 8 +
                                        Ord('A') shl 16 + Ord('[') shl 24) then
                begin
                  Inc(p, 7);
                  Value.Buffer := p;
                  Dec(e, 3);
                  while (p <= e) and
                        ((p^ <> ']') or
                         (p[1] <> ']') or
                         (p[2] <> '>')) do
                    Inc(p);
                  if p <= e then
                  begin
                    Value.Len := p - PUtf8Char(Value.Buffer);
                    Inc(p, 3);
                    Kind := xtCData;
                    Break;
                  end;
                  LastError := xpeEofInCdata;
                end
                else
                  LastError := xpeUnsupportedMarkup;
              end;
            '?':
              begin
                Inc(p);
                p := ParseName(p, e);
                if Name.Len <> 0 then
                begin
                  fCur := p;
                  Dec(e, 2);
                  while (p <= e) and
                        ((p^ <> '?') or
                         (p[1] <> '>')) do
                    Inc(p);
                  if p <= e then
                  begin
                    if not (xpoKeepPI in Options) then
                    begin
                      Inc(e, 2);
                      Inc(p, 2);
                      Continue;
                    end;
                    Value.Buffer := fCur;
                    while (p > fCur) and
                          (p[-1] <= ' ') do
                      Dec(p);
                    Value.Len := p - PUtf8Char(Value.Buffer);
                    while p^ <= ' ' do
                      Inc(p);
                    Inc(p, 2);
                    Kind := xtPI;
                    Break;
                  end;
                  LastError := xpeEofInPi;
                end
                else
                  LastError := xpeVoidPiName;
              end;
          else
            begin
              p := ParseName(p, e);
              if Name.Len <> 0 then
                if Name.Len shr 8 = 0 then
                  if Depth < High(fStackPos) then
                  begin
                    fStackPos[Depth] := Name.Text - fBegin;
                    fStackLen[Depth] := Name.Len;
                    Inc(Depth);
                    Kind := xtElementStart;
                    Break;
                  end
                  else
                    LastError := xpeTooMuchNesting
                else
                  LastError := xpeTagNameTooLong
              else
                LastError := xpeVoidTagName;
            end;
          end
          else
            LastError := xpeEofToken;
      end
      else
      begin
        fToken := p;
        if (p^ <= ' ') and
           not (xpoKeepWhiteSpace in Options) then
        begin
          while (p < e) and (p^ <= ' ') do
            Inc(p);
          if (p < e) and (p^ = '<') then
            Continue;
          p := fToken;
        end;
        Value.Buffer := p;
        Value.Len := ByteScanIndex(pointer(p), e - p, Ord('<'));
        if Value.Len < 0 then
          Value.Len := e - p;
        Inc(p, Value.Len);
        Kind := xtText;
        Break;
      end;
    end else
    begin
      fToken := p;
      Kind := xtEof;
      if Depth = 0 then
        Break;
      LastError := xpeEofElement;
    end;
    SetOrRaiseLastError;
    Break;
  until False;
  fCur := p;
  Result := Kind;
end;

procedure TXmlParser.NameToUtf8(var result: RawUtf8);
begin
  Name.ToUtf8(result);
end;

function TXmlParser.ValueToUtf8(var Dest: RawUtf8): Boolean;
begin
  Dest := '';
  Result := ValueAppendToUtf8(Dest);
end;

function TXmlParser.ValueAppendToUtf8(var Dest: RawUtf8): Boolean;
var
  amp: PUtf8Char;
  valBuf: PUtf8Char;
  oldLen: PtrInt;
begin
  valBuf := PUtf8Char(Value.Buffer);
  if Kind in [xtCData, xtComment] then
    amp := nil
  else
    amp := PosChar(valBuf, Value.Len, '&');
  if amp = nil then
  begin
    oldLen := Length(Dest);
    SetLength(Dest, oldLen + Value.Len);
    if Value.Len > 0 then
      Move(valBuf^, Dest[oldLen + 1], Value.Len);
    Result := True;
    Exit;
  end;
  Result := AddXmlUnescape(Dest, valBuf, amp, Value.Len);
  if not Result then
    SetOrRaiseError(xpeXmlUnescapeFailed);
end;

function TXmlParser.Skip: Boolean;
var
  level: Byte;
begin
  Result := False;
  if Kind <> xtElementStart then
    Exit;
  level := Depth;
  while True do
    case ParseNext of
      xtEof,
      xtError:
        Exit;
      xtElementEnd:
        if Depth < level then
          Break;
    end;
  Result := True;
end;

function TXmlParser.Next(const ElementName: RawUtf8): Boolean;
var
  p: PUtf8Char;
begin
  p := PUtf8Char(ElementName);
  Result := (p <> nil) and Next(p, Length(ElementName));
end;

function TXmlParser.Next(ElementName: PUtf8Char; ElementLen: PtrInt): Boolean;
var
  level: Byte;
begin
  Result := False;
  if ElementLen <= 0 then
    Exit;
  level := Depth;
  while True do
    case ParseNext of
      xtEof,
      xtError:
        Exit;
      xtElementStart:
        if (Name.Len = ElementLen) and
           (Depth = level + 1) and
           CompareMem(ElementName, Name.Text, ElementLen) then
          Break;
      xtElementEnd:
        if Depth < level then
          Exit;
    end;
  Result := True;
end;

function TXmlParser.FindAny(ElementName: PUtf8Char; ElementLen: PtrInt): Boolean;
begin
  Result := False;
  if ElementLen <= 0 then
    Exit;
  while True do
    case ParseNext of
      xtEof,
      xtError:
        Exit;
      xtElementStart:
        if (Name.Len = ElementLen) and
           CompareMem(ElementName, Name.Text, ElementLen) then
          Break;
    end;
  Result := True;
end;

function TXmlParser.Find(Path: PUtf8Char; Sep: AnsiChar): Boolean;
var
  l: PtrInt;
begin
  Result := False;
  if Path = nil then
    Exit;
  Result := True;
  if Path^ = Sep then
    if Path[1] = Sep then
    begin
      Inc(Path, 2);
      l := StrLen(Path);
      if PosChar(Path, l, Sep) = nil then
        if FindAny(Path, l) then
          Exit;
      Result := False;
      Exit;
    end
    else
    begin
      Rewind;
      Inc(Path);
    end;
  repeat
    l := PosChar0(Path, Sep) - Path;
    if not Next(Path, l) then
      Break;
    Inc(Path, l);
    if Path^ = #0 then
      Exit;
    Inc(Path);
  until False;
  Result := False;
end;

function TXmlParser.ConsumeText(var Dest: RawUtf8): Boolean;
begin
  Dest := '';
  Result := False;
  while True do
    case ParseNext of
      xtEof,
      xtError:
        Exit;
      xtElementStart:
        Skip;
      xtElementEnd:
        Break;
      xtText,
      xtCData:
        ValueAppendToUtf8(Dest);
    end;
  Result := True;
end;

function TXmlParser.GetU(Path: PUtf8Char; var V: RawUtf8): Boolean;
begin
  Save;
  Result := Find(Path) and ConsumeText(V);
  Restore;
end;

function TXmlParser.GetI(Path: PUtf8Char; var V: Int64): Boolean;
var
  tmp: RawUtf8;
begin
  Save;
  Result := Find(Path) and ConsumeText(tmp);
  Restore;
  if Result then
    V := StrToInt64Def(string(tmp), 0);
end;

procedure InitXmlTables;
var
  esc: ^TAnsiCharToByte;
begin
  FillChar(XML_ESC, SizeOf(XML_ESC), 9);
  esc := @XML_ESC;
  esc^[#0]   := 1;
  esc^[#9]   := 1;
  esc^[#10]  := 2;
  esc^[#13]  := 3;
  esc^['<']  := 4;
  esc^['>']  := 5;
  esc^['&']  := 6;
  esc^['"']  := 7;
  esc^[''''] := 8;

  FillChar(XML_KIND, SizeOf(XML_KIND), 1);
  esc := @XML_KIND;
  FillChar(esc^[#33], 223, 0);
  esc^['"']  := 1;
  esc^[''''] := 1;
  esc^['/']  := 1;
  esc^['<']  := 1;
  esc^['>']  := 1;
  esc^['=']  := 1;
  esc^['?']  := 1;
end;

initialization
  InitXmlTables;

end.
