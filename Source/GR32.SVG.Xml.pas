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
 * The Original Code is SVG Image Format support for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Anders Melander <anders@melander.dk>
 *
 * Portions created by the Initial Developer are Copyright (C) 2025-2026
 * the Initial Developer. All Rights Reserved.
 *
 * The code is this unit was extracted and adapted from mORMot 2:
 *
 *   https://github.com/synopse/mORMot2/src/core/mormot.core.fmt.pas
 *   Commit SHA: 3af914ac15eab28c41411a1c449da2cf5b1e3a67
 *
 * Patches to the original code has been marked with [*].
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

{$define PATCHED} // [*]

uses
  SysUtils,
  GR32.SVG.Utf8;

type
  Ucs4CodePoint = Cardinal;

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
    xtPI,
    xtDocType);

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
    xpeEofInDocType,
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
    xpoKeepDocType,
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
    'unexpected end of input within DOCTYPE',    // xpeEofInDocType
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

implementation

uses
  AnsiStrings;

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
// [*] Patched to preserves 'xmlns' as the attribute name when encountering 'xmlns:<prefix>',
// preventing 'xmlns:svg' from being stripped into attribute name 'svg'.
{$if defined(PATCHED)}
var
  isXmlns: Boolean; // [*]
{$ifend}
begin
  Name.Text := p;
{$if defined(PATCHED)}
  isXmlns := False;
{$ifend}
  if xpoStripNamespacePrefix in Options then
    while (p < e) and
          ({$ifdef FPCX86NOTPIC} XML_KIND {$else} fTab^ {$endif}[p^] = 0) do
    begin
      if p^ = ':' then
      // [*] :
{$if defined(PATCHED)}
      begin
        if (p - Name.Text = 5) and
           (Name.Text[0] = 'x') and (Name.Text[1] = 'm') and (Name.Text[2] = 'l') and
           (Name.Text[3] = 'n') and (Name.Text[4] = 's') then
        begin
          isXmlns := True;
        end else
          Name.Text := p + 1;
      end;
{$else}
        Name.Text := p + 1;
{$ifend}
      Inc(p);
    end
  else
    while (p < e) and
          ({$ifdef FPCX86NOTPIC} XML_KIND {$else} fTab^ {$endif}[p^] = 0) do
      Inc(p);

{$if defined(PATCHED)}
  if isXmlns then
    Name.Len := 5
  else
    Name.Len := p - Name.Text;
{$else}
  Name.Len := p - Name.Text;
{$ifend}

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
  quoteCh: AnsiChar;
  inSubset: Boolean;
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
                else if (e - p >= 7) and
                   ((p[0] = 'D') or (p[0] = 'd')) and
                   ((p[1] = 'O') or (p[1] = 'o')) and
                   ((p[2] = 'C') or (p[2] = 'c')) and
                   ((p[3] = 'T') or (p[3] = 't')) and
                   ((p[4] = 'Y') or (p[4] = 'y')) and
                   ((p[5] = 'P') or (p[5] = 'p')) and
                   ((p[6] = 'E') or (p[6] = 'e')) then
                begin
                  Inc(p, 7);
                  fCur := p;
                  quoteCh := #0;
                  inSubset := False;
                  while p < e do
                  begin
                    if inSubset then
                    begin
                      if quoteCh <> #0 then
                      begin
                        if p^ = quoteCh then
                          quoteCh := #0;
                      end
                      else if p^ in ['"', ''''] then
                        quoteCh := p^
                      else if p^ = ']' then
                        inSubset := False;
                    end
                    else
                    begin
                      if quoteCh <> #0 then
                      begin
                        if p^ = quoteCh then
                          quoteCh := #0;
                      end
                      else if p^ in ['"', ''''] then
                        quoteCh := p^
                      else if p^ = '[' then
                        inSubset := True
                      else if p^ = '>' then
                        Break;
                    end;
                    Inc(p);
                  end;
                  if (p >= e) or (p^ <> '>') then
                  begin
                    LastError := xpeEofInDocType;
                    Break;
                  end;

                  if xpoKeepDocType in Options then
                  begin
                    Value.Buffer := fCur;
                    Value.Len := p - fCur;
                    Inc(p);
                    Kind := xtDocType;
                    Break;
                  end;
                  Inc(p);
                  Continue;
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
  if Kind in [xtCData, xtComment, xtDocType] then
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
      l := AnsiStrings.StrLen(Path);
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
