unit GR32.Text.Unicode;

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
 * The Original Code is Text Layout for Graphics32
 *
 * The Initial Developer of the Original Code is Anders Melander
 *
 * Portions created by the Initial Developer are Copyright (C) 2025
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

uses
  GR32;

//------------------------------------------------------------------------------
//
//              TCodePoint
//
//------------------------------------------------------------------------------
// UCS4 = UTF-32
// A Unicode 2.0 codepoint - 32 bits wide
//------------------------------------------------------------------------------
// The Unicode® Standard, Version 15.0 – Core Specification, Appendix C
// Relationship to ISO/IEC 10646
// Section C.2 Encoding Forms in ISO/IEC 10646
// UCS-4 stands for “Universal Character Set coded in 4 octets.” It is now
// treated simply as a synonym for UTF-32, and is considered the canonical form
// for representation of characters in [ISO/IEC] 10646.
//------------------------------------------------------------------------------
type
  TCodePoint = Cardinal;

  TCodePoints = TArray<TCodePoint>;


//------------------------------------------------------------------------------
//
//              PascalTypeUnicode namespace
//
//------------------------------------------------------------------------------
type
  Graphics32Unicode = record
    const
      UCS4Replacement: Char     = #$FFFD;
      UCS4ReplacementCodePoint  = $0000FFFD;
      MaximumUCS2               = $0000FFFF;
      MaximumUTF16              = $0010FFFF; // Max Unicode code point
      MaximumUCS4               = $7FFFFFFF;

      MaxHighSurrogate          = $DBFF;
      MaxLowSurrogate           = $DFFF;
      MaxSurrogate              = $DFFF;
      MinHighSurrogate          = $D800;
      MinLowSurrogate           = $DC00;
      MinSurrogate              = $D800;

    const
      cpEmSpace                 = $00002003;
      cpDottedCircle            = $000025CC;
      cpReplacementCharacter    = $0000FFFD;

    //------------------------------------------------------------------------------
    //
    //              String conversion
    //
    //------------------------------------------------------------------------------
    // Convert between 16-bit Unicode (native char/string) and 32-bit Unicode
    //------------------------------------------------------------------------------
    class function UTF16ToUTF32(const AText: string; var Index: integer): TCodePoint; overload; static;
    class function UTF16ToUTF32(const AText: string): TCodePoints; overload; static;

    class function UTF32ToUTF16(const ACodePoints: TCodePoints): string; static;
  end;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------


implementation


//------------------------------------------------------------------------------
//
//              String conversion
//
//------------------------------------------------------------------------------
class function Graphics32Unicode.UTF16ToUTF32(const AText: string; var Index: integer): TCodePoint;
var
  CodePoint: TCodePoint;
begin
  if (Index > Length(AText)) then
    Exit(0);

  Result := Ord(AText[Index]);
  Inc(Index);

  if (Result and $FC00 = MinHighSurrogate) then
  begin
    if (Index <= Length(AText)) then
    begin
      CodePoint := Ord(AText[Index]);

      if (Word(CodePoint) and $FC00 = MinLowSurrogate) then
      begin
        Result := (Cardinal(Cardinal(Result and $03FF) shl 10) or Cardinal(CodePoint and $03FF)) + $10000;
        Inc(Index);
      end else
        Result := cpReplacementCharacter;

    end else
      Result := cpReplacementCharacter;
  end;
end;

//------------------------------------------------------------------------------

class function Graphics32Unicode.UTF16ToUTF32(const AText: string): TCodePoints;
var
  Index, i: integer;
begin
  Index := 1;
  i := 0;

  SetLength(Result, Length(AText));

  while (Index <= Length(AText)) do
  begin
    Result[i] := UTF16ToUTF32(AText, Index);
    inc(i);
  end;

  SetLength(Result, i);
end;

//------------------------------------------------------------------------------

class function Graphics32Unicode.UTF32ToUTF16(const ACodePoints: TCodePoints): string;
var
  i: integer;
  CodePoint: TCodePoint;
  Surrogate: Cardinal;
begin
  Result := '';

  i := Length(ACodePoints);
  for CodePoint in ACodePoints do
    if (CodePoint > MaximumUCS2) and (CodePoint <= MaximumUTF16) then
      Inc(i);

  SetLength(Result, i);

  i := 1;
  for CodePoint in ACodePoints do
  begin

    if CodePoint < MinHighSurrogate then
      Result[i] := Char(CodePoint)
    else
    if CodePoint <= MaxLowSurrogate then
      Result[i] := UCS4Replacement
    else
    if CodePoint <= $FFFD then
      Result[i] := Char(CodePoint)
    else
    if CodePoint <= $FFFF then
      Result[i] := UCS4Replacement
    else
    if CodePoint <= MaximumUTF16 then
    begin
      Surrogate := CodePoint - $10000;
      Result[i] := Char((Surrogate shr 10) or MinHighSurrogate);
      Inc(i);
      Result[i] := Char((Surrogate and $03FF) or MinLowSurrogate);
    end else
      Result[i] := UCS4Replacement;

    Inc(i);
  end;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

end.
