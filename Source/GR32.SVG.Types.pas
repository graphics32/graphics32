unit GR32.SVG.Types;

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
 * Portions created by the Initial Developer are Copyright (C) 2008-2024
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

uses
  SysUtils, Classes, Math,
  GR32,
  GR32_Transforms,
  GR32_Math,
  GR32_LowLevel,
  GR32.SVG.Utf8;

var
  SvgFormatSettings: TFormatSettings;

// HSLtoRGB overload for float Alpha in the range [0.0..1.0]
function HSLtoRGB(H, S, L, A: Single): TColor32;

function SvgColorNameToColor(const AName: TValuePUtf8Char; ADefault: TColor32): TColor32;

type
  TSvgUnitType = (
    suPx,
    suPt,
    suMm,
    suCm,
    suIn,
    suPc,
    suPercent,
    suEm,
    suEx
  );

  TSvgLength = record
    Value: Single;
    UnitType: TSvgUnitType;
    function ToPixels(const ARefSize: Single = 0; const ADpi: Single = 96.0; const AFontSize: Single = 16.0): Single;
    class function Create(AValue: Single; AUnit: TSvgUnitType = suPx): TSvgLength; static;
    class function Parse(const AStr: UTF8String): TSvgLength; static;
  end;

  TSvgColor = record
    Color: TColor32;
    IsNone: Boolean;
    IsCurrentColor: Boolean;
    class function Create(AColor: TColor32): TSvgColor; static;
    class function None: TSvgColor; static;
    class function CurrentColor: TSvgColor; static;
    class function Parse(const AStr: UTF8String): TSvgColor; overload; static;
    class function Parse(AColorStr: TValuePUtf8Char): TSvgColor; overload; static;
  end;

  TSvgAlign = (
    saNone,
    saXMinYMin,
    saXMidYMin,
    saXMaxYMin,
    saXMinYMid,
    saXMidYMid,
    saXMaxYMid,
    saXMinYMax,
    saXMidYMax,
    saXMaxYMax
  );

  TSvgMeetOrSlice = (
    msMeet,
    msSlice
  );

  TSvgPreserveAspectRatio = record
    Align: TSvgAlign;
    MeetOrSlice: TSvgMeetOrSlice;
    class function Default: TSvgPreserveAspectRatio; static;
    class function Parse(const AStr: UTF8String): TSvgPreserveAspectRatio; static;
  end;

  { TFloatMatrixHelper }
  TFloatMatrixHelper = record
    // Beware that Matrix is Row-Major!
    Matrix: TFloatMatrix;

    class operator Multiply(const Left, Right: TFloatMatrixHelper): TFloatMatrixHelper; overload; {$IF DEFINED(StaticOperators)}static;{$IFEND}
    class operator Multiply(const Left: TFloatMatrixHelper; const Right: TFloatMatrix): TFloatMatrixHelper; overload; {$IF DEFINED(StaticOperators)}static;{$IFEND}
    class operator Multiply(const Left: TFloatMatrix; const Right: TFloatMatrixHelper): TFloatMatrix; overload; {$IF DEFINED(StaticOperators)}static;{$IFEND}

    procedure Translate(Dx, Dy: TFloat);
    procedure Rotate(Alpha: TFloat); overload;
    procedure Rotate(Cx, Cy, Alpha: TFloat); overload;
    procedure Skew(Fx, Fy: TFloat);
    procedure Scale(Sx, Sy: TFloat); overload;
    procedure Scale(Value: TFloat); overload;
    function TransformPoint(const P: TFloatPoint): TFloatPoint;
  end;

  TSvgViewBox = record
    X: Single;
    Y: Single;
    Width: Single;
    Height: Single;
    IsDefined: Boolean;
    function IsValid: Boolean;
    class function Create(AX, AY, AWidth, AHeight: Single): TSvgViewBox; static;
    class function Parse(const AStr: string): TSvgViewBox; static;
    function GetTransform(const ATargetRect: TFloatRect; const AAspect: TSvgPreserveAspectRatio): TFloatMatrix;
  end;

type
  TSvgKeywordDictionary<T> = record
  private type
    TKeyword = record
      Keyword: AnsiString;
      Value: T;
    end;
  private
    FLengths: TArray<integer>;                  // Array of Keywords[] indices, indexed by keyword length.
                                                // 0 means no keywords for that length.

    FKeywords: TArray<TArray<TKeyword>>;        // Sparse array of keyword arrays.
                                                // All keywords within a keyword array has the same
                                                // length and are sorted alphabetically.
  public
    // Add a keyword to the dictionary
    procedure Add(const AKeyword: AnsiString; AValue: T);
    // Lookup a keyword in the dictionary. Return Value if found, Default(T) otherwise.
    function Lookup(const AKeyword: TValuePUtf8Char; var AValue: T): boolean; overload;
    function Lookup(const AKeyword: TValuePUtf8Char): T; overload;
  end;

implementation

uses
  AnsiStrings;

function HSLtoRGB(H, S, L, A: Single): TColor32;
begin
  Result := GR32.HSLtoRGB(
    EnsureRange(H, 0.0, 1.0),
    EnsureRange(S, 0.0, 1.0),
    EnsureRange(L, 0.0, 1.0),
    Clamp(Round(A * 255.0))
  );
end;

procedure TSvgKeywordDictionary<T>.Add(const AKeyword: AnsiString; AValue: T);
var
  Len: integer;
  Index: integer;
begin
  Len := Length(AKeyword);
  if (Len = 0) then
    exit;

  // Make room in length index
  if (Len >= High(FLengths)) then
  begin
    if (Len < 16) then
      SetLength(FLengths, 16)
    else
      SetLength(FLengths, Len * 2);
  end;

  // Get the keyword list for this length
  Index := FLengths[Len-1];

  // Do we have a list allocated for this length?
  if (Index = 0) then
  begin
    // Allocate a new keyword list
    Index := Length(FKeywords)+1; // 0=no entry, so first index is 1
    SetLength(FKeywords, Index);
    // Update the index
    FLengths[Len-1] := Index;
  end;

  Dec(Index); // Normalize index

  // Insert the new keyword in the keyword list
  SetLength(FKeywords[Index], Length(FKeywords[Index]) + 1);
  FKeywords[Index, High(FKeywords[Index])].Keyword := AKeyword;
  FKeywords[Index, High(FKeywords[Index])].Value := AValue;
end;

function TSvgKeywordDictionary<T>.Lookup(const AKeyword: TValuePUtf8Char): T;
begin
  if (not Lookup(AKeyword, Result)) then
    Result := Default(T);
end;

function TSvgKeywordDictionary<T>.Lookup(const AKeyword: TValuePUtf8Char; var AValue: T): boolean;
var
  Index: integer;
  i: integer;
begin
  Result := False;
  if (AKeyword.Len = 0) then
    exit;

  if (AKeyword.Len >= High(FLengths)) then
    exit;

  // Get the keyword list for this length
  Index := FLengths[AKeyword.Len-1];

  // Do we have a list allocated for this length?
  if (Index = 0) then
    exit;

  Dec(Index); // Normalize index

  // Find the keyword in the keyword list
  for i := 0 to High(FKeywords[Index]) do
    if (AKeyword.CompareText(FKeywords[Index, i].Keyword)) then
    begin
      AValue := FKeywords[Index, i].Value;
      Exit(True);
    end;
end;

type
  TColorName = record
    Name: AnsiString;
    Color: TColor32;
  end;

var
  SvgColorNameDictionary: TSvgKeywordDictionary<TColor32>;

const
  sColorNames: array[0..147] of TColorName = (
    (Name: 'red'; Color: clRed32),
    (Name: 'tan'; Color: clTan32),
    (Name: 'aqua'; Color: clAqua32),
    (Name: 'blue'; Color: clBlue32),
    (Name: 'cyan'; Color: clAqua32),
    (Name: 'gold'; Color: clGold32),
    (Name: 'gray'; Color: clGray32),
    (Name: 'grey'; Color: clGrey32),
    (Name: 'lime'; Color: clLime32),
    (Name: 'navy'; Color: clNavy32),
    (Name: 'peru'; Color: clPeru32),
    (Name: 'pink'; Color: clPink32),
    (Name: 'plum'; Color: clPlum32),
    (Name: 'snow'; Color: clSnow32),
    (Name: 'teal'; Color: clTeal32),
    (Name: 'azure'; Color: clAzure32),
    (Name: 'beige'; Color: clBeige32),
    (Name: 'black'; Color: clBlack32),
    (Name: 'brown'; Color: clBrown32),
    (Name: 'coral'; Color: clCoral32),
    (Name: 'green'; Color: clGreen32),
    (Name: 'ivory'; Color: clIvory32),
    (Name: 'khaki'; Color: clKhaki32),
    (Name: 'linen'; Color: clLinen32),
    (Name: 'olive'; Color: clOlive32),
    (Name: 'wheat'; Color: clWheat32),
    (Name: 'white'; Color: clWhite32),
    (Name: 'bisque'; Color: clBisque32),
    (Name: 'indigo'; Color: clIndigo32),
    (Name: 'maroon'; Color: clMaroon32),
    (Name: 'orange'; Color: clOrange32),
    (Name: 'orchid'; Color: clOrchid32),
    (Name: 'purple'; Color: clPurple32),
    (Name: 'salmon'; Color: clSalmon32),
    (Name: 'sienna'; Color: clSienna32),
    (Name: 'silver'; Color: clSilver32),
    (Name: 'tomato'; Color: clTomato32),
    (Name: 'violet'; Color: clViolet32),
    (Name: 'yellow'; Color: clYellow32),
    (Name: 'crimson'; Color: clCrimson32),
    (Name: 'darkred'; Color: clDarkRed32),
    (Name: 'dimgray'; Color: clDimGray32),
    (Name: 'dimgrey'; Color: clDimGray32),
    (Name: 'fuchsia'; Color: clFuchsia32),
    (Name: 'hotpink'; Color: clHotPink32),
    (Name: 'magenta'; Color: clFuchsia32),
    (Name: 'oldlace'; Color: clOldLace32),
    (Name: 'skyblue'; Color: clSkyblue32),
    (Name: 'thistle'; Color: clThistle32),
    (Name: 'cornsilk'; Color: clCornSilk32),
    (Name: 'darkblue'; Color: clDarkBlue32),
    (Name: 'darkcyan'; Color: clDarkCyan32),
    (Name: 'darkgray'; Color: clDarkGray32),
    (Name: 'darkgrey'; Color: clDarkGrey32),
    (Name: 'deeppink'; Color: clDeepPink32),
    (Name: 'honeydew'; Color: clHoneyDew32),
    (Name: 'lavender'; Color: clLavender32),
    (Name: 'moccasin'; Color: clMoccasin32),
    (Name: 'seagreen'; Color: clSeaGreen32),
    (Name: 'seashell'; Color: clSeaShell32),
    (Name: 'aliceblue'; Color: clAliceBlue32),
    (Name: 'burlywood'; Color: clBurlyWood32),
    (Name: 'cadetblue'; Color: clCadetblue32),
    (Name: 'chocolate'; Color: clChocolate32),
    (Name: 'darkgreen'; Color: clDarkGreen32),
    (Name: 'darkkhaki'; Color: clDarkKhaki32),
    (Name: 'firebrick'; Color: clFireBrick32),
    (Name: 'gainsboro'; Color: clGainsBoro32),
    (Name: 'goldenrod'; Color: clGoldenRod32),
    (Name: 'indianred'; Color: clIndianRed32),
    (Name: 'lawngreen'; Color: clLawnGreen32),
    (Name: 'lightblue'; Color: clLightBlue32),
    (Name: 'lightcyan'; Color: clLightCyan32),
    (Name: 'lightgray'; Color: clLightGray32),
    (Name: 'lightgrey'; Color: clLightGrey32),
    (Name: 'lightpink'; Color: clLightPink32),
    (Name: 'limegreen'; Color: clLimeGreen32),
    (Name: 'mintcream'; Color: clMintCream32),
    (Name: 'mistyrose'; Color: clMistyRose32),
    (Name: 'olivedrab'; Color: clOliveDrab32),
    (Name: 'orangered'; Color: clOrangeRed32),
    (Name: 'palegreen'; Color: clPaleGreen32),
    (Name: 'peachpuff'; Color: clPeachPuff32),
    (Name: 'rosybrown'; Color: clRosyBrown32),
    (Name: 'royalblue'; Color: clRoyalBlue32),
    (Name: 'slateblue'; Color: clSlateBlue32),
    (Name: 'slategray'; Color: clSlateGray32),
    (Name: 'slategrey'; Color: clSlateGrey32),
    (Name: 'steelblue'; Color: clSteelblue32),
    (Name: 'turquoise'; Color: clTurquoise32),
    (Name: 'aquamarine'; Color: clAquamarine32),
    (Name: 'blueviolet'; Color: clBlueViolet32),
    (Name: 'chartreuse'; Color: clChartReuse32),
    (Name: 'darkorange'; Color: clDarkOrange32),
    (Name: 'darkorchid'; Color: clDarkOrchid32),
    (Name: 'darksalmon'; Color: clDarkSalmon32),
    (Name: 'darkviolet'; Color: clDarkViolet32),
    (Name: 'dodgerblue'; Color: clDodgerBlue32),
    (Name: 'ghostwhite'; Color: clGhostWhite32),
    (Name: 'lightcoral'; Color: clLightCoral32),
    (Name: 'lightgreen'; Color: clLightGreen32),
    (Name: 'mediumblue'; Color: clMediumBlue32),
    (Name: 'papayawhip'; Color: clPapayaWhip32),
    (Name: 'powderblue'; Color: clPowderBlue32),
    (Name: 'sandybrown'; Color: clSandyBrown32),
    (Name: 'whitesmoke'; Color: clWhitesmoke32),
    (Name: 'darkmagenta'; Color: clDarkMagenta32),
    (Name: 'deepskyblue'; Color: clDeepSkyBlue32),
    (Name: 'floralwhite'; Color: clFloralWhite32),
    (Name: 'forestgreen'; Color: clForestGreen32),
    (Name: 'greenyellow'; Color: clGreenYellow32),
    (Name: 'lightsalmon'; Color: clLightSalmon32),
    (Name: 'lightyellow'; Color: clLightYellow32),
    (Name: 'navajowhite'; Color: clNavajoWhite32),
    (Name: 'saddlebrown'; Color: clSaddleBrown32),
    (Name: 'springgreen'; Color: clSpringgreen32),
    (Name: 'transparent'; Color: clNone32),
    (Name: 'yellowgreen'; Color: clYellowgreen32),
    (Name: 'antiquewhite'; Color: clAntiqueWhite32),
    (Name: 'darkseagreen'; Color: clDarkSeaGreen32),
    (Name: 'lemonchiffon'; Color: clLemonChiffon32),
    (Name: 'lightskyblue'; Color: clLightSkyblue32),
    (Name: 'mediumorchid'; Color: clMediumOrchid32),
    (Name: 'mediumpurple'; Color: clMediumPurple32),
    (Name: 'midnightblue'; Color: clMidnightBlue32),
    (Name: 'darkgoldenrod'; Color: clDarkGoldenRod32),
    (Name: 'darkslateblue'; Color: clDarkSlateBlue32),
    (Name: 'darkslategray'; Color: clDarkSlateGray32),
    (Name: 'darkslategrey'; Color: clDarkSlateGrey32),
    (Name: 'darkturquoise'; Color: clDarkTurquoise32),
    (Name: 'lavenderblush'; Color: clLavenderBlush32),
    (Name: 'lightseagreen'; Color: clLightSeagreen32),
    (Name: 'palegoldenrod'; Color: clPaleGoldenRod32),
    (Name: 'paleturquoise'; Color: clPaleTurquoise32),
    (Name: 'palevioletred'; Color: clPaleVioletred32),
    (Name: 'blanchedalmond'; Color: clBlancheDalmond32),
    (Name: 'cornflowerblue'; Color: clCornFlowerBlue32),
    (Name: 'darkolivegreen'; Color: clDarkOliveGreen32),
    (Name: 'lightslategray'; Color: clLightSlategray32),
    (Name: 'lightslategrey'; Color: clLightSlategrey32),
    (Name: 'lightsteelblue'; Color: clLightSteelblue32),
    (Name: 'mediumseagreen'; Color: clMediumSeaGreen32),
    (Name: 'mediumslateblue'; Color: clMediumSlateBlue32),
    (Name: 'mediumturquoise'; Color: clMediumTurquoise32),
    (Name: 'mediumvioletred'; Color: clMediumVioletRed32),
    (Name: 'mediumaquamarine'; Color: clMediumAquamarine32),
    (Name: 'mediumspringgreen'; Color: clMediumSpringGreen32),
    (Name: 'lightgoldenrodyellow'; Color: clLightGoldenRodYellow32)
  );

function SvgColorNameToColor(const AName: TValuePUtf8Char; ADefault: TColor32): TColor32;
begin
  if (not SvgColorNameDictionary.Lookup(AName, Result)) then
    Result := ADefault;
end;


{ TSvgLength }

class function TSvgLength.Create(AValue: Single; AUnit: TSvgUnitType): TSvgLength;
begin
  Result.Value := AValue;
  Result.UnitType := AUnit;
end;

function TSvgLength.ToPixels(const ARefSize, ADpi, AFontSize: Single): Single;
begin
  case UnitType of
    suPx: Result := Value;
    suPt: Result := Value * (ADpi / 72.0);
    suMm: Result := Value * (ADpi / 25.4);
    suCm: Result := Value * (ADpi / 2.54);
    suIn: Result := Value * ADpi;
    suPc: Result := Value * (ADpi / 6.0);
    suPercent: Result := Value * 0.01 * ARefSize;
    suEm: Result := Value * AFontSize;
    suEx: Result := Value * AFontSize * 0.5;
  else
    Result := Value;
  end;
end;

class function TSvgLength.Parse(const AStr: UTF8String): TSvgLength;
var
  p, pSuffix: PUtf8Char;
(*
  s, numStr, unitStr: UTF8String;
  i, len: Integer;
*)
  Value: Double;
begin
  // Zero-copy value/unit parser

  Result.UnitType := suPx;
  Result.Value := 0;

  p := Pointer(AStr);
  if (p = nil) then
    exit;

  // Skip leading spaces
  while (p^ <> #0) and (p^ = #32) do
    Inc(p);

  if (p^ = #0) then
    exit;

  // Find end of value = start of suffix
  pSuffix := p;
  while (pSuffix^ <> #0) and ((pSuffix^ in ['0'..'9', '.', '-', '+']) or ((pSuffix^ in ['e', 'E']) and not (pSuffix[1] in ['m', 'M', 'x', 'X']))) do
    Inc(pSuffix);

  // Convert value
  if GetExtended(p, pSuffix-p, Value) then
    Result.Value := Value;

  // Skip leading spaces
  while (pSuffix^ <> #0) and (pSuffix^ = #32) do
    Inc(pSuffix);

  // Match and skip suffix
  case pSuffix^ of
    #0: Result.UnitType := suPx;
    '%': begin Result.UnitType := suPercent; Inc(pSuffix, 1); end;
    'c':
      case pSuffix[1] of
        'm': begin Result.UnitType := suCm; Inc(pSuffix, 2); end;
      end;
    'e':
      case pSuffix[1] of
        'm': begin Result.UnitType := suEm; Inc(pSuffix, 2); end;
        'x': begin Result.UnitType := suEx; Inc(pSuffix, 2); end;
      end;
    'm':
      case pSuffix[1] of
        'm': begin Result.UnitType := suMm; Inc(pSuffix, 2); end;
      end;
    'i':
      case pSuffix[1] of
        'n': begin Result.UnitType := suIn; Inc(pSuffix, 2); end;
      end;
    'p':
      case pSuffix[1] of
        'c': begin Result.UnitType := suPc; Inc(pSuffix, 2); end;
        't': begin Result.UnitType := suPt; Inc(pSuffix, 2); end;
        'x': begin Result.UnitType := suPx; Inc(pSuffix, 2); end;
      end;
  end;

  // Skip trailing spaces
  while (pSuffix^ <> #0) and (pSuffix^ = #32) do
    Inc(pSuffix);

  // If we are not at end of string, then we have junk and we discard the suffix
  if (pSuffix^ <> #0) then
    Result.UnitType := suPx;
end;

{ TSvgColor }

class function TSvgColor.Create(AColor: TColor32): TSvgColor;
begin
  Result.Color := AColor;
  Result.IsNone := False;
  Result.IsCurrentColor := False;
end;

class function TSvgColor.None: TSvgColor;
begin
  Result.Color := $00000000;
  Result.IsNone := True;
  Result.IsCurrentColor := False;
end;


class function TSvgColor.CurrentColor: TSvgColor;
begin
  Result.Color := clBlack32;
  Result.IsNone := False;
  Result.IsCurrentColor := True;
end;

class function TSvgColor.Parse(AColorStr: TValuePUtf8Char): TSvgColor;

  function ParseHexByte(Twins: boolean = False): Byte;
  begin
    // First digit
    case AColorStr.Text^ of
      '0'..'9': Result := Ord(AColorStr.Text^) - Ord('0');
      'a'..'f': Result := Ord(AColorStr.Text^) - Ord('a') + 10;
      'A'..'F': Result := Ord(AColorStr.Text^) - Ord('A') + 10;
    else
      Exit(0);
    end;
    AColorStr.Skip;

    if Twins then
    begin
      Result := Result shl 4 + Result;
      exit;
    end;

    if (AColorStr.Len = 0) then
      exit;
    // Optional second digit
    case AColorStr.Text^ of
      '0'..'9': Result := Result shl 4 + Ord(AColorStr.Text^) - Ord('0');
      'a'..'f': Result := Result shl 4 + Ord(AColorStr.Text^) - Ord('a') + 10;
      'A'..'F': Result := Result shl 4 + Ord(AColorStr.Text^) - Ord('A') + 10;
    else
      exit;
    end;
    AColorStr.Skip;
  end;

var
  HasRGB: boolean;
  HasRGBA: boolean;
  n: Double;
//  sRGB: TValuePUtf8Char;
  r, g, b, a: Byte;
begin
  // Trim
  AColorStr.Trim;

  if (AColorStr.Len = 0) then
    Exit(None);

  if (AColorStr.CompareText('none')) then
    Exit(None);

  if (AColorStr.CompareText('currentcolor')) then
    Exit(CurrentColor);

  if (AColorStr.Text^ = '#') then
  begin
    AColorStr.Skip;

    case AColorStr.Len of
      3:
        begin
          r := ParseHexByte(True);
          g := ParseHexByte(True);
          b := ParseHexByte(True);
          Exit(Create(Color32(r, g, b, 255)));
        end;

      6:
        begin
          r := ParseHexByte;
          g := ParseHexByte;
          b := ParseHexByte;
          Exit(Create(Color32(r, g, b, 255)));
        end;

      8:
        begin
          r := ParseHexByte;
          g := ParseHexByte;
          b := ParseHexByte;
          a := ParseHexByte;
          Exit(Create(Color32(r, g, b, a)));
        end;
    end;
    Exit(None); // Invalid
  end;

  // Smallest possible 'rgb' string is rgb(0,0,0) -> length=10
  // Must be rgb(...) or rgba(...)
  if (AColorStr.Len >= 10) and (AColorStr.Text[AColorStr.Len-1] = ')') then
  begin
    HasRGBA := AColorStr.StartsText('rgba(', True);
    HasRGB := HasRGBA or AColorStr.StartsText('rgb(', True);

    if (HasRGB) then
    begin
      AColorStr.Trim;
      r := AColorStr.ToCardinalAndSkip;
      AColorStr.Trim([' ', ',']);
      g := AColorStr.ToCardinalAndSkip;
      AColorStr.Trim([' ', ',']);
      b := AColorStr.ToCardinalAndSkip;
      if HasRGBA then
      begin
        AColorStr.Trim([' ', ',']);
        GetExtended(AColorStr.Text, AColorStr.Len, n);
        a := Clamp(Round(n * 255.0));
      end else
        a := 255;
      Result := Create(Color32(r, g, b, a));
      exit;
    end;
  end;

  Result := Create(SvgColorNameToColor(AColorStr, clBlack32));
end;

class function TSvgColor.Parse(const AStr: UTF8String): TSvgColor;
var
  Value: TValuePUtf8Char;
begin
  Value.Text := pointer(AStr);
  Value.Len := Length(AStr);
  Result := Parse(Value);
end;

{ TSvgPreserveAspectRatio }

class function TSvgPreserveAspectRatio.Default: TSvgPreserveAspectRatio;
begin
  Result.Align := saXMidYMid;
  Result.MeetOrSlice := msMeet;
end;

class function TSvgPreserveAspectRatio.Parse(const AStr: UTF8String): TSvgPreserveAspectRatio;
type
  TChars = array[0..MaxInt-1] of AnsiChar;
  PChars = ^TChars;
var
  n: integer;
  s: UTF8String;
  p: PChars;
begin
  Result := Default;

  p := PChars(@AStr[1]);
  n := 0;
  while (p[n] <> #0) and (p[n] <> ' ') do
    Inc(n);

  if (n > 0) then
  begin
    SetString(s, PAnsiChar(p), n);
    s :=  AnsiStrings.LowerCase(s);

    if s = 'none' then Result.Align := saNone
    else if s = 'xminymin' then Result.Align := saXMinYMin
    else if s = 'xmidymin' then Result.Align := saXMidYMin
    else if s = 'xmaxymin' then Result.Align := saXMaxYMin
    else if s = 'xminymid' then Result.Align := saXMinYMid
    else if s = 'xmidymid' then Result.Align := saXMidYMid
    else if s = 'xmaxymid' then Result.Align := saXMaxYMid
    else if s = 'xminymax' then Result.Align := saXMinYMax
    else if s = 'xmidymax' then Result.Align := saXMidYMax
    else if s = 'xmaxymax' then Result.Align := saXMaxYMax;

    while (p[n] <> #0) and (p[n] = ' ') do
      Inc(n);

    p := PChars(@p[n]);
    n := 0;
    while (p[n] <> #0) and (p[n] <> ' ') do
      Inc(n);

    if (n > 0) then
    begin
      SetString(s, PAnsiChar(p), n);
      s :=  AnsiStrings.LowerCase(s);

      if s = 'slice' then
        Result.MeetOrSlice := msSlice
      else
      if s = 'meet' then
        Result.MeetOrSlice := msMeet;
    end;

  end;
end;

{ TFloatMatrixHelper }

class operator TFloatMatrixHelper.Multiply(const Left, Right: TFloatMatrixHelper): TFloatMatrixHelper;
begin
  Result.Matrix := Mult(Left.Matrix, Right.Matrix);
end;

class operator TFloatMatrixHelper.Multiply(const Left: TFloatMatrixHelper; const Right: TFloatMatrix): TFloatMatrixHelper;
begin
  Result.Matrix := Mult(Left.Matrix, Right);
end;

class operator TFloatMatrixHelper.Multiply(const Left: TFloatMatrix; const Right: TFloatMatrixHelper): TFloatMatrix;
begin
  Result := Mult(Left, Right.Matrix);
end;

procedure TFloatMatrixHelper.Rotate(Alpha: TFloat);
var
  S, C: TFloat;
  M: TFloatMatrix;
begin
  Alpha := DegToRad(Alpha);
  GR32_Math.SinCos(Alpha, S, C);

  M := IdentityMatrix;
  M[0, 0] := C;   M[1, 0] := -S;
  M[0, 1] := S;   M[1, 1] := C;
  Matrix := Mult(M, Matrix);
end;

procedure TFloatMatrixHelper.Rotate(Cx, Cy, Alpha: TFloat);
var
  S, C: TFloat;
  M: TFloatMatrix;
begin
  if (Cx <> 0) or (Cy <> 0) then
    Translate(-Cx, -Cy);

  Alpha := DegToRad(Alpha);
  GR32_Math.SinCos(Alpha, S, C);

  M := IdentityMatrix;
  M[0, 0] := C;   M[1, 0] := -S;
  M[0, 1] := S;   M[1, 1] := C;
  Matrix := Mult(M, Matrix);

  if (Cx <> 0) or (Cy <> 0) then
    Translate(Cx, Cy);
end;

procedure TFloatMatrixHelper.Scale(Sx, Sy: TFloat);
var
  M: TFloatMatrix;
begin
  M := IdentityMatrix;
  M[0, 0] := Sx;
  M[1, 1] := Sy;
  Matrix := Mult(M, Matrix);
end;

procedure TFloatMatrixHelper.Scale(Value: TFloat);
var
  M: TFloatMatrix;
begin
  M := IdentityMatrix;
  M[0, 0] := Value;
  M[1, 1] := Value;
  Matrix := Mult(M, Matrix);
end;

procedure TFloatMatrixHelper.Skew(Fx, Fy: TFloat);
var
  M: TFloatMatrix;
begin
  M := IdentityMatrix;
  M[1, 0] := Fx;
  M[0, 1] := Fy;
  Matrix := Mult(M, Matrix);
end;

procedure TFloatMatrixHelper.Translate(Dx, Dy: TFloat);
var
  M: TFloatMatrix;
begin
  M := IdentityMatrix;
  M[2, 0] := Dx;
  M[2, 1] := Dy;
  Matrix := Mult(M, Matrix);
end;

function TFloatMatrixHelper.TransformPoint(const P: TFloatPoint): TFloatPoint;
var
  vIn, vOut: TVector3f;
begin
  vIn[0] := P.X;
  vIn[1] := P.Y;
  vIn[2] := 1.0;
  vOut := VectorTransform(Matrix, vIn);
  Result.X := vOut[0];
  Result.Y := vOut[1];
end;

{ TSvgViewBox }

class function TSvgViewBox.Create(AX, AY, AWidth, AHeight: Single): TSvgViewBox;
begin
  Result.X := AX;
  Result.Y := AY;
  Result.Width := AWidth;
  Result.Height := AHeight;
  Result.IsDefined := True;
end;

function TSvgViewBox.IsValid: Boolean;
begin
  Result := IsDefined and (Width > 0) and (Height > 0);
end;

class function TSvgViewBox.Parse(const AStr: string): TSvgViewBox;
var
  s: string;
  parts: TStringList;
  v: array[0..3] of Single;
  i: Integer;
begin
  Result.IsDefined := False;
  s := StringReplace(Trim(AStr), ',', ' ', [rfReplaceAll]);
  parts := TStringList.Create;
  try
    parts.Delimiter := ' ';
    parts.DelimitedText := s;
    if parts.Count >= 4 then
    begin
      for i := 0 to 3 do
      begin
        if not TryStrToFloat(Trim(parts[i]), v[i], SvgFormatSettings) then
          Exit;
      end;
      Result := Create(v[0], v[1], v[2], v[3]);
    end;
  finally
    parts.Free;
  end;
end;

function TSvgViewBox.GetTransform(const ATargetRect: TFloatRect; const AAspect: TSvgPreserveAspectRatio): TFloatMatrix;
var
  targetW, targetH: Single;
  sx, sy, scale: Single;
  tx, ty: Single;
  helper: TFloatMatrixHelper;
begin
  if not IsValid then
    Exit(IdentityMatrix);

  targetW := ATargetRect.Right - ATargetRect.Left;
  targetH := ATargetRect.Bottom - ATargetRect.Top;

  if (targetW <= 0) or (targetH <= 0) then
    Exit(IdentityMatrix);

  sx := targetW / Width;
  sy := targetH / Height;

  if AAspect.Align = saNone then
  begin
    helper.Matrix := IdentityMatrix;
    helper.Translate(-X, -Y);
    helper.Scale(sx, sy);
    helper.Translate(ATargetRect.Left, ATargetRect.Top);
    Exit(helper.Matrix);
  end;

  if AAspect.MeetOrSlice = msMeet then
    scale := Min(sx, sy)
  else
    scale := Max(sx, sy);

  tx := ATargetRect.Left - X * scale;
  ty := ATargetRect.Top - Y * scale;

  case AAspect.Align of
    saXMidYMin, saXMidYMid, saXMidYMax:
      tx := tx + (targetW - Width * scale) * 0.5;
    saXMaxYMin, saXMaxYMid, saXMaxYMax:
      tx := tx + (targetW - Width * scale);
  end;

  case AAspect.Align of
    saXMinYMid, saXMidYMid, saXMaxYMid:
      ty := ty + (targetH - Height * scale) * 0.5;
    saXMinYMax, saXMidYMax, saXMaxYMax:
      ty := ty + (targetH - Height * scale);
  end;

  helper.Matrix := IdentityMatrix;
  helper.Scale(scale, scale);
  helper.Translate(tx, ty);
  Result := helper.Matrix;
end;

procedure InitializeKeywordDictionaries;
var
  i: integer;
begin
  for i := 0 to High(sColorNames) do
    SvgColorNameDictionary.Add(sColorNames[i].Name, sColorNames[i].Color);
end;

initialization
{$IFDEF FPC}
  SvgFormatSettings := DefaultFormatSettings;
{$ELSE}
  SvgFormatSettings := FormatSettings;
{$ENDIF}
  SvgFormatSettings.DecimalSeparator := '.';

  InitializeKeywordDictionaries;
end.
