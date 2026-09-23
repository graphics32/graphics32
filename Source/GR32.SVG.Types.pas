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
  TSvgBlendMode = (
    bmNormal,
    bmMultiply,
    bmScreen,
    bmOverlay,
    bmDarken,
    bmLighten,
    bmColorDodge,
    bmColorBurn,
    bmHardLight,
    bmSoftLight,
    bmDifference,
    bmExclusion
  );

  TSvgIsolation = (
    isoAuto,
    isoIsolate
  );

function ParseSvgBlendMode(const AName: TValuePUtf8Char): TSvgBlendMode; overload;
function ParseSvgBlendMode(const AName: AnsiString): TSvgBlendMode; overload;
function ParseSvgIsolation(const AName: TValuePUtf8Char): TSvgIsolation; overload;
function ParseSvgIsolation(const AName: AnsiString): TSvgIsolation; overload;
function SvgBlendModeToString(AMode: TSvgBlendMode): string;
function SvgIsolationToString(AIsolation: TSvgIsolation): string;

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
  (*
    Note that while TFloatMatrixHelper CAN be used independently:

      var Helper: TFloatMatrixHelper;
      Helper.Matrix := IdentityMatrix;
      Helper.Scale(a, b);
      ...

    it is actually meant to be used as a type cast helper for TFloatMatrix:

      var MatrixA: TFloatMatrix;
      var MatrixB: TFloatMatrix;
      ...
      TFloatMatrixHelper(MatrixA).Scale(a, b);
      MatrixB := MatrixB * TFloatMatrixHelper(MatrixA);
      ...
  *)
  TFloatMatrixHelper = record
    // Beware that Matrix is Row-Major!
    Matrix: TFloatMatrix;

    // Note: operator Multiply(Left, Right) internally calls Mult(Right, Left) so
    // the result matches normal algebraic expectations: matRes := matA x matB
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
    function IsIdentity: boolean;
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

type
  TBlendModeName = record
    Name: AnsiString;
    Value: TSvgBlendMode;
  end;

var
  SvgBlendModeDictionary: TSvgKeywordDictionary<TSvgBlendMode>;

const
  sBlendModes: array[0..11] of TBlendModeName = (
    (Name: ''; Value: bmNormal),
    (Name: 'multiply'; Value: bmMultiply),
    (Name: 'screen'; Value: bmScreen),
    (Name: 'overlay'; Value: bmOverlay),
    (Name: 'darken'; Value: bmDarken),
    (Name: 'lighten'; Value: bmLighten),
    (Name: 'color-dodge'; Value: bmColorDodge),
    (Name: 'color-burn'; Value: bmColorBurn),
    (Name: 'hard-light'; Value: bmHardLight),
    (Name: 'soft-light'; Value: bmSoftLight),
    (Name: 'difference'; Value: bmDifference),
    (Name: 'exclusion'; Value: bmExclusion)
  );

function ParseSvgBlendMode(const AName: AnsiString): TSvgBlendMode;
var
  Name: TValuePUtf8Char;
begin
  Name.Text := pointer(AName);
  Name.Len := Length(AName);
  Result := ParseSvgBlendMode(Name);
end;

function ParseSvgBlendMode(const AName: TValuePUtf8Char): TSvgBlendMode;
begin
  if (not SvgBlendModeDictionary.Lookup(AName, Result)) then
    Result := bmNormal;
end;

function ParseSvgIsolation(const AName: AnsiString): TSvgIsolation;
var
  Name: TValuePUtf8Char;
begin
  Name.Text := pointer(AName);
  Name.Len := Length(AName);
  Result := ParseSvgIsolation(Name);
end;

function ParseSvgIsolation(const AName: TValuePUtf8Char): TSvgIsolation;
var
  Name: TValuePUtf8Char;
begin
  Name := AName;
  Name.Trim;
  if (Name.StartsText('isolate')) then
    Result := isoIsolate
  else
    Result := isoAuto;
end;

function SvgBlendModeToString(AMode: TSvgBlendMode): string;
begin
  case AMode of
    bmMultiply: Result := 'multiply';
    bmScreen: Result := 'screen';
    bmOverlay: Result := 'overlay';
    bmDarken: Result := 'darken';
    bmLighten: Result := 'lighten';
    bmColorDodge: Result := 'color-dodge';
    bmColorBurn: Result := 'color-burn';
    bmHardLight: Result := 'hard-light';
    bmSoftLight: Result := 'soft-light';
    bmDifference: Result := 'difference';
    bmExclusion: Result := 'exclusion';
  else
    Result := 'normal';
  end;
end;

function SvgIsolationToString(AIsolation: TSvgIsolation): string;
begin
  case AIsolation of
    isoIsolate: Result := 'isolate';
  else
    Result := 'auto';
  end;
end;

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
    Value: TColor32;
  end;

var
  SvgColorNameDictionary: TSvgKeywordDictionary<TColor32>;

const
  sColorNames: array[0..147] of TColorName = (
    (Name: 'red'; Value: clRed32),
    (Name: 'tan'; Value: clTan32),
    (Name: 'aqua'; Value: clAqua32),
    (Name: 'blue'; Value: clBlue32),
    (Name: 'cyan'; Value: clAqua32),
    (Name: 'gold'; Value: clGold32),
    (Name: 'gray'; Value: clGray32),
    (Name: 'grey'; Value: clGrey32),
    (Name: 'lime'; Value: clLime32),
    (Name: 'navy'; Value: clNavy32),
    (Name: 'peru'; Value: clPeru32),
    (Name: 'pink'; Value: clPink32),
    (Name: 'plum'; Value: clPlum32),
    (Name: 'snow'; Value: clSnow32),
    (Name: 'teal'; Value: clTeal32),
    (Name: 'azure'; Value: clAzure32),
    (Name: 'beige'; Value: clBeige32),
    (Name: 'black'; Value: clBlack32),
    (Name: 'brown'; Value: clBrown32),
    (Name: 'coral'; Value: clCoral32),
    (Name: 'green'; Value: clGreen32),
    (Name: 'ivory'; Value: clIvory32),
    (Name: 'khaki'; Value: clKhaki32),
    (Name: 'linen'; Value: clLinen32),
    (Name: 'olive'; Value: clOlive32),
    (Name: 'wheat'; Value: clWheat32),
    (Name: 'white'; Value: clWhite32),
    (Name: 'bisque'; Value: clBisque32),
    (Name: 'indigo'; Value: clIndigo32),
    (Name: 'maroon'; Value: clMaroon32),
    (Name: 'orange'; Value: clOrange32),
    (Name: 'orchid'; Value: clOrchid32),
    (Name: 'purple'; Value: clPurple32),
    (Name: 'salmon'; Value: clSalmon32),
    (Name: 'sienna'; Value: clSienna32),
    (Name: 'silver'; Value: clSilver32),
    (Name: 'tomato'; Value: clTomato32),
    (Name: 'violet'; Value: clViolet32),
    (Name: 'yellow'; Value: clYellow32),
    (Name: 'crimson'; Value: clCrimson32),
    (Name: 'darkred'; Value: clDarkRed32),
    (Name: 'dimgray'; Value: clDimGray32),
    (Name: 'dimgrey'; Value: clDimGray32),
    (Name: 'fuchsia'; Value: clFuchsia32),
    (Name: 'hotpink'; Value: clHotPink32),
    (Name: 'magenta'; Value: clFuchsia32),
    (Name: 'oldlace'; Value: clOldLace32),
    (Name: 'skyblue'; Value: clSkyblue32),
    (Name: 'thistle'; Value: clThistle32),
    (Name: 'cornsilk'; Value: clCornSilk32),
    (Name: 'darkblue'; Value: clDarkBlue32),
    (Name: 'darkcyan'; Value: clDarkCyan32),
    (Name: 'darkgray'; Value: clDarkGray32),
    (Name: 'darkgrey'; Value: clDarkGrey32),
    (Name: 'deeppink'; Value: clDeepPink32),
    (Name: 'honeydew'; Value: clHoneyDew32),
    (Name: 'lavender'; Value: clLavender32),
    (Name: 'moccasin'; Value: clMoccasin32),
    (Name: 'seagreen'; Value: clSeaGreen32),
    (Name: 'seashell'; Value: clSeaShell32),
    (Name: 'aliceblue'; Value: clAliceBlue32),
    (Name: 'burlywood'; Value: clBurlyWood32),
    (Name: 'cadetblue'; Value: clCadetblue32),
    (Name: 'chocolate'; Value: clChocolate32),
    (Name: 'darkgreen'; Value: clDarkGreen32),
    (Name: 'darkkhaki'; Value: clDarkKhaki32),
    (Name: 'firebrick'; Value: clFireBrick32),
    (Name: 'gainsboro'; Value: clGainsBoro32),
    (Name: 'goldenrod'; Value: clGoldenRod32),
    (Name: 'indianred'; Value: clIndianRed32),
    (Name: 'lawngreen'; Value: clLawnGreen32),
    (Name: 'lightblue'; Value: clLightBlue32),
    (Name: 'lightcyan'; Value: clLightCyan32),
    (Name: 'lightgray'; Value: clLightGray32),
    (Name: 'lightgrey'; Value: clLightGrey32),
    (Name: 'lightpink'; Value: clLightPink32),
    (Name: 'limegreen'; Value: clLimeGreen32),
    (Name: 'mintcream'; Value: clMintCream32),
    (Name: 'mistyrose'; Value: clMistyRose32),
    (Name: 'olivedrab'; Value: clOliveDrab32),
    (Name: 'orangered'; Value: clOrangeRed32),
    (Name: 'palegreen'; Value: clPaleGreen32),
    (Name: 'peachpuff'; Value: clPeachPuff32),
    (Name: 'rosybrown'; Value: clRosyBrown32),
    (Name: 'royalblue'; Value: clRoyalBlue32),
    (Name: 'slateblue'; Value: clSlateBlue32),
    (Name: 'slategray'; Value: clSlateGray32),
    (Name: 'slategrey'; Value: clSlateGrey32),
    (Name: 'steelblue'; Value: clSteelblue32),
    (Name: 'turquoise'; Value: clTurquoise32),
    (Name: 'aquamarine'; Value: clAquamarine32),
    (Name: 'blueviolet'; Value: clBlueViolet32),
    (Name: 'chartreuse'; Value: clChartReuse32),
    (Name: 'darkorange'; Value: clDarkOrange32),
    (Name: 'darkorchid'; Value: clDarkOrchid32),
    (Name: 'darksalmon'; Value: clDarkSalmon32),
    (Name: 'darkviolet'; Value: clDarkViolet32),
    (Name: 'dodgerblue'; Value: clDodgerBlue32),
    (Name: 'ghostwhite'; Value: clGhostWhite32),
    (Name: 'lightcoral'; Value: clLightCoral32),
    (Name: 'lightgreen'; Value: clLightGreen32),
    (Name: 'mediumblue'; Value: clMediumBlue32),
    (Name: 'papayawhip'; Value: clPapayaWhip32),
    (Name: 'powderblue'; Value: clPowderBlue32),
    (Name: 'sandybrown'; Value: clSandyBrown32),
    (Name: 'whitesmoke'; Value: clWhitesmoke32),
    (Name: 'darkmagenta'; Value: clDarkMagenta32),
    (Name: 'deepskyblue'; Value: clDeepSkyBlue32),
    (Name: 'floralwhite'; Value: clFloralWhite32),
    (Name: 'forestgreen'; Value: clForestGreen32),
    (Name: 'greenyellow'; Value: clGreenYellow32),
    (Name: 'lightsalmon'; Value: clLightSalmon32),
    (Name: 'lightyellow'; Value: clLightYellow32),
    (Name: 'navajowhite'; Value: clNavajoWhite32),
    (Name: 'saddlebrown'; Value: clSaddleBrown32),
    (Name: 'springgreen'; Value: clSpringgreen32),
    (Name: 'transparent'; Value: clNone32),
    (Name: 'yellowgreen'; Value: clYellowgreen32),
    (Name: 'antiquewhite'; Value: clAntiqueWhite32),
    (Name: 'darkseagreen'; Value: clDarkSeaGreen32),
    (Name: 'lemonchiffon'; Value: clLemonChiffon32),
    (Name: 'lightskyblue'; Value: clLightSkyblue32),
    (Name: 'mediumorchid'; Value: clMediumOrchid32),
    (Name: 'mediumpurple'; Value: clMediumPurple32),
    (Name: 'midnightblue'; Value: clMidnightBlue32),
    (Name: 'darkgoldenrod'; Value: clDarkGoldenRod32),
    (Name: 'darkslateblue'; Value: clDarkSlateBlue32),
    (Name: 'darkslategray'; Value: clDarkSlateGray32),
    (Name: 'darkslategrey'; Value: clDarkSlateGrey32),
    (Name: 'darkturquoise'; Value: clDarkTurquoise32),
    (Name: 'lavenderblush'; Value: clLavenderBlush32),
    (Name: 'lightseagreen'; Value: clLightSeagreen32),
    (Name: 'palegoldenrod'; Value: clPaleGoldenRod32),
    (Name: 'paleturquoise'; Value: clPaleTurquoise32),
    (Name: 'palevioletred'; Value: clPaleVioletred32),
    (Name: 'blanchedalmond'; Value: clBlancheDalmond32),
    (Name: 'cornflowerblue'; Value: clCornFlowerBlue32),
    (Name: 'darkolivegreen'; Value: clDarkOliveGreen32),
    (Name: 'lightslategray'; Value: clLightSlategray32),
    (Name: 'lightslategrey'; Value: clLightSlategrey32),
    (Name: 'lightsteelblue'; Value: clLightSteelblue32),
    (Name: 'mediumseagreen'; Value: clMediumSeaGreen32),
    (Name: 'mediumslateblue'; Value: clMediumSlateBlue32),
    (Name: 'mediumturquoise'; Value: clMediumTurquoise32),
    (Name: 'mediumvioletred'; Value: clMediumVioletRed32),
    (Name: 'mediumaquamarine'; Value: clMediumAquamarine32),
    (Name: 'mediumspringgreen'; Value: clMediumSpringGreen32),
    (Name: 'lightgoldenrodyellow'; Value: clLightGoldenRodYellow32)
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
  Result.Matrix := Mult(Right.Matrix, Left.Matrix);
end;

class operator TFloatMatrixHelper.Multiply(const Left: TFloatMatrixHelper; const Right: TFloatMatrix): TFloatMatrixHelper;
begin
  Result.Matrix := Mult(Right, Left.Matrix);
end;

function TFloatMatrixHelper.IsIdentity: boolean;
begin
  Result := IsIdentityMatrix(Matrix);
end;

class operator TFloatMatrixHelper.Multiply(const Left: TFloatMatrix; const Right: TFloatMatrixHelper): TFloatMatrix;
begin
  Result := Mult(Right.Matrix, Left);
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
    SvgColorNameDictionary.Add(sColorNames[i].Name, sColorNames[i].Value);

  for i := 0 to High(sBlendModes) do
    SvgBlendModeDictionary.Add(sBlendModes[i].Name, sBlendModes[i].Value);
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
