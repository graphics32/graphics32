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
  SysUtils, Classes, Math, GR32, GR32_Transforms, GR32_Math, GR32_LowLevel;

var
  SvgFormatSettings: TFormatSettings;

// HSLtoRGB overload for float Alpha in the range [0.0..1.0]
function HSLtoRGB(H, S, L, A: Single): TColor32;

function SvgColorNameToColor(const AName: string; ADefault: TColor32): TColor32;

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
    class function Parse(const AStr: UTF8String): TSvgColor; static;
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

implementation

uses
  AnsiStrings,
  GR32.SVG.Xml;

function HSLtoRGB(H, S, L, A: Single): TColor32;
begin
  Result := GR32.HSLtoRGB(
    EnsureRange(H, 0.0, 1.0),
    EnsureRange(S, 0.0, 1.0),
    EnsureRange(L, 0.0, 1.0),
    Clamp(Round(A * 255.0))
  );
end;

function SvgColorNameToColor(const AName: string; ADefault: TColor32): TColor32;
var
  s: string;
begin
  s := LowerCase(AName);
  case Length(s) of
    3:
      if s = 'red' then Exit(clRed32)
      else if s = 'tan' then Exit(clTan32)
      ;

    4:
      case s[1] of
        'a':
          if s = 'aqua' then Exit(clAqua32)
          ;
        'b':
          if s = 'blue' then Exit(clBlue32)
          ;
        'c':
          if s = 'cyan' then Exit(clAqua32)
          ;
        'g':
          if s = 'gold' then Exit(clGold32)
          else if s = 'gray' then Exit(clGray32)
          else if s = 'grey' then Exit(clGrey32)
          ;
        'l':
          if s = 'lime' then Exit(clLime32)
          ;
        'n':
          if s = 'navy' then Exit(clNavy32)
          ;
        'p':
          if s = 'peru' then Exit(clPeru32)
          else if s = 'pink' then Exit(clPink32)
          else if s = 'plum' then Exit(clPlum32)
          ;
        's':
          if s = 'snow' then Exit(clSnow32)
          ;
        't':
          if s = 'teal' then Exit(clTeal32)
          ;
      end;

    5:
      case s[1] of
        'a':
          if s = 'azure' then Exit(clAzure32)
          ;
        'b':
          if s = 'beige' then Exit(clBeige32)
          else if s = 'black' then Exit(clBlack32)
          else if s = 'brown' then Exit(clBrown32)
          ;
        'c':
          if s = 'coral' then Exit(clCoral32)
          ;
        'g':
          if s = 'green' then Exit(clGreen32)
          ;
        'i':
          if s = 'ivory' then Exit(clIvory32)
          ;
        'k':
          if s = 'khaki' then Exit(clKhaki32)
          ;
        'l':
          if s = 'linen' then Exit(clLinen32)
          ;
        'o':
          if s = 'olive' then Exit(clOlive32)
          ;
        'w':
          if s = 'wheat' then Exit(clWheat32)
          else if s = 'white' then Exit(clWhite32)
          ;
      end;

    6:
      case s[1] of
        'b':
          if s = 'bisque' then Exit(clBisque32)
          ;
        'i':
          if s = 'indigo' then Exit(clIndigo32)
          ;
        'm':
          if s = 'maroon' then Exit(clMaroon32)
          ;
        'o':
          if s = 'orange' then Exit(clOrange32)
          else if s = 'orchid' then Exit(clOrchid32)
          ;
        'p':
          if s = 'purple' then Exit(clPurple32)
          ;
        's':
          if s = 'salmon' then Exit(clSalmon32)
          else if s = 'sienna' then Exit(clSienna32)
          else if s = 'silver' then Exit(clSilver32)
          ;
        't':
          if s = 'tomato' then Exit(clTomato32)
          ;
        'v':
          if s = 'violet' then Exit(clViolet32)
          ;
        'y':
          if s = 'yellow' then Exit(clYellow32)
          ;
      end;

    7:
      case s[1] of
        'c':
          if s = 'crimson' then Exit(clCrimson32)
          ;
        'd':
          if s = 'darkred' then Exit(clDarkRed32)
          else if s = 'dimgray' then Exit(clDimGray32)
          else if s = 'dimgrey' then Exit(clDimGray32)
          ;
        'f':
          if s = 'fuchsia' then Exit(clFuchsia32)
          ;
        'h':
          if s = 'hotpink' then Exit(clHotPink32)
          ;
        'm':
          if s = 'magenta' then Exit(clFuchsia32)
          ;
        'o':
          if s = 'oldlace' then Exit(clOldLace32)
          ;
        's':
          if s = 'skyblue' then Exit(clSkyblue32)
          ;
        't':
          if s = 'thistle' then Exit(clThistle32)
          ;
      end;

    8:
      case s[1] of
        'c':
          if s = 'cornsilk' then Exit(clCornSilk32)
          ;
        'd':
          if s = 'darkblue' then Exit(clDarkBlue32)
          else if s = 'darkcyan' then Exit(clDarkCyan32)
          else if s = 'darkgray' then Exit(clDarkGray32)
          else if s = 'darkgrey' then Exit(clDarkGrey32)
          else if s = 'deeppink' then Exit(clDeepPink32)
          ;
        'h':
          if s = 'honeydew' then Exit(clHoneyDew32)
          ;
        'l':
          if s = 'lavender' then Exit(clLavender32)
          ;
        'm':
          if s = 'moccasin' then Exit(clMoccasin32)
          ;
        's':
          if s = 'seagreen' then Exit(clSeaGreen32)
          else if s = 'seashell' then Exit(clSeaShell32)
          ;
      end;

    9:
      case s[1] of
        'a':
          if s = 'aliceblue' then Exit(clAliceBlue32)
          ;
        'b':
          if s = 'burlywood' then Exit(clBurlyWood32)
          ;
        'c':
          if s = 'cadetblue' then Exit(clCadetblue32)
          else if s = 'chocolate' then Exit(clChocolate32)
          ;
        'd':
          if s = 'darkgreen' then Exit(clDarkGreen32)
          else if s = 'darkkhaki' then Exit(clDarkKhaki32)
          ;
        'f':
          if s = 'firebrick' then Exit(clFireBrick32)
          ;
        'g':
          if s = 'gainsboro' then Exit(clGainsBoro32)
          else if s = 'goldenrod' then Exit(clGoldenRod32)
          ;
        'i':
          if s = 'indianred' then Exit(clIndianRed32)
          ;
        'l':
          case s[2] of
            'a':
              if s = 'lawngreen' then Exit(clLawnGreen32)
              ;
            'i':
              case s[3] of
                'g':
                  if s = 'lightblue' then Exit(clLightBlue32)
                  else if s = 'lightcyan' then Exit(clLightCyan32)
                  else if s = 'lightgray' then Exit(clLightGray32)
                  else if s = 'lightgrey' then Exit(clLightGrey32)
                  else if s = 'lightpink' then Exit(clLightPink32)
                  ;
                'm':
                  if s = 'limegreen' then Exit(clLimeGreen32)
                  ;
              end;
          end;
        'm':
          if s = 'mintcream' then Exit(clMintCream32)
          else if s = 'mistyrose' then Exit(clMistyRose32)
          ;
        'o':
          if s = 'olivedrab' then Exit(clOliveDrab32)
          else if s = 'orangered' then Exit(clOrangeRed32)
          ;
        'p':
          if s = 'palegreen' then Exit(clPaleGreen32)
          else if s = 'peachpuff' then Exit(clPeachPuff32)
          ;
        'r':
          if s = 'rosybrown' then Exit(clRosyBrown32)
          else if s = 'royalblue' then Exit(clRoyalBlue32)
          ;
        's':
          if s = 'slateblue' then Exit(clSlateBlue32)
          else if s = 'slategray' then Exit(clSlateGray32)
          else if s = 'slategrey' then Exit(clSlateGrey32)
          else if s = 'steelblue' then Exit(clSteelblue32)
          ;
        't':
          if s = 'turquoise' then Exit(clTurquoise32)
          ;
      end;

    10:
      case s[1] of
        'a':
          if s = 'aquamarine' then Exit(clAquamarine32)
          ;
        'b':
          if s = 'blueviolet' then Exit(clBlueViolet32)
          ;
        'c':
          if s = 'chartreuse' then Exit(clChartReuse32)
          ;
        'd':
          if s = 'darkorange' then Exit(clDarkOrange32)
          else if s = 'darkorchid' then Exit(clDarkOrchid32)
          else if s = 'darksalmon' then Exit(clDarkSalmon32)
          else if s = 'darkviolet' then Exit(clDarkViolet32)
          else if s = 'dodgerblue' then Exit(clDodgerBlue32)
          ;
        'g':
          if s = 'ghostwhite' then Exit(clGhostWhite32)
          ;
        'l':
          if s = 'lightcoral' then Exit(clLightCoral32)
          else if s = 'lightgreen' then Exit(clLightGreen32)
          ;
        'm':
          if s = 'mediumblue' then Exit(clMediumBlue32)
          ;
        'p':
          if s = 'papayawhip' then Exit(clPapayaWhip32)
          else if s = 'powderblue' then Exit(clPowderBlue32)
          ;
        's':
          if s = 'sandybrown' then Exit(clSandyBrown32)
          ;
        'w':
          if s = 'whitesmoke' then Exit(clWhitesmoke32)
          ;
      end;

    11:
      case s[1] of
        'd':
          if s = 'darkmagenta' then Exit(clDarkMagenta32)
          else if s = 'deepskyblue' then Exit(clDeepSkyBlue32)
          ;
        'f':
          if s = 'floralwhite' then Exit(clFloralWhite32)
          else if s = 'forestgreen' then Exit(clForestGreen32)
          ;
        'g':
          if s = 'greenyellow' then Exit(clGreenYellow32)
          ;
        'l':
          if s = 'lightsalmon' then Exit(clLightSalmon32)
          else if s = 'lightyellow' then Exit(clLightYellow32)
          ;
        'n':
          if s = 'navajowhite' then Exit(clNavajoWhite32)
          ;
        's':
          if s = 'saddlebrown' then Exit(clSaddleBrown32)
          else if s = 'springgreen' then Exit(clSpringgreen32)
          ;
        't':
          if s = 'transparent' then Exit(clNone32)
          ;
        'y':
          if s = 'yellowgreen' then Exit(clYellowgreen32)
          ;
      end;

    12:
      case s[1] of
        'a':
          if s = 'antiquewhite' then Exit(clAntiqueWhite32)
          ;
        'd':
          if s = 'darkseagreen' then Exit(clDarkSeaGreen32)
          ;
        'l':
          if s = 'lemonchiffon' then Exit(clLemonChiffon32)
          else if s = 'lightskyblue' then Exit(clLightSkyblue32)
          ;
        'm':
          if s = 'mediumorchid' then Exit(clMediumOrchid32)
          else if s = 'mediumpurple' then Exit(clMediumPurple32)
          else if s = 'midnightblue' then Exit(clMidnightBlue32)
          ;
      end;

    13:
      case s[1] of
        'd':
          if s = 'darkgoldenrod' then Exit(clDarkGoldenRod32)
          else if s = 'darkslateblue' then Exit(clDarkSlateBlue32)
          else if s = 'darkslategray' then Exit(clDarkSlateGray32)
          else if s = 'darkslategrey' then Exit(clDarkSlateGrey32)
          else if s = 'darkturquoise' then Exit(clDarkTurquoise32)
          ;
        'l':
          if s = 'lavenderblush' then Exit(clLavenderBlush32)
          else if s = 'lightseagreen' then Exit(clLightSeagreen32)
          ;
        'p':
          if s = 'palegoldenrod' then Exit(clPaleGoldenRod32)
          else if s = 'paleturquoise' then Exit(clPaleTurquoise32)
          else if s = 'palevioletred' then Exit(clPaleVioletred32)
          ;
      end;

    14:
      case s[1] of
        'b':
          if s = 'blanchedalmond' then Exit(clBlancheDalmond32)
          ;
        'c':
          if s = 'cornflowerblue' then Exit(clCornFlowerBlue32)
          ;
        'd':
          if s = 'darkolivegreen' then Exit(clDarkOliveGreen32)
          ;
        'l':
          if s = 'lightslategray' then Exit(clLightSlategray32)
          else if s = 'lightslategrey' then Exit(clLightSlategrey32)
          else if s = 'lightsteelblue' then Exit(clLightSteelblue32)
          ;
        'm':
          if s = 'mediumseagreen' then Exit(clMediumSeaGreen32)
          ;
      end;

    15:
      if s = 'mediumslateblue' then Exit(clMediumSlateBlue32)
      else if s = 'mediumturquoise' then Exit(clMediumTurquoise32)
      else if s = 'mediumvioletred' then Exit(clMediumVioletRed32)
      ;

    16:
      if s = 'mediumaquamarine' then Exit(clMediumAquamarine32)
      ;

    17:
      if s = 'mediumspringgreen' then Exit(clMediumSpringGreen32)
      ;

    20:
      if s = 'lightgoldenrodyellow' then Exit(clLightGoldenRodYellow32)
      ;
  end;
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

class function TSvgColor.Parse(const AStr: UTF8String): TSvgColor;

  function ParseHexByte(const h: UTF8String): Byte;
  begin
    Result := StrToIntDef('$' + h, 0);
  end;

var
  s, lowerStr: UTF8String;
  r, g, b, a: Byte;
  parts: TStringList;
  valFloat: Single;
  pStart, pEnd: Integer;
begin
  if (AStr = '') then
    Exit(None);

  s := AnsiStrings.Trim(AStr);
  lowerStr := AnsiStrings.LowerCase(s);

  if (lowerStr = '') or (lowerStr = 'none') then
    Exit(None);

  if lowerStr = 'currentcolor' then
    Exit(CurrentColor);

  if (Length(s) > 0) and (s[1] = '#') then
  begin
    Delete(s, 1, 1);
    if Length(s) = 3 then
    begin
      r := ParseHexByte(s[1] + s[1]);
      g := ParseHexByte(s[2] + s[2]);
      b := ParseHexByte(s[3] + s[3]);
      Exit(Create(Color32(r, g, b, 255)));
    end
    else if Length(s) = 6 then
    begin
      r := ParseHexByte(Copy(s, 1, 2));
      g := ParseHexByte(Copy(s, 3, 2));
      b := ParseHexByte(Copy(s, 5, 2));
      Exit(Create(Color32(r, g, b, 255)));
    end
    else if Length(s) = 8 then
    begin
      r := ParseHexByte(Copy(s, 1, 2));
      g := ParseHexByte(Copy(s, 3, 2));
      b := ParseHexByte(Copy(s, 5, 2));
      a := ParseHexByte(Copy(s, 7, 2));
      Exit(Create(Color32(r, g, b, a)));
    end;
  end;

  if (Pos('rgb(', lowerStr) = 1) or (Pos('rgba(', lowerStr) = 1) then
  begin
    pStart := Pos('(', s);
    pEnd := Pos(')', s);
    if (pStart > 0) and (pEnd > pStart) then
      s := Copy(s, pStart + 1, pEnd - pStart - 1)
    else
      s := '';
    s := StringReplace(s, ',', ' ', [rfReplaceAll]);
    parts := TStringList.Create;
    try
      parts.Delimiter := ' ';
      parts.DelimitedText := s;
      if Pos('rgb(', lowerStr) = 1 then
      begin
        if parts.Count >= 3 then
        begin
          r := StrToIntDef(Trim(parts[0]), 0);
          g := StrToIntDef(Trim(parts[1]), 0);
          b := StrToIntDef(Trim(parts[2]), 0);
          Exit(Create(Color32(r, g, b, 255)));
        end;
      end
      else if Pos('rgba(', lowerStr) = 1 then
      begin
        if parts.Count >= 4 then
        begin
          r := StrToIntDef(Trim(parts[0]), 0);
          g := StrToIntDef(Trim(parts[1]), 0);
          b := StrToIntDef(Trim(parts[2]), 0);
          valFloat := 1.0;
          TryStrToFloat(Trim(parts[3]), valFloat, SvgFormatSettings);
          a := Round(EnsureRange(valFloat, 0.0, 1.0) * 255.0);
          Exit(Create(Color32(r, g, b, a)));
        end;
      end;
    finally
      parts.Free;
    end;
  end;

  Result := Create(SvgColorNameToColor(lowerStr, clBlack32));
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

initialization
{$IFDEF FPC}
  SvgFormatSettings := DefaultFormatSettings;
{$ELSE}
  SvgFormatSettings := FormatSettings;
{$ENDIF}
  SvgFormatSettings.DecimalSeparator := '.';

end.
