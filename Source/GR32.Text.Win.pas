unit GR32.Text.Win;

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
 * The Original Code is Delphi/Windows text vectorization utilities for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Mattias Andersson <mattias@centaurix.com>
 *
 * Portions created by the Initial Developer are Copyright (C) 2012
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

{$ifndef FONT_CACHE}
  {$undef FONT_CACHE_PATH}
{$endif FONT_CACHE}


//------------------------------------------------------------------------------
//
//      This unit should be considered internal to Graphics32.
//
//      Use the corresponding functions in the backend instead.
//
//------------------------------------------------------------------------------

uses
  Generics.Defaults,
  Windows, Types,
  Graphics,

  GR32,
  GR32_Paths,
  GR32.Text.Types,
  GR32.Text.Cache;

//------------------------------------------------------------------------------
//
//      Font provider for Windows platform
//
//------------------------------------------------------------------------------
type
  TFontContext = class(TInterfacedObject, IFontContext32)
  private class var
    FFontDC: HDC;
  private
    class function GetFontDC: HDC; static;
    class destructor Destroy;

  private
    FFont: TFont;
    FLogFont: TLogFont;
    FSessionCount: integer;

  private
    // IFontContext32
    function Comparer: IEqualityComparer<IFontContext32>;

    function GetFontFaceMetric(var AFontFaceMetric: TFontFaceMetric32): boolean;
    function GetGlyphMetric(AGlyph: Cardinal; var AGlyphMetrics: TGlyphMetrics32): boolean;
    function GetGlyphOutline(AGlyph: Cardinal; var AGlyphMetrics: TGlyphMetrics32; APath: TCustomPath; OffsetX: Single = 0; OffsetY: Single = 0; MaxX: Single = -1): boolean;

    procedure BeginSession;
    procedure EndSession;
  public
    constructor Create(AFont: TFont);
    class property FontDC: HDC read GetFontDC;
  end;

type
  TFontOutlineProvider = class(TInterfacedObject, IFontOutlineProvider32)
  private
    // IFontOutlineProvider32
    function CreateContext(AFont: TFont): IFontContext32;
  end;

//------------------------------------------------------------------------------
//
//      Text functions for Windows
//
//------------------------------------------------------------------------------
type
  TextToolsWin = record
  private
    // GetDC/ReleaseDC is relatively expensive and would take up the bulk of the
    // time if used. Instead we create and cache a dedicated DC for use
    // exclusively here.
    class var FontDC: HDC;

    class var HorizontalScale: Integer; // stretch factor when calling GetGlyphOutline()
    class var HorizontalScaleInv: Single;

  private
    class destructor Destroy;

    class function DoMeasureText(const FontContext: IFontContext32; const ARect: TFloatRect; const AText: string; AFlags: Cardinal = 0): TFloatRect; static;
    class procedure InternalTextToPath(const FontContext: IFontContext32; Path: TCustomPath; var ARect: TFloatRect; const Text: string; Flags: Cardinal); static;
  public
    class procedure TextToPath(AFont: TFont; APath: TCustomPath; const ARect: TFloatRect; const AText: string; AFlags: Cardinal = 0); static;
    class function TextToPolyPolygon(AFont: TFont; const ARect: TFloatRect; const AText: string; AFlags: Cardinal = 0): TArrayOfArrayOfFloatPoint; static;

    class function MeasureText(AFont: TFont; const ARect: TFloatRect; const AText: string; AFlags: Cardinal = 0): TFloatRect; static;

    class procedure SetHinting(Value: TTextHinting); static;
    class function GetHinting: TTextHinting; static;
  end;


var
  UseHinting: Boolean = False;
  VertFlip_mat2: TMat2;

const
  GGO_UNHINTED = $0100;
  GGODefaultFlags: array [Boolean] of Integer = (GGO_NATIVE or GGO_UNHINTED, GGO_NATIVE);

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  Character,
{$ifndef FPC}
  System.Hash,
{$endif}
{$IFDEF USESTACKALLOC}
  GR32_LowLevel,
{$ENDIF}
  SysUtils;

const
  TT_PRIM_CSPLINE = 3;

  MaxSingle   =  3.4e+38;

type
  TKerningPairArray = array [0..0] of TKerningPair;

//------------------------------------------------------------------------------

// import GetKerningPairs from gdi32 library
function GetKerningPairs(DC: HDC; Count: DWORD; P: PKerningPair): DWORD; stdcall; external gdi32 name 'GetKerningPairs';

//------------------------------------------------------------------------------

function PointFXtoPointF(const Point: tagPointFX): TFloatPoint; {$IFDEF UseInlining} inline; {$ENDIF}
begin
  Result.X := Point.X.Value + Point.X.Fract * FixedToFloat;
  Result.Y := Point.Y.Value + Point.Y.Fract * FixedToFloat;
end;


//------------------------------------------------------------------------------

function IsWhiteSpace(c: Char): boolean; {$IFDEF UseInlining} inline; {$ENDIF}
begin
  Result := (c.GetUnicodeCategory = TUnicodeCategory.ucSpaceSeparator);
end;

function IsBreakableWhiteSpace(c: Char): boolean; {$IFDEF UseInlining} inline; {$ENDIF}
begin
  case c of
    #$00A0, #$202F, #$2060, #$FEFF:
      Result := False;
  else
    Result := IsWhiteSpace(c);
  end;
end;

class procedure TextToolsWin.InternalTextToPath(const FontContext: IFontContext32; Path: TCustomPath; var ARect: TFloatRect; const Text: string; Flags: Cardinal);
const
  CHAR_CR = 10;
  CHAR_NL = 13;
  CHAR_SP = 32;
var
  FontFaceMetric: TFontFaceMetric32;
  SpaceMetric: TGlyphMetrics32;
  CharOffsets: TArrayOfInteger;
  CharWidths: TArrayOfInteger;
  TextPath: TFlattenedPath;
  LineStart: Integer;
  SpaceCount: Integer;

{$IFDEF USEKERNING}
  NextCharValue: Integer;
  KerningPairs: PKerningPairArray;
  KerningPairCount: Integer;
{$ENDIF}

  procedure AlignTextCenter(X: Single; CurrentI: Integer);
  var
    w, M, N, PathStart, PathEnd, CharStart, CharEnd: Integer;
    Delta: TFloat;
    i: Integer;
    MinX, MaxX: Single;
  begin
    Delta := Round(((ARect.Right - ARect.Left) * HorizontalScale - X - 1) * 0.5);
    PathStart := CharOffsets[LineStart];
    PathEnd := CharOffsets[CurrentI] - 1;

    if (Flags and DT_SINGLELINE = 0) and (Flags and DT_NOCLIP = 0) then
    begin
      MinX := ARect.Left + Delta;
      MaxX := ARect.Right + Delta;
      CharStart := LineStart;
      CharEnd := CurrentI;

      w := Round(Delta);
      for i := LineStart to CurrentI - 1 do
      begin
        if w < Arect.Left then
        begin
          CharStart := i + 1;
          MinX := w + CharWidths[i];
        end;
        w := w + CharWidths[i];
        if w <= ARect.Right then
        begin
          CharEnd := i + 1;
          MaxX := w;
        end;
      end;

      if (Flags and DT_WORDBREAK <> 0) then
      begin
        if (CharStart > LineStart) then
          while (not IsBreakableWhiteSpace(Text[CharStart])) and (CharStart < CharEnd) do
            Inc(CharStart);

        if (CharEnd < CurrentI) then
          while (not IsBreakableWhiteSpace(Text[CharEnd])) and (CharEnd > CharStart) do
            Dec(CharEnd);

        MinX := Round(Delta);
        for i := 0 to CharStart - 1 do
          MinX := MinX + CharWidths[i];

        MaxX := Round(Delta);
        for i := 0 to CharEnd - 1 do
          MaxX := MaxX + CharWidths[i];
      end;

      PathStart := CharOffsets[CharStart];
      PathEnd := CharOffsets[CharEnd] - 1;

      for M := 0 to PathStart - 1 do
        SetLength(TextPath.Path[M], 0);

      for M := PathEnd + 1 to CharOffsets[CurrentI] - 1 do
        SetLength(TextPath.Path[M], 0);

      Delta := Delta + (((MinX - ARect.Left) + (ARect.Right - MaxX)) * 0.5) - MinX;
    end;

    for M := PathStart to PathEnd do
      for N := 0 to High(TextPath.Path[M]) do
        TextPath.Path[M, N].X := TextPath.Path[M, N].X + Delta;
  end;

  procedure AlignTextRight(X: Single; CurrentI: Integer);
  var
    w, i, M, N, PathStart, PathEnd, CharStart: Integer;
    Delta: TFloat;
  begin
    Delta := Round(ARect.Right * HorizontalScale - X - 1);
    PathStart := CharOffsets[LineStart];
    PathEnd := CharOffsets[CurrentI] - 1;

    if (Flags and DT_SINGLELINE = 0) and (Flags and DT_NOCLIP = 0) then
    begin
      CharStart := LineStart;

      w := 0;
      for i := LineStart to CurrentI - 1 do
      begin
        if w + Delta < Arect.Left then
          CharStart := i + 1;

        w := w + CharWidths[i];
      end;

      if (Flags and DT_WORDBREAK <> 0) then
        if (CharStart > LineStart) then
          while (not IsBreakableWhiteSpace(Text[CharStart])) and (CharStart < CurrentI) do
            Inc(CharStart);

      PathStart := CharOffsets[CharStart];

      for M := 0 to PathStart - 1 do
        SetLength(TextPath.Path[M], 0);
    end;

    for M := PathStart to PathEnd do
      for N := 0 to High(TextPath.Path[M]) do
        TextPath.Path[M, N].X := TextPath.Path[M, N].X + Delta;
  end;

  procedure AlignTextLeft(X: Single; CurrentI: Integer);
  var
    w, i, M, PathEnd, CharEnd: Integer;
  begin
    if (Flags and DT_SINGLELINE = DT_SINGLELINE) or (Flags and DT_NOCLIP = DT_NOCLIP) then
      exit;

    CharEnd := LineStart;

    w := 0;
    for i := LineStart to CurrentI - 1 do
    begin
      w := w + CharWidths[i];

      if w > ARect.Width then
        break;

      CharEnd := i + 1;
    end;

    if (Flags and DT_WORDBREAK <> 0) then
      if (CharEnd < CurrentI) then
        while (not IsBreakableWhiteSpace(Text[CharEnd])) and (CharEnd > LineStart) do
          Dec(CharEnd);

    PathEnd := CharOffsets[CharEnd] - 1;

    for M := PathEnd + 1 to CharOffsets[CurrentI] - 1 do
      SetLength(TextPath.Path[M], 0);
  end;

  procedure AlignTextJustify(X: Single; CurrentI: Integer);
  var
    L, M, N, PathStart, PathEnd: Integer;
    SpcDelta, SpcDeltaInc: TFloat;
  begin
    if (SpaceCount < 1) or IsWhiteSpace(Text[CurrentI]) then
      Exit;

    SpcDelta := (ARect.Right * HorizontalScale - X - 1) / SpaceCount;
    SpcDeltaInc := SpcDelta;
    L := LineStart;

    // Trim leading spaces ...
    while (L < CurrentI) and IsWhiteSpace(Text[L]) do
      Inc(L);

    // Now find first space char in line ...
    while (L < CurrentI) and (not IsWhiteSpace(Text[L])) do
      Inc(L);

    PathStart := CharOffsets[L - 1];
    repeat
      M := L + 1;
      while (M < CurrentI) and (not IsWhiteSpace(Text[M])) do
        Inc(M);

      PathEnd := CharOffsets[M];
      L := M;

      for M := PathStart to PathEnd - 1 do
        for N := 0 to High(TextPath.Path[M]) do
          TextPath.Path[M, N].X := TextPath.Path[M, N].X + SpcDeltaInc;

      SpcDeltaInc := SpcDeltaInc + SpcDelta;
      PathStart := PathEnd;
    until (L >= CurrentI);
  end;

  procedure AlignLine(X: Single; CurrentI: Integer);
  begin
    if (TextPath = nil) or (Length(TextPath.Path) = 0) then
      exit;

    case (Flags and DT_HORZ_ALIGN_MASK) of
      DT_LEFT   : AlignTextLeft(X, CurrentI);
      DT_CENTER : AlignTextCenter(X, CurrentI);
      DT_RIGHT  : AlignTextRight(X, CurrentI);
      DT_JUSTIFY: AlignTextJustify(X, CurrentI);
    end;
  end;

  procedure AddSpace(var X: Single);
  begin
    Inc(SpaceCount);
    X := X + SpaceMetric.AdvanceWidthX;
  end;

  procedure NewLine(var X, Y: Single; CurrentI: Integer);
  begin
    if (Flags and DT_SINGLELINE <> 0) then
    begin
      AddSpace(X);
      Exit;
    end;

    AlignLine(X, CurrentI);

    X := ARect.Left * HorizontalScale;
    Y := Y + FontFaceMetric.Height;

    LineStart := CurrentI;
    SpaceCount := 0;
  end;

  function MeasureTextX(const S: string): Integer;
  var
    i: Integer;
    CharValue: Cardinal;
    GlyphMetric: TGlyphMetrics32;
  begin
    Result := 0;

    for i := 1 to Length(S) do
    begin
      CharValue := Ord(S[i]);

      if (not FontContext.GetGlyphMetric(CharValue, GlyphMetric)) then
        RaiseLastOSError;

      Inc(Result, GlyphMetric.AdvanceWidthX);
    end;
  end;

  function NeedsNewLine(X: Single): Boolean;
  begin
    Result := (ARect.Right > ARect.Left) and (X > ARect.Right * HorizontalScale);
  end;

var
  I, J, TextLen: Integer;
  CharValue: Cardinal;
  X, Y, XMax, YMax, MaxRight: Single;
  S: string;
  OwnedPath: TFlattenedPath;
  GlyphMetric: TGlyphMetrics32;
  UnicodeCategory: TUnicodeCategory;
  LastUnicodeCategory: TUnicodeCategory;
begin
  SpaceCount := 0;
  LineStart := 0;
  OwnedPath := nil;
  try

    if (Path <> nil) then
    begin

      if (Path is TFlattenedPath) then
      begin
        TextPath := TFlattenedPath(Path);
        TextPath.Clear;
      end else
      begin
        OwnedPath := TFlattenedPath.Create;
        TextPath := OwnedPath;
      end

    end else
      TextPath := nil;

    FontContext.GetFontFaceMetric(FontFaceMetric);

    TextLen := Length(Text);
    X := ARect.Left * HorizontalScale;
    Y := ARect.Top + FontFaceMetric.Ascent;
    XMax := X;

    if (Path = nil) or (ARect.Right = ARect.Left) then
      MaxRight := MaxSingle //either measuring Text or unbounded Text
    else
      MaxRight := ARect.Right * HorizontalScale;

    SetLength(CharOffsets, TextLen + 1);
    CharOffsets[0] := 0;
    SetLength(CharWidths, TextLen);

    if (not FontContext.GetGlyphMetric(CHAR_SP, SpaceMetric)) then
      RaiseLastOSError;

    if (Flags and DT_SINGLELINE <> 0) or (ARect.Left = ARect.Right) then
    begin
      // ignore justify when forcing singleline ...
      if (Flags and DT_JUSTIFY = DT_JUSTIFY) then
        Flags := Flags and not DT_JUSTIFY;

      // ignore wordbreak when forcing singleline ...
      //if (Flags and DT_WORDBREAK = DT_WORDBREAK) then
      //  Flags := Flags and not DT_WORDBREAK;
      MaxRight := MaxSingle;
    end;

{$IFDEF USEKERNING}
    KerningPairs := nil;
    try

      KerningPairCount := GetKerningPairs(DC, 0, nil);
      if GetLastError <> 0 then
        RaiseLastOSError;

      if KerningPairCount > 0 then
      begin
        GetMem(KerningPairs, KerningPairCount * SizeOf(TKerningPair));
        GetKerningPairs(DC, KerningPairCount, PKerningPair(KerningPairs));
      end;
{$ENDIF}

    // Batch whole path construction so we can be sure that the path isn't rendered
    // while we're still modifying it.
    if (TextPath <> nil) then
      TextPath.BeginUpdate;

    LastUnicodeCategory := TUnicodeCategory.ucUnassigned;

    for I := 1 to TextLen do
    begin

      CharValue := Ord(Text[I]);

      // $00A0  No-break space
      // $202F  Narrow no-break space
      // $2060  Word joiner (zero width no-break space)
      // $FEFF  Zero width no-break space
      case CharValue of
        $00A0, $202F, $2060, $FEFF:
          UnicodeCategory := TUnicodeCategory.ucUnassigned
      else
        UnicodeCategory := Char(CharValue).GetUnicodeCategory;
      end;

      if (UnicodeCategory in [TUnicodeCategory.ucControl, TUnicodeCategory.ucLineSeparator, TUnicodeCategory.ucParagraphSeparator, TUnicodeCategory.ucSpaceSeparator]) then
      begin
        if (Flags and DT_SINGLELINE = DT_SINGLELINE) then
        begin
          CharValue := CHAR_SP;
          UnicodeCategory := TUnicodeCategory.ucSpaceSeparator;
        end;

        if (TextPath <> nil) then
          // Save path list offset of first path of current glyph
          CharOffsets[I] := Length(TextPath.Path);

        CharWidths[i-1] := SpaceMetric.AdvanceWidthX;

        case UnicodeCategory of
          TUnicodeCategory.ucLineSeparator:
            if (LastUnicodeCategory <> TUnicodeCategory.ucParagraphSeparator) then
              NewLine(X, Y, I);

          TUnicodeCategory.ucParagraphSeparator:
            if (LastUnicodeCategory <> TUnicodeCategory.ucLineSeparator) then
              NewLine(X, Y, I);

          TUnicodeCategory.ucSpaceSeparator:
            begin
              if (Flags and DT_WORDBREAK = DT_WORDBREAK) then
              begin

                J := I + 1;
                while (J <= TextLen) do
                begin
                  CharValue := Ord(Text[J]);
                  if IsBreakableWhiteSpace(Char(CharValue)) or (Char(CharValue).GetUnicodeCategory in [TUnicodeCategory.ucLineSeparator, TUnicodeCategory.ucParagraphSeparator]) then
                    Inc(J)
                  else
                    break;
                end;

                S := Copy(Text, I, J - I);

                if NeedsNewLine(X + MeasureTextX(S)) then
                  NewLine(X, Y, I)
                else
                  AddSpace(X);

              end else
              begin

                if NeedsNewLine(X + SpaceMetric.AdvanceWidthX) then
                  NewLine(X, Y, I)
                else
                  AddSpace(X);

              end;
            end;
        end;

      end else
      begin

        if FontContext.GetGlyphOutline(CharValue, GlyphMetric, TextPath, X, Y, MaxRight) then
        begin

          if (TextPath <> nil) then
            // Save path list offset of first path of current glyph
            CharOffsets[I] := Length(TextPath.Path);

          CharWidths[I-1] := GlyphMetric.AdvanceWidthX;

        end else
        begin

          if (I > 0) and IsWhiteSpace(Text[I-1]) then
          begin
            // this only happens without DT_WORDBREAK
            X := X - SpaceMetric.AdvanceWidthX;
            Dec(SpaceCount);
          end;

          // the current glyph doesn't fit so a word must be split since
          // it fills more than a whole line ...
          NewLine(X, Y, I-1);

          if (not FontContext.GetGlyphOutline(CharValue, GlyphMetric, TextPath, X, Y, MaxRight)) then
            break;

          if (TextPath <> nil) then
            // Save path list offset of first path of current glyph
            CharOffsets[I] := Length(TextPath.Path);

          CharWidths[I-1] := GlyphMetric.AdvanceWidthX;

        end;

        X := X + GlyphMetric.AdvanceWidthX;

{$IFDEF USEKERNING}
        if i < TextLen then
          NextCharValue := Ord(Text[i + 1]);

        for J := 0 to KerningPairCount - 1 do
          if (KerningPairs^[J].wFirst = CharValue) and (KerningPairs^[J].wSecond = NextCharValue) then
          begin
            X := X + KerningPairs^[J].iKernAmount;
            break;
          end;
{$ENDIF}

        if (X > XMax) then
          XMax := X;
      end;

      LastUnicodeCategory := UnicodeCategory;

    end;

{$IFDEF USEKERNING}
    finally
      if (KerningPairs <> nil) then
        FreeMem(KerningPairs);
    end;
{$ENDIF}

    if ((Flags and DT_HORZ_ALIGN_MASK) in [DT_LEFT, DT_CENTER, DT_RIGHT]) then
      AlignLine(X, TextLen);

    YMax := Y + FontFaceMetric.Height - FontFaceMetric.Ascent;

    // Reverse HorizontalScale (if any) ...
    if (HorizontalScale <> 1) and (TextPath <> nil) then
      for I := 0 to High(TextPath.Path) do
        for J := 0 to High(TextPath.Path[I]) do
          TextPath.Path[I, J].X := TextPath.Path[I, J].X * HorizontalScaleInv;

    XMax := XMax * HorizontalScaleInv;

    X := ARect.Right - XMax;
    Y := ARect.Bottom - YMax;

    case (Flags and DT_HORZ_ALIGN_MASK) of
      DT_LEFT   : ARect := FloatRect(ARect.Left, ARect.Top, XMax, YMax);
      DT_CENTER : ARect := FloatRect(ARect.Left + X * 0.5, ARect.Top, XMax + X * 0.5, YMax);
      DT_RIGHT  : ARect := FloatRect(ARect.Left + X, ARect.Top, ARect.Right, YMax);
      DT_JUSTIFY: ARect := FloatRect(ARect.Left, ARect.Top, ARect.Right, YMax);
    end;

    if (Flags and (DT_VCENTER or DT_BOTTOM) <> 0) then
    begin
      if (Flags and DT_VCENTER <> 0) then
        Y := Y * 0.5;

      if (TextPath <> nil) then
        for I := 0 to High(TextPath.Path) do
          for J := 0 to High(TextPath.Path[I]) do
            TextPath.Path[I, J].Y := TextPath.Path[I, J].Y + Y;

      GR32.OffsetRect(ARect, 0, Y);
    end;

    if (Path <> nil) then
    begin
      TextPath.EndPath; // TODO : Why is this needed?

      if (Path <> TextPath) then
        Path.Assign(TextPath);

      TextPath.EndUpdate;
    end;

  finally
    OwnedPath.Free;
  end;
end;

//------------------------------------------------------------------------------

class procedure TextToolsWin.TextToPath(AFont: TFont; APath: TCustomPath; const ARect: TFloatRect; const AText: string; AFlags: Cardinal);
var
  R: TFloatRect;
  FontContext: IFontContext32;
begin
  FontContext := TFontContext.Create(AFont);

  FontContext.BeginSession;

  R := ARect;
  InternalTextToPath(FontContext, APath, R, AText, AFlags);

  FontContext.EndSession;
end;

//------------------------------------------------------------------------------

class function TextToolsWin.TextToPolyPolygon(AFont: TFont; const ARect: TFloatRect; const AText: string; AFlags: Cardinal): TArrayOfArrayOfFloatPoint;
var
  Path: TFlattenedPath;
begin
  Path := TFlattenedPath.Create;
  try

    TextToPath(AFont, Path, ARect, AText, AFlags);
    Result := Path.Path;

  finally
    Path.Free;
  end;
end;

//------------------------------------------------------------------------------

class function TextToolsWin.DoMeasureText(const FontContext: IFontContext32; const ARect: TFloatRect; const AText: string; AFlags: Cardinal): TFloatRect;
begin
  Result := ARect;
  InternalTextToPath(FontContext, nil, Result, AText, AFlags);
end;

//------------------------------------------------------------------------------

class function TextToolsWin.MeasureText(AFont: TFont; const ARect: TFloatRect; const AText: string; AFlags: Cardinal): TFloatRect;
var
  FontContext: IFontContext32;
begin
  FontContext := TFontContext.Create(AFont);

  Result := DoMeasureText(FontContext, ARect, AText, AFlags);
end;

//------------------------------------------------------------------------------

class procedure TextToolsWin.SetHinting(Value: TTextHinting);
{$ifdef FONT_CACHE}
var
  OldHinting: TTextHinting;
{$endif FONT_CACHE}
begin
{$ifdef FONT_CACHE}
  OldHinting := GetHinting;
{$endif FONT_CACHE}

  UseHinting := (Value <> thNone);

  if (Value = thNoHorz) then
    HorizontalScale := 16
  else
    HorizontalScale := 1;

  HorizontalScaleInv := 1 / HorizontalScale;
  VertFlip_mat2 := Default(TMat2);
  VertFlip_mat2.eM11.value := HorizontalScale;
  VertFlip_mat2.eM22.value := -1; // Reversed Y axis

{$ifdef FONT_CACHE}
  // Changing hinting invalidates cache
  if (FontCache <> nil) and (OldHinting <> GetHinting) then
    FontCache.Clear;
{$endif FONT_CACHE}
end;

class destructor TextToolsWin.Destroy;
begin
  if (FontDC <> 0) then
    DeleteDC(FontDC);
end;

class function TextToolsWin.GetHinting: TTextHinting;
begin
  if (HorizontalScale <> 1) then
    Result := thNoHorz
  else
  if UseHinting then
    Result := thHinting
  else
    Result := thNone;
end;

//------------------------------------------------------------------------------

procedure InitHinting;
begin
{$if defined(NOHORIZONTALHINTING)}
  TextToolsWin.SetHinting(thNoHorz);
{$elseif defined(NOHINTING)}
  TextToolsWin.SetHinting(thNone);
{$else}
  TextToolsWin.SetHinting(thHinting);
{$ifend}
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

{ TFontContext }

type
  TEqualityComparer32 = class(TEqualityComparer<IFontContext32>)
  public
    function Equals({$ifndef FPC}const{$else}constref{$endif} Left, Right: IFontContext32): Boolean; override;
{$ifndef FPC}
  function GetHashCode(const Value: IFontContext32): Integer; override;
{$else}
  function GetHashCode(constref Value: IFontContext32): UInt32; override;
{$endif}
  end;

{$ifndef FPC}
function TEqualityComparer32.Equals(const Left, Right: IFontContext32): Boolean;
begin
  Result := (BinaryCompare(@TFontContext(Left).FLogFont, @TFontContext(Right).FLogFont, SizeOf(TLogFont)) = 0);
end;
{$else}
function TEqualityComparer32.Equals(constref Left, Right: IFontContext32): Boolean;
begin
  Result := (TCompare.Binary(TFontContext(Left).FLogFont, TFontContext(Right).FLogFont, SizeOf(TLogFont)) = 0);
end;
{$endif}

{$ifndef FPC}
function TEqualityComparer32.GetHashCode(const Value: IFontContext32): Integer;
begin
  Result := THashBobJenkins.GetHashValue(TFontContext(Value).FLogFont, SizeOf(TLogFont));
end;
{$else}
function TEqualityComparer32.GetHashCode(constref Value: IFontContext32): UInt32;
begin
  Result := TDelphiHashFactory.GetHashCode(@TFontContext(Value).FLogFont, SizeOf(TLogFont));
end;
{$endif}

constructor TFontContext.Create(AFont: TFont);
var
  Size: integer;
  i: integer;
  Clear: boolean;
begin
  FFont := AFont;

  FLogFont := Default(TLogFont);

  Size := GetObject(FFont.Handle, SizeOf(TLogFont), @FLogFont);
  if (Size <> SizeOf(TLogFont)) then
    raise Exception.Create('Failed to retrieve LOGFONT');

  // Clear junk
  Clear := False;
  for i := 0 to High(FLogFont.lfFaceName) do
  begin
    if (Clear) then
      FLogFont.lfFaceName[i] := #0
    else
      Clear := (FLogFont.lfFaceName[i] = #0);
  end;
end;

class destructor TFontContext.Destroy;
begin
  if (FFontDC <> 0) then
    DeleteDC(FFontDC);
end;

function TFontContext.Comparer: IEqualityComparer<IFontContext32>;
begin
  Result := TEqualityComparer32.Create;
end;

procedure TFontContext.BeginSession;
begin
  Inc(FSessionCount);

  if (FSessionCount = 1) then
    SelectObject(FontDC, FFont.Handle);
end;

procedure TFontContext.EndSession;
begin
  Dec(FSessionCount);
end;

class function TFontContext.GetFontDC: HDC;
begin
  if (FFontDC = 0) then
    FFontDC := CreateCompatibleDC(0);
  Result := FFontDC;
end;

function TFontContext.GetGlyphMetric(AGlyph: Cardinal; var AGlyphMetrics: TGlyphMetrics32): boolean;
var
  GlyphMetrics: TGlyphMetrics;
  Res: DWORD;
begin
  Result := False;
  AGlyphMetrics := Default(TGlyphMetrics32);

  BeginSession;
  try

    GlyphMetrics := Default(TGlyphMetrics);
    Res := Windows.GetGlyphOutline(FontDC, AGlyph, GGODefaultFlags[UseHinting], GlyphMetrics, 0, nil, VertFlip_mat2);

    if (Res <> GDI_ERROR) then
    begin
      AGlyphMetrics.Valid := True;
      AGlyphMetrics.AdvanceWidthX := GlyphMetrics.gmCellIncX;
      Result := True;
    end;

  finally
    EndSession;
  end;
end;

function TFontContext.GetGlyphOutline(AGlyph: Cardinal; var AGlyphMetrics: TGlyphMetrics32; APath: TCustomPath; OffsetX, OffsetY, MaxX: Single): boolean;
var
  GlyphMetrics: TGlyphMetrics;
  PolygonHeaderAlloc: PTTPolygonHeader;
  PolygonHeader: PTTPolygonHeader;
  PolygonHeaderSize: DWORD;
  CurvePtr: PTTPolyCurve;
  P1, P2, P3, PNext: TFloatPoint;
  i, k: Integer;
  Size: Integer;
begin
  AGlyphMetrics := Default(TGlyphMetrics32);

  BeginSession;
  try

    GlyphMetrics := Default(TGlyphMetrics);
    PolygonHeaderSize := Windows.GetGlyphOutline(FontDC, AGlyph, GGODefaultFlags[UseHinting], GlyphMetrics, 0, nil, VertFlip_mat2);

    if (PolygonHeaderSize <> GDI_ERROR) then
    begin
      AGlyphMetrics.Valid := True;
      AGlyphMetrics.AdvanceWidthX := GlyphMetrics.gmCellIncX;
    end;

    Result := (APath <> nil) and (PolygonHeaderSize <> GDI_ERROR) and (PolygonHeaderSize <> 0) and
      ((MaxX < 0) or (OffsetX + AGlyphMetrics.AdvanceWidthX <= MaxX));

    if (not Result) then
      exit;

    GetMem(PolygonHeaderAlloc, PolygonHeaderSize);
    try
      PolygonHeader := PolygonHeaderAlloc;
      PolygonHeaderSize := Windows.GetGlyphOutline(FontDC, AGlyph, GGODefaultFlags[UseHinting], GlyphMetrics, PolygonHeaderSize, PolygonHeader, VertFlip_mat2);

      if (PolygonHeaderSize = GDI_ERROR) or (PolygonHeader.dwType <> TT_POLYGON_TYPE) then
        exit;

      // Batch each glyph so we're sure that the polygons are rendered as a whole (no pun...)
      // and not as individual independent polygons.
      // We're doing this here for completeness but since the path will also be batched at
      // an outer level it isn't really necessary here.
      APath.BeginUpdate;

      while (PolygonHeaderSize > 0) do
      begin
        Size := PolygonHeader.cb - SizeOf(TTTPolygonHeader);
        PByte(CurvePtr) := PByte(PolygonHeader) + SizeOf(TTTPolygonHeader);

        // First point is part of header
        P1 := PointFXtoPointF(PolygonHeader.pfxStart);
        P1.Offset(OffsetX, OffsetY);
        APath.MoveTo(P1);

        while (Size > 0) do
        begin
          case CurvePtr.wType of
            TT_PRIM_LINE:
              for i := 0 to CurvePtr.cpfx-1 do
              begin
                P1 := PointFXtoPointF(CurvePtr.apfx[i]);
                P1.Offset(OffsetX, OffsetY);

                APath.LineTo(P1);
              end;

            TT_PRIM_QSPLINE:
              if (CurvePtr.cpfx > 1) then
              begin
                PNext := PointFXtoPointF(CurvePtr.apfx[0]);
                PNext.Offset(OffsetX, OffsetY);

                for i := 0 to CurvePtr.cpfx-2 do
                begin
                  P1 := PNext;

                  P2 := PointFXtoPointF(CurvePtr.apfx[i+1]);
                  P2.Offset(OffsetX, OffsetY);

                  PNext := P2;

                  if (i < CurvePtr.cpfx-2) then
                  begin
                    P2.x := (P1.x + P2.x) * 0.5;
                    P2.y := (P1.y + P2.y) * 0.5;
                  end;

                  APath.ConicTo(P1, P2);
                end;
              end;

            TT_PRIM_CSPLINE:
              if (CurvePtr.cpfx > 2) then
              begin
                PNext := PointFXtoPointF(CurvePtr.apfx[0]);
                PNext.Offset(OffsetX, OffsetY);

                i := 0;
                while (i < CurvePtr.cpfx-2) do
                begin
                  P1 := PNext;

                  P2 := PointFXtoPointF(CurvePtr.apfx[i+1]);
                  P2.Offset(OffsetX, OffsetY);

                  P3 := PointFXtoPointF(CurvePtr.apfx[i+2]);
                  P3.Offset(OffsetX, OffsetY);

                  PNext := P3;

                  APath.CurveTo(P1, P2, P3);

                  Inc(i, 2);
                end;
              end;
          end;

          k := (CurvePtr.cpfx-1) * SizeOf(TPointFX) + SizeOf(TTPolyCurve);
          Dec(Size, k);

          Inc(PByte(CurvePtr), k);
        end;

        APath.EndPath(True);

        Dec(PolygonHeaderSize, PolygonHeader.cb);
        Inc(PByte(PolygonHeader), PolygonHeader.cb);
      end;

      APath.EndUpdate;

    finally
      FreeMem(PolygonHeaderAlloc);
    end;

  finally
    EndSession;
  end;

  Result := True;
end;

function TFontContext.GetFontFaceMetric(var AFontFaceMetric: TFontFaceMetric32): boolean;
var
  TextMetric: TTextMetric;
begin
  BeginSession;
  try

    Result := GetTextMetrics(FontDC, TextMetric);

  finally
    EndSession;
  end;

  if (Result) then
  begin
    AFontFaceMetric.Valid := True;
    AFontFaceMetric.Height := TextMetric.tmHeight;
    AFontFaceMetric.Ascent := TextMetric.tmAscent;
    AFontFaceMetric.Descent := TextMetric.tmDescent;
  end else
    AFontFaceMetric := Default(TFontFaceMetric32);
end;

{ TFontOutlineProvider }

function TFontOutlineProvider.CreateContext(AFont: TFont): IFontContext32;
begin
  Result := TFontContext.Create(AFont);
end;


initialization
  InitHinting;

finalization

end.
