unit GR32_Text_LCL_Win;

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
 * The Original Code is Vectorial Polygon Rasterizer for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Mattias Andersson <mattias@centaurix.com>
 *
 * Portions created by the Initial Developer are Copyright (C) 2012
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  Windows, Types, GR32, GR32_Paths;

procedure TextToPath(Font: HFONT; Path: TCustomPath;
  const ARect: TFloatRect; const Text: WideString; Flags: Cardinal); overload;
function MeasureTextDC(DC: HDC; const ARect: TFloatRect; const Text: WideString;
  Flags: Cardinal): TFloatRect; overload;
function MeasureText(Font: HFONT; const ARect: TFloatRect; const Text: WideString;
  Flags: Cardinal): TFloatRect;

var
  UseHinting: Boolean = {$IFDEF NOHINTING}False{$ELSE}True{$ENDIF};

const
  DT_CENTER = 1;      //See also Window's DrawText() flags ...
  DT_RIGHT = 2;       //http://msdn.microsoft.com/en-us/library/ms901121.aspx
  DT_WORDBREAK = $10;
  DT_JUSTIFY = 3;     //Graphics32 additions ...
  DT_ALIGN_MASK = 3;

implementation

uses
  GR32_LowLevel;

const
  GGO_UNHINTED = $0100;
  GGODefaultFlags: array [Boolean] of Integer = (GGO_NATIVE or GGO_UNHINTED, GGO_NATIVE);

  TT_PRIM_CSPLINE = 3;

{$IFDEF NOHORIZONTALHINTING}
// stretching factor when calling GetGlyphOutline()
  HORZSTRETCH = 16;
{$ENDIF}

  VertFlip_mat2: tmat2 = (
    eM11: (fract: 0; Value: {$IFNDEF NOHORIZONTALHINTING}1{$ELSE}HORZSTRETCH{$ENDIF});
    eM12: (fract: 0; Value: 0);
    eM21: (fract: 0; Value: 0);
    eM22: (fract: 0; Value: -1);
  );

function PointFXtoPointF(const Point: tagPointFX): TFloatPoint;
begin
  Result.X := Point.X.Value + Point.X.Fract * FixedToFloat;
  Result.Y := Point.Y.Value + Point.Y.Fract * FixedToFloat;
end;


function GlyphOutlineToPath(Handle: HDC; Path: TCustomPath;
  DstX, MaxX, DstY: Single;
  const Glyph: Integer; out Metrics: TGlyphMetrics): Boolean;
var
  I, K, S: Integer;
  Res: DWORD;
  GlyphMemPtr, BufferPtr: PTTPolygonHeader;
  CurvePtr: PTTPolyCurve;
  P1, P2, P3: TFloatPoint;
begin
  Res := GetGlyphOutlineW(Handle, Glyph, GGODefaultFlags[UseHinting], Metrics,
    0, nil, VertFlip_mat2);
  Result := DstX + Metrics.gmCellIncX <= MaxX;
  if not Result or not Assigned(Path) then Exit;

  GetMem(GlyphMemPtr, Res);
  BufferPtr := GlyphMemPtr;

  Res := GetGlyphOutlineW(Handle, Glyph, GGODefaultFlags[UseHinting], Metrics,
    Res, BufferPtr, VertFlip_mat2);

  if (Res = GDI_ERROR) or (BufferPtr^.dwType <> TT_POLYGON_TYPE) then
  begin
    FreeMem(GlyphMemPtr);
    Exit;
  end;

  while Res > 0 do
  begin
    S := BufferPtr.cb - SizeOf(TTTPolygonHeader);
    PtrUInt(CurvePtr) := PtrUInt(BufferPtr) + SizeOf(TTTPolygonHeader);
    P1 := PointFXtoPointF(BufferPtr.pfxStart);
    Path.MoveTo(P1.X + DstX, P1.Y + DstY);
    while S > 0 do
    begin
      case CurvePtr.wType of
        TT_PRIM_LINE:
          for I := 0 to CurvePtr.cpfx - 1 do
          begin
            P1 := PointFXtoPointF(CurvePtr.apfx[I]);
            Path.LineTo(P1.X + DstX, P1.Y + DstY);
          end;
        TT_PRIM_QSPLINE:
          begin
            for I := 0 to CurvePtr.cpfx - 2 do
            begin
              P1 := PointFXtoPointF(CurvePtr.apfx[I]);
              if I < CurvePtr.cpfx - 2 then
                with PointFXtoPointF(CurvePtr.apfx[I + 1]) do
                begin
                  P2.x := (P1.x + x) * 0.5;
                  P2.y := (P1.y + y) * 0.5;
                end
              else
                P2 := PointFXtoPointF(CurvePtr.apfx[I + 1]);
              Path.ConicTo(P1.X + DstX, P1.Y + DstY, P2.X + DstX, P2.Y + DstY);
            end;
          end;
        TT_PRIM_CSPLINE:
          begin
            I := 0;
            while I < CurvePtr.cpfx - 2 do
            begin
              P1 := PointFXtoPointF(CurvePtr.apfx[I]);
              P2 := PointFXtoPointF(CurvePtr.apfx[I + 1]);
              P3 := PointFXtoPointF(CurvePtr.apfx[I + 2]);
              Path.CurveTo(P1.X + DstX, P1.Y + DstY, P2.X + DstX, P2.Y + DstY,
                P3.X + DstX, P3.Y + DstY);
              Inc(I, 2);
            end;
          end;
      end;
      K := (CurvePtr.cpfx - 1) * SizeOf(TPointFX) + SizeOf(TTPolyCurve);
      Dec(S, K);

      Inc(PtrInt(CurvePtr), K);
    end;

    Path.ClosePath;

    Dec(Res, BufferPtr.cb);
    Inc(PtrInt(BufferPtr), BufferPtr.cb);
  end;

  FreeMem(GlyphMemPtr);
end;

procedure InternalTextToPath(DC: HDC; Path: TCustomPath; var ARect: TFloatRect;
  const Text: WideString; Flags: Cardinal);
const
  CHAR_CR = 10;
  CHAR_NL = 13;
  CHAR_SP = 32;
var
  GlyphMetrics: TGlyphMetrics;
  TextMetric: TTextMetric;
  I, J, TextLen, SpcCount, LineStart: Integer;
  CharValue: Integer;
  CharOffsets: TArrayOfInteger;
  X, Y, XMax: Single;
  S: WideString;
  TmpPath: TFlattenedPath;

  procedure AlignTextCenter(CurrentI: Integer);
  var
    M, N, PathStart, PathEnd: Integer;
    Delta: TFloat;
  begin
    if not assigned(TmpPath) then Exit;
    Delta := (ARect.Right - X)/ 2;
    PathStart := CharOffsets[LineStart - 1];
    PathEnd := CharOffsets[CurrentI - 1];
    for M := PathStart to PathEnd - 1 do
      for N := 0 to High(TmpPath.Path[M]) do
        TmpPath.Path[M][N].X := TmpPath.Path[M][N].X + Delta;
  end;

  procedure AlignTextRight(CurrentI: Integer);
  var
    M, N, PathStart, PathEnd: Integer;
    Delta: TFloat;
  begin
    if not assigned(TmpPath) then Exit;
    Delta := (ARect.Right - X);
    PathStart := CharOffsets[LineStart - 1];
    PathEnd := CharOffsets[CurrentI - 1];
    for M := PathStart to PathEnd - 1 do
      for N := 0 to High(TmpPath.Path[M]) do
        TmpPath.Path[M][N].X := TmpPath.Path[M][N].X + Delta;
  end;

  procedure AlignTextJustify(CurrentI: Integer);
  var
    L, M, N, PathStart, PathEnd: Integer;
    SpcDelta, SpcDeltaInc: TFloat;
  begin
    if not assigned(TmpPath) or (SpcCount < 2) or
      // Don't justify lines ending with a carriage return ...
      (Ord(Text[CurrentI]) = CHAR_CR) then Exit;
    SpcDelta := (ARect.Right - X)/ (SpcCount - 1);
    SpcDeltaInc := SpcDelta;
    L := LineStart;
    while (L < CurrentI) and (Ord(Text[L]) <> CHAR_SP) do Inc(L);
    PathStart := CharOffsets[L - 1];
    repeat
      M := L + 1;
      while (M < CurrentI) and (Ord(Text[M]) <> CHAR_SP) do Inc(M);
      PathEnd := CharOffsets[M - 1];
      L := M;
      for M := PathStart to PathEnd - 1 do
        for N := 0 to High(TmpPath.Path[M]) do
          TmpPath.Path[M][N].X := TmpPath.Path[M][N].X + SpcDeltaInc;
      SpcDeltaInc := SpcDeltaInc + SpcDelta;
      PathStart := PathEnd;
    until L >= CurrentI;
  end;

  procedure NewLine;
  begin
    case (Flags and DT_ALIGN_MASK) of
      1: AlignTextCenter(I);
      2: AlignTextRight(I);
      3: AlignTextJustify(I);
    end;
    X := ARect.Left{$IFDEF NOHORIZONTALHINTING} * HORZSTRETCH{$ENDIF};
    Y := Y + TextMetric.tmHeight;
    LineStart := I + 1;
    SpcCount := 0;
  end;

  function MeasureTextX(const S: WideString): Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 1 to Length(S) do
    begin
      CharValue := Ord(S[I]);
      GetGlyphOutlineW(DC, CharValue,
        GGODefaultFlags[UseHinting], GlyphMetrics, 0, nil, VertFlip_mat2);
      Inc(Result, GlyphMetrics.gmCellIncX);
    end;
  end;

  procedure TestNewLine(X: Single);
  begin
    if X > ARect.Right{$IFDEF NOHORIZONTALHINTING} * HORZSTRETCH{$ENDIF} then
      NewLine;
  end;

begin
  SpcCount := 0;
  LineStart := 1;
  if Assigned(Path) then
    TmpPath := TFlattenedPath.Create
  else
    TmpPath := nil;

  GetTextMetrics(DC, TextMetric);
  TextLen := Length(Text);
  X := ARect.Left {$IFDEF NOHORIZONTALHINTING} * HORZSTRETCH{$ENDIF};
  Y := ARect.Top + TextMetric.tmAscent;
  XMax := X;
  SetLength(CharOffsets, TextLen);
  for I := 1 to TextLen do
  begin
    if Assigned(TmpPath) then
      CharOffsets[I - 1] := Length(TmpPath.Path);

    CharValue := Ord(Text[I]);
    if CharValue <= 32 then
    begin
      case CharValue of
        CHAR_CR: NewLine;
        CHAR_NL: ;
        CHAR_SP:
          begin
            Inc(SpcCount);
            GetGlyphOutlineW(DC, CharValue, GGODefaultFlags[UseHinting],
              GlyphMetrics{%H-}, 0, nil, VertFlip_mat2);
            X := X + GlyphMetrics.gmCellIncX;

            if Flags and DT_WORDBREAK <> 0 then
            begin
              J := I + 1;
              while (J <= TextLen) and
                ([Ord(Text[J])] * [CHAR_CR, CHAR_NL, CHAR_SP] = []) do
                  Inc(J);
              S := Copy(Text, I + 1, J - I - 1);
              TestNewLine(X + MeasureTextX(S));
            end;
          end;
      end;
    end
    else
    begin
      //a word may still need to be split if it fills more that a whole line ...
      if not GlyphOutlineToPath(DC, TmpPath,
        X, ARect.Right, Y, CharValue, GlyphMetrics) then
      begin
        NewLine;
        if not GlyphOutlineToPath(DC, TmpPath,
          X, ARect.Right, Y, CharValue, GlyphMetrics) then Exit;
      end;
      X := X + GlyphMetrics.gmCellIncX;
      if X > XMax then XMax := X;
    end;
  end;
  case (Flags and DT_ALIGN_MASK) of
    1: AlignTextCenter(TextLen);
    2: AlignTextRight(TextLen);
  end;
  Y := Y + TextMetric.tmHeight - TextMetric.tmAscent;

{$IFNDEF NOHORIZONTALHINTING}
  ARect := FloatRect(ARect.Left, ARect.Top, XMax, Y);
{$ELSE}
  ARect := FloatRect(ARect.Left, ARect.Top, XMax / HORZSTRETCH, Y);
{$ENDIF}

  if Assigned(TmpPath) then
  begin
    Path.Assign(TmpPath);
    TmpPath.Free;
  end;
end;

function MeasureTextDC(DC: HDC; const ARect: TFloatRect; const Text: WideString;
  Flags: Cardinal): TFloatRect;
begin
  Result := ARect;
  InternalTextToPath(DC, nil, Result, Text, Flags);
end;

function MeasureTextDC(DC: HDC;
  const ARect: TFloatRect; const Text: WideString;
  WordWrap: Boolean; Flags: Cardinal): TFloatRect; overload;
begin
  Result := MeasureTextDC(DC, ARect, Text, Flags);

  if Flags and DT_CENTER <> 0 then
    OffsetRect(Result, (((ARect.Left + ARect.Right) - (Result.Left + Result.Right)) * 0.5), 0);
  if Flags and DT_RIGHT <> 0 then
    OffsetRect(Result, ARect.Right - Result.Right, 0);

  if Flags and DT_VCENTER <> 0 then
    OffsetRect(Result, 0, ((ARect.Top + ARect.Bottom) - (Result.Top + Result.Bottom)) * 0.5);
  if Flags and DT_BOTTOM <> 0 then
    OffsetRect(Result, 0, ARect.Bottom - Result.Bottom);

  Result.Left := Round(Result.Left);
  Result.Top := Round(Result.Top);
  Result.Right := Round(Result.Right);
  Result.Bottom := Round(Result.Bottom);
end;

function MeasureText(Font: HFONT; const ARect: TFloatRect;
  const Text: WideString; Flags: Cardinal): TFloatRect;
var
  DC: HDC;
  SavedFont: HFONT;
begin
  DC := GetDC(0);
  try
    SavedFont := SelectObject(DC, Font);
    Result := MeasureTextDC(DC, ARect, Text, Flags);
    SelectObject(DC, SavedFont);
  finally
    ReleaseDC(0, DC);
  end;
end;

procedure TextToPath(Font: HFONT; Path: TCustomPath; const ARect: TFloatRect;
  const Text: WideString; Flags: Cardinal); overload;
var
  DC: HDC;
  SavedFont: HFONT;
  R: TFloatRect;
begin
  DC := GetDC(0);
  try
    SavedFont := SelectObject(DC, Font);
    R := ARect;
    InternalTextToPath(DC, Path, R, Text, Flags);
    SelectObject(DC, SavedFont);
  finally
    ReleaseDC(0, DC);
  end;
end;

end.
