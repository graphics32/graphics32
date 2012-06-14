unit GR32_Text_VCL;

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

{$I GR32.INC}

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

implementation

uses
  GR32_LowLevel;

// stretching factor when calling GetGlyphOutline()
const
  HORZSTRETCH = 16;

type
  PBezierVertex = ^TBezierVertex;
  TBezierVertex = record
    case Integer of
      0: (Point: TFloatPoint; ControlPoints: array [0..1] of TFloatPoint);
      1: (Points: array [0..2] of TFloatPoint);
  end;

const
  GGO_UNHINTED = $0100;
  GGODefaultFlags: array [Boolean] of Integer = (GGO_NATIVE or GGO_UNHINTED, GGO_NATIVE);

  TT_PRIM_CSPLINE = 3;

const
  FixedOne = $10000;

  VertFlip_mat2: tmat2 = (
    eM11: (fract: 0; Value: {$IFNDEF NOHORIZONTALHINTING}1{$ELSE}HORZSTRETCH{$ENDIF});
    eM12: (fract: 0; Value: 0);
    eM21: (fract: 0; Value: 0);
    eM22: (fract: 0; Value: -1);
  );

function QuadToBezier(Q0, Q1, Q2: TFloatPoint): TBezierVertex;
// Q-spline to Bezier curve:
// B0 = Q0
// B1 = (Q0 + 2*Q1) / 3
// B2 = (Q0 + 2*Q2) / 3
begin
  with Result do
  begin
    Points[0] := Q0;
    Points[1].X := (Q0.X + 2*Q1.X) / 3;
    Points[1].Y := (Q0.Y + 2*Q1.Y) / 3;
    Points[2].X := (Q0.X + 2*Q2.X) / 3;
    Points[2].Y := (Q0.Y + 2*Q2.Y) / 3;
  end;
end;

procedure OffsetVertex(var Vertex: TBezierVertex; const Dx, Dy: Single);
begin
  Vertex.Points[0].X := Vertex.Points[0].X + Dx;
  Vertex.Points[0].Y := Vertex.Points[0].Y + Dy;
  Vertex.Points[1].X := Vertex.Points[1].X + Dx;
  Vertex.Points[1].Y := Vertex.Points[1].Y + Dy;
  Vertex.Points[2].X := Vertex.Points[2].X + Dx;
  Vertex.Points[2].Y := Vertex.Points[2].Y + Dy;
end;

function PointFXtoPointF(const Point: tagPointFX): TFloatPoint;
begin
  Result.X := Point.X.Value + Point.X.Fract * FixedToFloat;
  Result.Y := Point.Y.Value + Point.Y.Fract * FixedToFloat;
end;


{$IFDEF USESTACKALLOC}
{$W+}
{$ENDIF}
procedure GlyphOutlineToPath(Handle: HDC; Path: TCustomPath; DstX, DstY: Single;
  const Glyph: Integer; out Metrics: TGlyphMetrics);
var
  I, K, S: Integer;
  Res: DWORD;
  PGlyphMem, PBuffer: PTTPolygonHeader;
  PPCurve: PTTPolyCurve;
  P1, P2, P3: TFloatPoint;
begin
  Res := GetGlyphOutlineW(Handle, Glyph, GGODefaultFlags[UseHinting], Metrics, 0, nil, VertFlip_mat2);
  if not Assigned(Path) then Exit;

  PGlyphMem := StackAlloc(Res);
  PBuffer := PGlyphMem;

  Res := GetGlyphOutlineW(Handle, Glyph, GGODefaultFlags[UseHinting], Metrics, Res, PBuffer, VertFlip_mat2);

  if (Res = GDI_ERROR) or (PBuffer^.dwType <> TT_POLYGON_TYPE) then
  begin
    StackFree(PGlyphMem);
    Exit;
  end;

  while Res > 0 do
  begin
    S := PBuffer.cb - SizeOf(TTTPolygonHeader);
    {$IFDEF HAS_NATIVEINT}
    NativeInt(PPCurve) := NativeInt(PBuffer) + SizeOf(TTTPolygonHeader);
    {$ELSE}
    Integer(PPCurve) := Integer(PBuffer) + SizeOf(TTTPolygonHeader);
    {$ENDIF}
    P1 := PointFXtoPointF(PBuffer.pfxStart);
    Path.MoveTo(P1.X + DstX, P1.Y + DstY);
    while S > 0 do
    begin
      case PPCurve.wType of
        TT_PRIM_LINE:
          for I := 0 to PPCurve.cpfx - 1 do
          begin
            P1 := PointFXtoPointF(PPCurve.apfx[I]);
            Path.LineTo(P1.X + DstX, P1.Y + DstY);
          end;
        TT_PRIM_QSPLINE:
          begin
            for I := 0 to PPCurve.cpfx - 2 do
            begin
              P1 := PointFXtoPointF(PPCurve.apfx[I]);
              if I < PPCurve.cpfx - 2 then
                with PointFXtoPointF(PPCurve.apfx[I + 1]) do
                begin
                  P2.x := (P1.x + x) * 0.5;
                  P2.y := (P1.y + y) * 0.5;
                end
              else
                P2 := PointFXtoPointF(PPCurve.apfx[I + 1]);
              Path.ConicTo(P1.X + DstX, P1.Y + DstY, P2.X + DstX, P2.Y + DstY);
            end;
          end;
        TT_PRIM_CSPLINE:
          begin
            I := 0;
            while I < PPCurve.cpfx - 2 do
            begin
              P1 := PointFXtoPointF(PPCurve.apfx[I]);
              P2 := PointFXtoPointF(PPCurve.apfx[I + 1]);
              P3 := PointFXtoPointF(PPCurve.apfx[I + 2]);
              Path.CurveTo(P1.X + DstX, P1.Y + DstY, P2.X + DstX, P2.Y + DstY, P3.X + DstX, P3.Y + DstY);
              Inc(I, 2);
            end;
          end;
      end;
      K := (PPCurve.cpfx - 1) * SizeOf(TPointFX) + SizeOf(TTPolyCurve);
      Dec(S, K);

      {$IFDEF HAS_NATIVEINT}
      Inc(NativeInt(PPCurve), K);
      {$ELSE}
      Inc(integer(PPCurve), K);
      {$ENDIF}
    end;

    Path.ClosePath;

    Dec(Res, PBuffer.cb);
    {$IFDEF HAS_NATIVEINT}
    Inc(NativeInt(PBuffer), PBuffer.cb);
    {$ELSE}
    Inc(integer(PBuffer), PBuffer.cb);
    {$ENDIF}
  end;

  StackFree(PGlyphMem);
end;
{$IFDEF USESTACKALLOC}
{$W-}
{$ENDIF}


procedure InternalTextToPath(DC: HDC; Path: TCustomPath; var ARect: TFloatRect;
  const Text: WideString; Flags: Cardinal);
const
  CHAR_CR = 10;
  CHAR_NL = 13;
  CHAR_SP = 32;
var
  GlyphMetrics: TGlyphMetrics;
  TextMetric: TTextMetric;
  I, J, TextLen: Integer;
  CharValue: Integer;
  X, Y, XMax: Single;
  S: WideString;

  procedure NewLine;
  begin
    X := ARect.Left{$IFDEF NOHORIZONTALHINTING}*HORZSTRETCH{$ENDIF};
    Y := Y + TextMetric.tmHeight;
  end;

  function MeasureTextX(const S: WideString): Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 1 to Length(S) do
    begin
      CharValue := Ord(S[I]);
      GetGlyphOutlineW(DC, CharValue, GGODefaultFlags[UseHinting], GlyphMetrics, 0, nil, VertFlip_mat2);
      Inc(Result, GlyphMetrics.gmCellIncX);
    end;
  end;

  procedure TestNewLine(X: Single);
  begin
    if X > ARect.Right{$IFDEF NOHORIZONTALHINTING}*HORZSTRETCH{$ENDIF} then
      NewLine;
  end;

begin
  if Assigned(Path) then Path.BeginPath;
  GetTextMetrics(DC, TextMetric);

  TextLen := Length(Text);
  X := ARect.Left {$IFDEF NOHORIZONTALHINTING}*HORZSTRETCH{$ENDIF};
  Y := ARect.Top + TextMetric.tmAscent;
  XMax := X;
  for I := 1 to TextLen do
  begin
    CharValue := Ord(Text[I]);
    if CharValue <= 32 then
    begin
      case CharValue of
        CHAR_CR: X := ARect.Left;
        CHAR_NL: Y := Y + TextMetric.tmHeight;
        CHAR_SP:
          begin
            GetGlyphOutlineW(DC, CharValue, GGODefaultFlags[UseHinting], GlyphMetrics, 0, nil, VertFlip_mat2);
            X := X + GlyphMetrics.gmCellIncX;

            if Flags and DT_WORDBREAK <> 0 then
            begin
              J := I + 1;
              while (J <= TextLen) and ([Ord(Text[J])] * [CHAR_CR, CHAR_NL, CHAR_SP] = []) do
                Inc(J);
              S := Copy(Text, I + 1, J - I - 1);
              TestNewLine(X + MeasureTextX(S));
            end;
          end;
      end;
    end
    else
    begin
      //TestNewLine(X);

      GlyphOutlineToPath(DC, Path, X, Y, CharValue, GlyphMetrics);
      X := X + GlyphMetrics.gmCellIncX;
      if X > XMax then XMax := X;
    end;
  end;
  Y := Y + TextMetric.tmHeight - TextMetric.tmAscent;

{$IFNDEF NOHORIZONTALHINTING}
  ARect := FloatRect(ARect.Left, ARect.Top, XMax, Y);
{$ELSE}
  ARect := FloatRect(ARect.Left, ARect.Top, XMax/HORZSTRETCH, Y);
  if Assigned(Path) then
    Path.Scale(1/HORZSTRETCH, 1);
{$ENDIF}
  if Assigned(Path) then Path.EndPath;
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
  savedFont: HFONT;
begin
  DC := GetDC(0);
  try
    savedFont := SelectObject(DC, Font);
    Result := MeasureTextDC(DC, ARect, Text, Flags);
    SelectObject(DC, savedFont);
  finally
    ReleaseDC(0, DC);
  end;
end;

procedure TextToPath(Font: HFONT; Path: TCustomPath; const ARect: TFloatRect;
  const Text: WideString; Flags: Cardinal); overload;
var
  DC: HDC;
  savedFont: HFONT;
  R: TFloatRect;
begin
  DC := GetDC(0);
  try
    savedFont := SelectObject(DC, Font);
    R := MeasureTextDC(DC, ARect, Text, Flags);
    InternalTextToPath(DC, Path, R, Text, Flags);
    SelectObject(DC, savedFont);
  finally
    ReleaseDC(0, DC);
  end;
end;

end.
