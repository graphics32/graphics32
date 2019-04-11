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

{$I GR32.inc}

uses
  Windows, Types, GR32, GR32_Paths, Math;

procedure TextToPath(Font: HFONT; Path: TCustomPath;
  const ARect: TFloatRect; const Text: WideString; Flags: Cardinal = 0);
function TextToPolyPolygon(Font: HFONT; const ARect: TFloatRect;
  const Text: WideString; Flags: Cardinal = 0): TArrayOfArrayOfFloatPoint;

function MeasureTextDC(DC: HDC; const ARect: TFloatRect; const Text: WideString;
  Flags: Cardinal = 0): TFloatRect; overload;
function MeasureText(Font: HFONT; const ARect: TFloatRect; const Text: WideString;
  Flags: Cardinal = 0): TFloatRect;

type
  TTextHinting = (thNone, thNoHorz, thHinting);

  TKerningPairArray = array [0..0] of TKerningPair;
  PKerningPairArray = ^TKerningPairArray;

procedure SetHinting(Value: TTextHinting);
function GetHinting: TTextHinting;

const
  DT_LEFT       = 0;   //See also Window's DrawText() flags ...
  DT_CENTER     = 1;   //http://msdn.microsoft.com/en-us/library/ms901121.aspx
  DT_RIGHT      = 2;
  DT_VCENTER    = 4;
  DT_BOTTOM     = 8;
  DT_WORDBREAK  = $10;
  DT_SINGLELINE = $20;
  DT_NOCLIP     = $100;
  DT_JUSTIFY         = 3;  //Graphics32 additions ...
  DT_HORZ_ALIGN_MASK = 3;

implementation

uses
{$IFDEF USESTACKALLOC}
  GR32_LowLevel,
{$ENDIF}
  SysUtils;

var
  UseHinting: Boolean;
  HorzStretch: Integer; // stretching factor when calling GetGlyphOutline()
  HorzStretch_Inv: Single;

  VertFlip_mat2: TMat2;

const
  GGO_UNHINTED = $0100;
  GGODefaultFlags: array [Boolean] of Integer = (GGO_NATIVE or GGO_UNHINTED, GGO_NATIVE);

  TT_PRIM_CSPLINE = 3;

  MaxSingle   =  3.4e+38;

// import GetKerningPairs from gdi32 library
function GetKerningPairs(DC: HDC; Count: DWORD; P: PKerningPair): DWORD;
  stdcall; external gdi32 name 'GetKerningPairs';

function PointFXtoPointF(const Point: tagPointFX): TFloatPoint; {$IFDEF UseInlining} inline; {$ENDIF}
begin
  Result.X := Point.X.Value + Point.X.Fract * FixedToFloat;
  Result.Y := Point.Y.Value + Point.Y.Fract * FixedToFloat;
end;


{$IFDEF USESTACKALLOC}
{$W+}
{$ENDIF}
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
  if (Res = 0) then Exit;

  Result := DstX + Metrics.gmCellIncX <= MaxX;
  if not Result or not Assigned(Path) then Exit;

  {$IFDEF USESTACKALLOC}
  GlyphMemPtr := StackAlloc(Res);
  {$ELSE}
  GetMem(GlyphMemPtr, Res);
  {$ENDIF}
  BufferPtr := GlyphMemPtr;

  Res := GetGlyphOutlineW(Handle, Glyph, GGODefaultFlags[UseHinting], Metrics,
    Res, BufferPtr, VertFlip_mat2);

  if (Res = GDI_ERROR) or (BufferPtr^.dwType <> TT_POLYGON_TYPE) then
  begin
    {$IFDEF USESTACKALLOC}
    StackFree(GlyphMemPtr);
    {$ELSE}
    FreeMem(GlyphMemPtr);
    {$ENDIF}
    Exit;
  end;

  // Batch each glyph so we're sure that the polygons are rendered as a whole (no pun...)
  // and not as individual independent polygons.
  // We're doing this here for completeness but since the path will also be batched at
  // an outer level it isn't really necessary here.
  Path.BeginUpdate;

  while Res > 0 do
  begin
    S := BufferPtr.cb - SizeOf(TTTPolygonHeader);
    {$IFDEF HAS_NATIVEINT}
    NativeInt(CurvePtr) := NativeInt(BufferPtr) + SizeOf(TTTPolygonHeader);
    {$ELSE}
    Integer(CurvePtr) := Integer(BufferPtr) + SizeOf(TTTPolygonHeader);
    {$ENDIF}
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

      {$IFDEF HAS_NATIVEINT}
      Inc(NativeInt(CurvePtr), K);
      {$ELSE}
      Inc(Integer(CurvePtr), K);
      {$ENDIF}
    end;

    Path.EndPath(True);

    Dec(Res, BufferPtr.cb);
    {$IFDEF HAS_NATIVEINT}
    Inc(NativeInt(BufferPtr), BufferPtr.cb);
    {$ELSE}
    Inc(integer(BufferPtr), BufferPtr.cb);
    {$ENDIF}
  end;

  {$IFDEF USESTACKALLOC}
  StackFree(GlyphMemPtr);
  {$ELSE}
  FreeMem(GlyphMemPtr);
  {$ENDIF}

  Path.EndUpdate;
end;
{$IFDEF USESTACKALLOC}
{$W-}
{$ENDIF}


procedure InternalTextToPath(DC: HDC; Path: TCustomPath; const ARect: TFloatRect;
  const Text: WideString; Flags: Cardinal = 0);
const
  CHAR_CR = 10;
  CHAR_NL = 13;
  CHAR_SP = 32;
var
  GlyphMetrics: TGlyphMetrics;
  TextMetric: TTextMetric;
  I, J, TextLen, SpcCount, SpcX, LineStart: Integer;
  CharValue: Integer;
  CharOffsets: TArrayOfInteger;
  CharWidths: TArrayOfInteger;
  X, Y, XMax, YMax, MaxRight: Single;
  S: WideString;
  TextPath: TFlattenedPath;
  OwnedPath: TFlattenedPath;
{$IFDEF USEKERNING}
  NextCharValue: Integer;
  KerningPairs: PKerningPairArray;
  KerningPairCount: Integer;
{$ENDIF}

  procedure AlignTextCenter(CurrentI: Integer);
  var
    w, M, N, PathStart, PathEnd, CharStart, CharEnd: Integer;
    Delta: TFloat;
    i: Integer;
    MinX, MaxX: Single;
  begin
    Delta := Round(((ARect.Right - ARect.Left) * HorzStretch - X - 1) * 0.5);
    PathStart := CharOffsets[LineStart];
    PathEnd := CharOffsets[CurrentI] - 1;
    if (Flags and DT_SINGLELINE <> 0) and (Flags and DT_NOCLIP <> DT_NOCLIP) then
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
        if (CharStart > LineStart) and (Text[CharStart] <> ' ') then
          while (Text[CharStart] <> ' ') and (CharStart < CharEnd) do
            Inc(CharStart);
        if (CharEnd < CurrentI) and (Text[CharEnd] <> ' ') then
          while (Text[CharEnd] <> ' ') and (CharEnd > CharStart) do
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

  procedure AlignTextRight(CurrentI: Integer);
  var
    w, i, M, N, PathStart, PathEnd, CharStart: Integer;
    Delta: TFloat;
  begin
    Delta := Round(ARect.Right * HorzStretch - X - 1);
    PathStart := CharOffsets[LineStart];
    PathEnd := CharOffsets[CurrentI] - 1;

    if (Flags and DT_SINGLELINE <> 0) and (Flags and DT_NOCLIP <> DT_NOCLIP) then
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
        if (CharStart > LineStart) and (Text[CharStart] <> ' ') then
          while (Text[CharStart] <> ' ') and (CharStart < CurrentI) do
            Inc(CharStart);

      PathStart := CharOffsets[CharStart];

      for M := 0 to PathStart - 1 do
        SetLength(TextPath.Path[M], 0);
    end;

    for M := PathStart to PathEnd do
      for N := 0 to High(TextPath.Path[M]) do
        TextPath.Path[M, N].X := TextPath.Path[M, N].X + Delta;
  end;

  procedure AlignTextLeft(CurrentI: Integer);
  var
    w, i, M, PathEnd, CharEnd: Integer;
  begin
    if (Flags and DT_SINGLELINE <> 0) and (Flags and DT_NOCLIP <> DT_NOCLIP) then
    begin
      CharEnd := LineStart;

      w := 0;
      for i := LineStart to CurrentI - 1 do
      begin
        w := w + CharWidths[i];
        if w <= (ARect.Right - ARect.Left) then
          CharEnd := i + 1;
      end;

      if (Flags and DT_WORDBREAK <> 0) then
        if (CharEnd < CurrentI) and (Text[CharEnd] <> ' ') then
          while (Text[CharEnd] <> ' ') and (CharEnd > LineStart) do
            Dec(CharEnd);

      PathEnd := CharOffsets[CharEnd] - 1;

      for M := PathEnd + 1 to CharOffsets[CurrentI] - 1 do
        SetLength(TextPath.Path[M], 0);
    end;
  end;

  procedure AlignTextJustify(CurrentI: Integer);
  var
    L, M, N, PathStart, PathEnd: Integer;
    SpcDelta, SpcDeltaInc: TFloat;
  begin
    if (SpcCount < 1) or (Ord(Text[CurrentI]) = CHAR_CR) then
      Exit;
    SpcDelta := (ARect.Right * HorzStretch - X - 1) / SpcCount;
    SpcDeltaInc := SpcDelta;
    L := LineStart;

    // Trim leading spaces ...
    while (L < CurrentI) and (Ord(Text[L]) = CHAR_SP) do Inc(L);

    // Now find first space char in line ...
    while (L < CurrentI) and (Ord(Text[L]) <> CHAR_SP) do Inc(L);

    PathStart := CharOffsets[L - 1];
    repeat
      M := L + 1;
      while (M < CurrentI) and (Ord(Text[M]) <> CHAR_SP) do Inc(M);
      PathEnd := CharOffsets[M];
      L := M;
      for M := PathStart to PathEnd - 1 do
        for N := 0 to High(TextPath.Path[M]) do
          TextPath.Path[M, N].X := TextPath.Path[M, N].X + SpcDeltaInc;
      SpcDeltaInc := SpcDeltaInc + SpcDelta;
      PathStart := PathEnd;
    until L >= CurrentI;
  end;

  procedure AlignLine(CurrentI: Integer);
  begin
    if Assigned(TextPath) and (Length(TextPath.Path) > 0) then
      case (Flags and DT_HORZ_ALIGN_MASK) of
        DT_LEFT   : AlignTextLeft(CurrentI);
        DT_CENTER : AlignTextCenter(CurrentI);
        DT_RIGHT  : AlignTextRight(CurrentI);
        DT_JUSTIFY: AlignTextJustify(CurrentI);
      end;
  end;

  procedure AddSpace;
  begin
    Inc(SpcCount);
    X := X + SpcX;
  end;

  procedure NewLine(CurrentI: Integer);
  begin
    if (Flags and DT_SINGLELINE <> 0) then
    begin
      AddSpace;
      Exit;
    end;
    AlignLine(CurrentI);
    X := ARect.Left * HorzStretch;
    Y := Y + TextMetric.tmHeight;
    LineStart := CurrentI;
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

  function NeedsNewLine(X: Single): Boolean;
  begin
    Result := (ARect.Right > ARect.Left) and (X > ARect.Right * HorzStretch);
  end;

begin
{$IFDEF USEKERNING}
  KerningPairs := nil;
  KerningPairCount := GetKerningPairs(DC, 0, nil);
  if GetLastError <> 0 then
    RaiseLastOSError;
  if KerningPairCount > 0 then
  begin
    GetMem(KerningPairs, KerningPairCount * SizeOf(TKerningPair));
    GetKerningPairs(DC, KerningPairCount, PKerningPair(KerningPairs));
  end;
{$ENDIF}

  SpcCount := 0;
  LineStart := 0;
  OwnedPath := nil;
  if (Path <> nil) then
  begin
    if (Path is TFlattenedPath) then
    begin
      TextPath := TFlattenedPath(Path);
      TextPath.Clear;
    end
    else
    begin
      OwnedPath := TFlattenedPath.Create;
      TextPath := OwnedPath;
    end
  end else
    TextPath := nil;

  GetTextMetrics(DC, TextMetric);
  TextLen := Length(Text);
  X := ARect.Left * HorzStretch;
  Y := ARect.Top + TextMetric.tmAscent;
  XMax := X;

  if not Assigned(Path) or (ARect.Right = ARect.Left) then
    MaxRight := MaxSingle //either measuring Text or unbounded Text
  else
    MaxRight := ARect.Right * HorzStretch;
  SetLength(CharOffsets, TextLen + 1);
  CharOffsets[0] := 0;
  SetLength(CharWidths, TextLen);

  GetGlyphOutlineW(DC, CHAR_SP, GGODefaultFlags[UseHinting], GlyphMetrics,
    0, nil, VertFlip_mat2);
  SpcX := GlyphMetrics.gmCellIncX;

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

  // Batch whole path construction so we can be sure that the path isn't rendered
  // while we're still modifying it.
  if (TextPath <> nil) then
    TextPath.BeginUpdate;

  for I := 1 to TextLen do
  begin
    CharValue := Ord(Text[I]);
    if CharValue <= 32 then
    begin
      if (Flags and DT_SINGLELINE = DT_SINGLELINE) then
        CharValue := CHAR_SP;
      if Assigned(TextPath) then
        // Save path list offset of first path of current glyph
        CharOffsets[I] := Length(TextPath.Path);
      CharWidths[i - 1]:= SpcX;
      case CharValue of
        CHAR_CR: NewLine(I);
        CHAR_NL: ;
        CHAR_SP:
          begin
            if Flags and DT_WORDBREAK = DT_WORDBREAK then
            begin
              J := I + 1;
              while (J <= TextLen) and
                ([Ord(Text[J])] * [CHAR_CR, CHAR_NL, CHAR_SP] = []) do
                  Inc(J);
              S := Copy(Text, I, J - I);
              if NeedsNewLine(X + MeasureTextX(S)) then
                NewLine(I) else
                AddSpace;
            end else
            begin
              if NeedsNewLine(X + SpcX) then
                NewLine(I)
              else
                AddSpace;
            end;
          end;
      end;
    end
    else
    begin
      if GlyphOutlineToPath(DC, TextPath, X, MaxRight, Y, CharValue,
        GlyphMetrics) then
      begin
        if Assigned(TextPath) then
        // Save path list offset of first path of current glyph
          CharOffsets[I] := Length(TextPath.Path);
        CharWidths[I - 1]:= GlyphMetrics.gmCellIncX;
      end else
      begin
        if Ord(Text[I - 1]) = CHAR_SP then
        begin
          // this only happens without DT_WORDBREAK
          X := X - SpcX;
          Dec(SpcCount);
        end;
        // the current glyph doesn't fit so a word must be split since
        // it fills more than a whole line ...
        NewLine(I - 1);
        if not GlyphOutlineToPath(DC, TextPath, X, MaxRight, Y, CharValue,
          GlyphMetrics) then Break;
        if Assigned(TextPath) then
          // Save path list offset of first path of current glyph
          CharOffsets[I] := Length(TextPath.Path);
        CharWidths[I - 1]:= GlyphMetrics.gmCellIncX;
      end;

      X := X + GlyphMetrics.gmCellIncX;
      {$IFDEF USEKERNING}
      if i < TextLen then NextCharValue := Ord(Text[i + 1]);
      for J := 0 to KerningPairCount - 1 do 
      begin
        if (KerningPairs^[J].wFirst = CharValue) and
          (KerningPairs^[J].wSecond = NextCharValue) then
        begin
          X := X + KerningPairs^[J].iKernAmount;
          break;
        end;
      end;
      {$ENDIF}
      if X > XMax then XMax := X;
    end;
  end;
  if [(Flags and DT_HORZ_ALIGN_MASK)] * [DT_LEFT, DT_CENTER, DT_RIGHT] <> [] then
    AlignLine(TextLen);

  YMax := Y + TextMetric.tmHeight - TextMetric.tmAscent;
  // reverse HorzStretch (if any) ...
  if (HorzStretch <> 1) and assigned(TextPath) then
    for I := 0 to High(TextPath.Path) do
      for J := 0 to High(TextPath.Path[I]) do
        TextPath.Path[I, J].X := TextPath.Path[I, J].X * HorzStretch_Inv;
  XMax := XMax * HorzStretch_Inv;

  X := ARect.Right - XMax;
  Y := ARect.Bottom - YMax;
  if Flags and (DT_VCENTER or DT_BOTTOM) <> 0 then
  begin
    if Flags and DT_VCENTER <> 0 then
      Y := Y * 0.5;
    if Assigned(TextPath) then
      for I := 0 to High(TextPath.Path) do
        for J := 0 to High(TextPath.Path[I]) do
          TextPath.Path[I, J].Y := TextPath.Path[I, J].Y + Y;
  end;

{$IFDEF USEKERNING}
  if Assigned(KerningPairs) then
    FreeMem(KerningPairs);
{$ENDIF}

  if (Path <> nil) then
  begin
    TextPath.EndPath; // TODO : Why is this needed?

    if (Path <> TextPath) then
      Path.Assign(TextPath);

    TextPath.EndUpdate;

    OwnedPath.Free;
  end;
end;

procedure TextToPath(Font: HFONT; Path: TCustomPath; const ARect: TFloatRect;
  const Text: WideString; Flags: Cardinal = 0);
var
  DC: HDC;
  SavedFont: HFONT;
begin
  DC := GetDC(0);
  try
    SavedFont := SelectObject(DC, Font);
    InternalTextToPath(DC, Path, ARect, Text, Flags);
    SelectObject(DC, SavedFont);
  finally
    ReleaseDC(0, DC);
  end;
end;

function TextToPolyPolygon(Font: HFONT; const ARect: TFloatRect;
  const Text: WideString; Flags: Cardinal = 0): TArrayOfArrayOfFloatPoint;
var
  Path: TFlattenedPath;
begin
  Path := TFlattenedPath.Create;
  try
    TextToPath(Font, Path, ARect, Text, Flags);
    Result := Path.Path;
  finally
    Path.Free;
  end;
end;

function MeasureTextDC(DC: HDC; const ARect: TFloatRect; const Text: WideString;
  Flags: Cardinal): TFloatRect;
begin
  Result := ARect;
  InternalTextToPath(DC, nil, Result, Text, Flags);
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

procedure SetHinting(Value: TTextHinting);
begin
  UseHinting := Value <> thNone;
  if (Value = thNoHorz) then
    HorzStretch := 16 else
    HorzStretch := 1;
  HorzStretch_Inv := 1 / HorzStretch;
  FillChar(VertFlip_mat2, SizeOf(VertFlip_mat2), 0);
  VertFlip_mat2.eM11.value := HorzStretch;
  VertFlip_mat2.eM22.value := -1; //reversed Y axis
end;

function GetHinting: TTextHinting;
begin
  if HorzStretch <> 1 then Result := thNoHorz
  else if UseHinting then Result := thHinting
  else Result := thNone;
end;

procedure InitHinting;
begin
{$IFDEF NOHORIZONTALHINTING}
  SetHinting(thNoHorz);
{$ELSE}
{$IFDEF NOHINTING}
  SetHinting(thNone);
{$ELSE}
  SetHinting(thHinting);
{$ENDIF}
{$ENDIF}
end;

initialization
  InitHinting;

end.