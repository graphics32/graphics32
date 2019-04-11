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

type
  TTextHinting = (thNone, thNoHorz, thHinting);

procedure SetHinting(Value: TTextHinting);
function GetHinting: TTextHinting;

const
  DT_LEFT       = 0;   //See also Window's DrawText() flags ...
  DT_CENTER     = 1;   //http://msdn.microsoft.com/en-us/library/ms901121.aspx
  DT_RIGHT      = 2;
  DT_WORDBREAK  = $10;
  DT_VCENTER    = 4;
  DT_BOTTOM     = 8;
  DT_SINGLELINE = $20;
  DT_JUSTIFY         = 3;  //Graphics32 additions ...
  DT_HORZ_ALIGN_MASK = 3;

implementation

uses
  GR32_LowLevel;

var
  UseHinting: Boolean;
  HorzStretch: Integer; // stretching factor when calling GetGlyphOutline()
  HorzStretch_Inv: single;

  VertFlip_mat2: tmat2;

const
  GGO_UNHINTED = $0100;
  GGODefaultFlags: array [Boolean] of Integer = (GGO_NATIVE or GGO_UNHINTED, GGO_NATIVE);

  TT_PRIM_CSPLINE = 3;

  MaxSingle   =  3.4e+38;

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

    Path.EndPath(True);

    Dec(Res, BufferPtr.cb);
    Inc(PtrInt(BufferPtr), BufferPtr.cb);
  end;

  FreeMem(GlyphMemPtr);
end;

procedure InternalTextToPath(DC: HDC; Path: TCustomPath; const ARect: TFloatRect;
  const Text: WideString; Flags: Cardinal);
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
  X, Y, XMax, YMax, MaxRight: Single;
  S: WideString;
  TextPath: TFlattenedPath;
  OwnedPath: TFlattenedPath;
{$IFDEF USEKERNING}
  LastCharValue: Integer;
  KerningPairs: PKerningPairArray;
  KerningPairCount: Integer;
{$ENDIF}

  procedure AlignTextCenter(CurrentI: Integer);
  var
    M, N, PathStart, PathEnd: Integer;
    Delta: TFloat;
  begin
    Delta := Round((ARect.Right * HorzStretch - X - 1) * 0.5);
    PathStart := CharOffsets[LineStart];
    PathEnd := CharOffsets[CurrentI];
    for M := PathStart to PathEnd - 1 do
      for N := 0 to High(TextPath.Path[M]) do
        TextPath.Path[M][N].X := TextPath.Path[M][N].X + Delta;
  end;

  procedure AlignTextRight(CurrentI: Integer);
  var
    M, N, PathStart, PathEnd: Integer;
    Delta: TFloat;
  begin
    Delta := Round(ARect.Right * HorzStretch - X - 1);
    PathStart := CharOffsets[LineStart];
    PathEnd := CharOffsets[CurrentI];
    for M := PathStart to PathEnd - 1 do
      for N := 0 to High(TextPath.Path[M]) do
        TextPath.Path[M][N].X := TextPath.Path[M][N].X + Delta;
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
    //Trim leading spaces ...
    while (L < CurrentI) and (Ord(Text[L]) = CHAR_SP) do Inc(L);
    //Now find first space char in line ...
    while (L < CurrentI) and (Ord(Text[L]) <> CHAR_SP) do Inc(L);
    PathStart := CharOffsets[L - 1];
    repeat
      M := L + 1;
      while (M < CurrentI) and (Ord(Text[M]) <> CHAR_SP) do Inc(M);
      PathEnd := CharOffsets[M];
      L := M;
      for M := PathStart to PathEnd - 1 do
        for N := 0 to High(TextPath.Path[M]) do
          TextPath.Path[M][N].X := TextPath.Path[M][N].X + SpcDeltaInc;
      SpcDeltaInc := SpcDeltaInc + SpcDelta;
      PathStart := PathEnd;
    until L >= CurrentI;
  end;

  procedure NewLine(CurrentI: Integer);
  begin
    if (Flags and DT_SINGLELINE <> 0) then Exit;
    if assigned(TextPath) then
      case (Flags and DT_HORZ_ALIGN_MASK) of
        DT_CENTER : AlignTextCenter(CurrentI);
        DT_RIGHT  : AlignTextRight(CurrentI);
        DT_JUSTIFY: AlignTextJustify(CurrentI);
      end;
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

  function NeedsNewLine(X: Single): boolean;
  begin
    Result := X > ARect.Right * HorzStretch;
  end;

  procedure AddSpace;
  begin
    Inc(SpcCount);
    X := X + SpcX;
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
  LastCharValue := 0;
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
    MaxRight := MaxSingle //either measuring text or unbounded text
  else
    MaxRight := ARect.Right * HorzStretch;
  SetLength(CharOffsets, TextLen +1);
  CharOffsets[0] := 0;

  GetGlyphOutlineW(DC, CHAR_SP, GGODefaultFlags[UseHinting],
    GlyphMetrics, 0, nil, VertFlip_mat2);
  SpcX := GlyphMetrics.gmCellIncX;

  if (Flags and DT_SINGLELINE <> 0) then
  begin
    //ignore justify when forcing singleline ...
    if (Flags and DT_JUSTIFY = DT_JUSTIFY) then
      Flags := Flags and not DT_JUSTIFY;
    //ignore wordbreak when forcing singleline ...
    if (Flags and DT_WORDBREAK = DT_WORDBREAK) then
      Flags := Flags and not DT_WORDBREAK;
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
        CharOffsets[I] := Length(TextPath.Path);

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
      if GlyphOutlineToPath(DC, TextPath,
        X, MaxRight, Y, CharValue, GlyphMetrics) then
      begin
        if Assigned(TextPath) then
          CharOffsets[I] := Length(TextPath.Path);
      end else
      begin
        if Ord(Text[I -1]) = CHAR_SP then
        begin
          //this only happens without DT_WORDBREAK
          X := X - SpcX;
          Dec(SpcCount);
        end;
        //the current glyph doesn't fit so a word must be split since
        //it fills more than a whole line ...
        NewLine(I - 1);
        if not GlyphOutlineToPath(DC, TextPath,
            X, MaxRight, Y, CharValue, GlyphMetrics) then Break;
        if Assigned(TextPath) then
          CharOffsets[I] := Length(TextPath.Path);
      end;
      X := X + GlyphMetrics.gmCellIncX;
      {$IFDEF USEKERNING}
      for J := 0 to KerningPairCount - 1 do
      begin
        if (KerningPairs^[J].wFirst = LastCharValue) and
          (KerningPairs^[J].wSecond = CharValue) then
          X := X + KerningPairs^[J].iKernAmount;
      end;
      LastCharValue := CharValue;
      {$ENDIF}
      if X > XMax then XMax := X;
    end;
  end;
  if [(Flags and DT_HORZ_ALIGN_MASK)] * [DT_CENTER, DT_RIGHT] <> [] then
    NewLine(TextLen);

  YMax := Y + TextMetric.tmHeight - TextMetric.tmAscent;
  //reverse HorzStretch (if any) ...
  if (HorzStretch <> 1) and assigned(TextPath) then
    for I := 0 to High(TextPath.Path) do
      for J := 0 to High(TextPath.Path[I]) do
        TextPath.Path[I][J].X := TextPath.Path[I][J].X * HorzStretch_Inv;
  XMax := XMax * HorzStretch_Inv;

  X := ARect.Right - XMax;
  Y := ARect.Bottom - YMax;
  if Flags and (DT_VCENTER or DT_BOTTOM) <> 0 then
  begin
    if Flags and DT_VCENTER <> 0 then
      Y := Y * 0.5;
    if assigned(TextPath) then
      for I := 0 to High(TextPath.Path) do
        for J := 0 to High(TextPath.Path[I]) do
          TextPath.Path[I][J].Y := TextPath.Path[I][J].Y + Y;
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
  const Text: WideString; Flags: Cardinal); overload;
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
  {$ENDIF};
{$ENDIF}
end;

initialization
  InitHinting;

end.
