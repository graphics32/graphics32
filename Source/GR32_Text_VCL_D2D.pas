unit GR32_Text_VCL_D2D;

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
 * Christian-W. Budde (Christian@pcjv.de)
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
  Windows, Types, Math, D2D1, GR32, GR32_Paths;

procedure TextToPath(Font: HFONT; const FontHeight: Integer; Path: TCustomPath;
  const ARect: TFloatRect; const Text: WideString; Flags: Cardinal = 0);
function TextToPolyPolygon(Font: HFONT; const FontHeight: Integer;
  const ARect: TFloatRect; const Text: WideString;
  Flags: Cardinal = 0): TArrayOfArrayOfFloatPoint;

function MeasureTextDC(DC: HDC; const FontHeight: Integer;
  const ARect: TFloatRect; const Text: WideString;
  Flags: Cardinal = 0): TFloatRect; overload;
function MeasureText(Font: HFONT; const FontHeight: Integer;
  const ARect: TFloatRect; const Text: WideString;
  Flags: Cardinal = 0): TFloatRect;

type
  TTextGeometrySink = class(TInterfacedObject, ID2D1SimplifiedGeometrySink, ID2D1GeometrySink)
  private
    FPath: TCustomPath;
    FDstX, FDstY: TFloat;
  public
    constructor Create(Path: TCustomPath; DstX, DstY: TFloat);

    procedure SetFillMode(fillMode: D2D1_FILL_MODE); stdcall;
    procedure SetSegmentFlags(vertexFlags: D2D1_PATH_SEGMENT); stdcall;
    procedure BeginFigure(startPoint: D2D1_POINT_2F;
      figureBegin: D2D1_FIGURE_BEGIN); stdcall;
    procedure AddLines(points: PD2D1Point2F; pointsCount: UINT); stdcall;
    procedure AddBeziers(beziers: PD2D1BezierSegment;
      beziersCount: UINT); stdcall;
    procedure EndFigure(figureEnd: D2D1_FIGURE_END); stdcall;
    function Close: HResult; stdcall;
    procedure AddLine(point: D2D1_POINT_2F); stdcall;
    procedure AddBezier(const bezier: D2D1_BEZIER_SEGMENT); stdcall;
    procedure AddQuadraticBezier(const bezier: D2D1_QUADRATIC_BEZIER_SEGMENT); stdcall;
    procedure AddQuadraticBeziers(beziers: PD2D1QuadraticBezierSegment;
      beziersCount: UINT); stdcall;
    procedure AddArc(const arc: D2D1_ARC_SEGMENT); stdcall;
  end;

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
  ComObj,
  SysUtils;

type
  IDWriteFontFaceFixed = interface(IUnknown)
    [SID_IDWriteFontFace]
    function GetType: DWRITE_FONT_FACE_TYPE; stdcall;

    function GetFiles(var numberOfFiles: Cardinal;
      out fontFiles: IDWriteFontFile): HResult; stdcall;

    function GetIndex: UINT32; stdcall;

    function GetSimulations: DWRITE_FONT_SIMULATIONS; stdcall;

    function IsSymbolFont: BOOL; stdcall;

    procedure GetMetrics(var fontFaceMetrics: TDwriteFontMetrics); stdcall;

    function GetGlyphCount: UINT16; stdcall;

    function GetDesignGlyphMetrics(glyphIndices: PWord; glyphCount: Cardinal;
      glyphMetrics: PDwriteGlyphMetrics; isSideways: BOOL = False): HResult; stdcall;

    function GetGlyphIndices(var codePoints: Cardinal; codePointCount: Cardinal;
      var glyphIndices: Word): HResult; stdcall;

    function TryGetFontTable(openTypeTableTag: Cardinal; var tableData: Pointer;
      var tableSize: Cardinal; var tableContext: Pointer;
      var exists: BOOL): HResult; stdcall;

    procedure ReleaseFontTable(tableContext: Pointer); stdcall;

    function GetGlyphRunOutline(emSize: Single; const glyphIndices: PWord;
      const glyphAdvances: PSingle; const glyphOffsets: PDwriteGlyphOffset;
      glyphCount: Cardinal; isSideways: BOOL; isRightToLeft: BOOL;
      geometrySink: IDWriteGeometrySink): HResult; stdcall;

    function GetRecommendedRenderingMode(emSize: Single; pixelsPerDip: Single;
      measuringMode: TDWriteMeasuringMode;
      var renderingParams: IDWriteRenderingParams;
      var renderingMode: TDWriteRenderingMode): HResult; stdcall;

    function GetGdiCompatibleMetrics(emSize: Single; pixelsPerDip: Single;
      transform: PDwriteMatrix; var fontFaceMetrics: DWRITE_FONT_METRICS): HResult; stdcall;

    function GetGDICompatibleGlyphMetrics(emSize: Single; pixelsPerDip: Single;
      transform: PDwriteMatrix; useGdiNatural: BOOL;
      glyphIndicies: PWord; glpyhCount: Cardinal;
      out glyphMetrics: TDwriteGlyphMetrics; isSideways: BOOL = FALSE): HResult; stdcall;
  end;


const
  MaxSingle   =  3.4e+38;

{ TTextGeometrySink }

constructor TTextGeometrySink.Create(Path: TCustomPath; DstX, DstY: TFloat);
begin
  FPath := Path;
  FDstX := DstX;
  FDstY := DstY;
end;

function D2D_POINT_2F_to_TFloatPoint(Value: D2D_POINT_2F): TFloatPoint;
begin
  Result.X := Value.x;
  Result.Y := Value.Y;
end;

procedure TTextGeometrySink.AddArc(const arc: D2D1_ARC_SEGMENT);
begin
//  FPath.Arc(D2D_POINT_2F_to_TFloatPoint(arc.point), arc.rotationAngle, arc.);
end;

procedure TTextGeometrySink.AddBezier(const bezier: D2D1_BEZIER_SEGMENT);
begin
  FPath.CurveTo(
    FDstX + bezier.point1.x, FDstY + bezier.point1.y,
    FDstX + bezier.point2.x, FDstY + bezier.point2.y,
    FDstX + bezier.point3.x, FDstY + bezier.point3.y);
end;

procedure TTextGeometrySink.AddBeziers(beziers: PD2D1BezierSegment;
  beziersCount: UINT);
var
  Index: Integer;
begin
  for Index := 0 to beziersCount - 1 do
  begin
    FPath.CurveTo(
      FDstX + beziers.point1.x, FDstY + beziers.point1.y,
      FDstX + beziers.point2.x, FDstY + beziers.point2.y,
      FDstX + beziers.point3.x, FDstY + beziers.point3.y);
    Inc(Beziers);
  end;
end;

procedure TTextGeometrySink.AddLine(point: D2D1_POINT_2F);
begin
  FPath.LineTo(FDstX + point.x, FDstY + point.y);
end;

procedure TTextGeometrySink.AddLines(points: PD2D1Point2F; pointsCount: UINT);
var
  Index: Integer;
begin
  for Index := 0 to pointsCount - 1 do
  begin
    FPath.LineTo(FDstX + points^.x, FDstY + points^.Y);
    Inc(points);
  end;
end;

procedure TTextGeometrySink.AddQuadraticBezier(
  const bezier: D2D1_QUADRATIC_BEZIER_SEGMENT);
begin
  FPath.CurveTo(
    FDstX + bezier.point1.x, FDstY + bezier.point1.y,
    FDstX + bezier.point2.x, FDstY + bezier.point2.y);
end;

procedure TTextGeometrySink.AddQuadraticBeziers(
  beziers: PD2D1QuadraticBezierSegment; beziersCount: UINT);
var
  Index: Integer;
begin
  for Index := 0 to beziersCount - 1 do
  begin
    FPath.CurveTo(
      FDstX + beziers^.point1.x, FDstY + beziers^.point1.y,
      FDstX + beziers^.point2.x, FDstY + beziers^.point2.y);
    Inc(Beziers);
  end;
end;

procedure TTextGeometrySink.BeginFigure(startPoint: D2D1_POINT_2F;
  figureBegin: D2D1_FIGURE_BEGIN);
begin
  FPath.MoveTo(FDstX + startPoint.x, FDstY + startPoint.Y);
end;

function TTextGeometrySink.Close: HResult;
begin
  Result := S_OK;
end;

procedure TTextGeometrySink.EndFigure(figureEnd: D2D1_FIGURE_END);
begin
  FPath.EndPath(True);
end;

procedure TTextGeometrySink.SetFillMode(fillMode: D2D1_FILL_MODE);
begin
end;

procedure TTextGeometrySink.SetSegmentFlags(vertexFlags: D2D1_PATH_SEGMENT);
begin

end;

var
  SingletonD2DFactory: ID2D1Factory;

function D2DFactory(FactoryType: TD2D1FactoryType = D2D1_FACTORY_TYPE_SINGLE_THREADED;
  FactoryOptions: PD2D1FactoryOptions = nil): ID2D1Factory;
var
  LD2DFactory: ID2D1Factory;
begin
  if SingletonD2DFactory = nil then
  begin
    D2D1CreateFactory(FactoryType, IID_ID2D1Factory, FactoryOptions, LD2DFactory);
    if InterlockedCompareExchangePointer(Pointer(SingletonD2DFactory), Pointer(LD2DFactory), nil) = nil then
      LD2DFactory._AddRef;
  end;
  Result := SingletonD2DFactory;
end;

var
  SingletonDWriteFactory: IDWriteFactory;

function DWriteFactory(FactoryType: TDWriteFactoryType = DWRITE_FACTORY_TYPE_SHARED): IDWriteFactory;
var
  LDWriteFactory: IDWriteFactory;
begin
  if SingletonDWriteFactory = nil then
  begin
    DWriteCreateFactory(FactoryType, IID_IDWriteFactory, IUnknown(LDWriteFactory));
    if InterlockedCompareExchangePointer(Pointer(SingletonDWriteFactory), Pointer(LDWriteFactory), nil) = nil then
      LDWriteFactory._AddRef;
  end;
  Result := SingletonDWriteFactory;
end;


procedure InternalTextToPath(DC: HDC; FontHeight: Integer;
  Path: TCustomPath; const ARect: TFloatRect;
  const Text: WideString; Flags: Cardinal = 0);
const
  CHAR_CR = 10;
  CHAR_NL = 13;
  CHAR_SP = 32;
var
  I, J, TextLen, SpcCount, SpcX, LineStart: Integer;
  CharValue: Integer;
  CharAdvance: TFloat;
  CharOffsets: TArrayOfInteger;
  CharWidths: TArrayOfInteger;
  X, Y, XMax, YMax, MaxRight: Single;
  S: WideString;
  // UseTempPath: Boolean;
  TextPath: TFlattenedPath;
  OwnedPath: TFlattenedPath;
  EmSize, PixelPerDip: Single;

  GDIInterop: IDWriteGdiInterop;
  Metrics: TDwriteFontMetrics;
  GlyphMetrics: TDwriteGlyphMetrics;
  GlyphIndex: Word;
  TextGeometrySink: TTextGeometrySink;
  FontFace: IDWriteFontFace;
  HR: HRESULT;
  CurrentChar: Word;

  procedure AlignTextCenter(CurrentI: Integer);
  var
    w, M, N, PathStart, PathEnd, CharStart, CharEnd: Integer;
    Delta: TFloat;
    i: Integer;
    MinX, MaxX: Single;
  begin
    Delta := Round(((ARect.Right - ARect.Left) - X - 1) * 0.5);
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
          MinX:= Round(Delta);
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
    Delta := Round(ARect.Right - X - 1);
    PathStart := CharOffsets[LineStart];
    PathEnd := CharOffsets[CurrentI] - 1;

    if (Flags and DT_SINGLELINE <> 0) and (Flags and DT_NOCLIP <> DT_NOCLIP) then
    begin
      CharStart := LineStart;

      w := 0;
      for i := LineStart to CurrentI - 1 do
      begin
        if w + Delta < Arect.Left then
          CharStart:= i + 1;
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
          CharEnd:= i + 1;
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
    SpcDelta := (ARect.Right - X - 1) / SpcCount;
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
    X := ARect.Left;
    Y := Y + (Metrics.ascent + Metrics.descent) / Metrics.designUnitsPerEm * EmSize * PixelPerDip; // was tmHeight
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
      IDWriteFontFaceFixed(FontFace).GetGDICompatibleGlyphMetrics(EmSize,
        PixelPerDip, nil, True, @CharValue, 1, GlyphMetrics);
      Inc(Result, Round(GlyphMetrics.advanceWidth / Metrics.designUnitsPerEm * EmSize * PixelPerDip));
    end;
  end;

  function NeedsNewLine(X: Single): Boolean;
  begin
    Result := (ARect.Right > ARect.Left) and (X > ARect.Right);
  end;

begin
  // get interoperability layer
  HR := DWriteFactory.GetGdiInterop(GDIInterop);
  OleCheck(HR);

  // get font face from GDI
  HR := GDIInterop.CreateFontFaceFromHdc(DC, FontFace);
  OleCheck(HR);

  FontFace.GetMetrics(Metrics);
  EmSize := FontHeight * 96 / 72;
  PixelPerDip := 1;

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

  TextLen := Length(Text);
  X := ARect.Left;
  Y := ARect.Top + Metrics.ascent / Metrics.designUnitsPerEm * EmSize * PixelPerDip;
  XMax := X;

  if not Assigned(Path) or (ARect.Right = ARect.Left) then
    MaxRight := MaxSingle //either measuring Text or unbounded Text
  else
    MaxRight := ARect.Right;
  SetLength(CharOffsets, TextLen + 1);
  CharOffsets[0] := 0;
  SetLength(CharWidths, TextLen);

  CurrentChar := CHAR_SP;
  IDWriteFontFaceFixed(FontFace).GetGDICompatibleGlyphMetrics(EmSize,
    PixelPerDip, nil, True, PWORD(@CurrentChar), 1, GlyphMetrics);
  SpcX := Round(GlyphMetrics.advanceWidth / Metrics.designUnitsPerEm * EmSize * PixelPerDip); // was gmCellIncX

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
      HR := FontFace.GetGlyphIndices(Cardinal(CharValue), 1, GlyphIndex);
      OleCheck(HR);

      HR := IDWriteFontFaceFixed(FontFace).GetGDICompatibleGlyphMetrics(EmSize,
        PixelPerDip, nil, True, @GlyphIndex, 1, GlyphMetrics);
      OleCheck(HR);

      CharAdvance := GlyphMetrics.advanceWidth / Metrics.designUnitsPerEm * EmSize * PixelPerDip;

      if X + CharAdvance <= MaxRight then
      begin
        TextGeometrySink := TTextGeometrySink.Create(TextPath, X, Y);
        try
          HR := FontFace.GetGlyphRunOutline(EmSize, @GlyphIndex, nil, nil, 1,
            False, False, TextGeometrySink);
          OleCheck(HR);
        finally
          TextGeometrySink.Free;
        end;

        if Assigned(TextPath) then
          CharOffsets[I] := Length(TextPath.Path);
        CharWidths[I - 1] := Round(CharAdvance);
      end
      else
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

        TextGeometrySink := TTextGeometrySink.Create(TextPath, X, Y);
        try
          HR := FontFace.GetGlyphRunOutline(EmSize, @GlyphIndex, nil, nil, 1,
            False, False, TextGeometrySink);
          OleCheck(HR);
        finally
          TextGeometrySink.Free;
        end;

        if Assigned(TextPath) then
          CharOffsets[I] := Length(TextPath.Path);
        CharWidths[I - 1] := Round(CharAdvance);
      end;

      X := X + CharAdvance;
      if X > XMax then XMax := X;
    end;
  end;
  if [(Flags and DT_HORZ_ALIGN_MASK)] * [DT_LEFT, DT_CENTER, DT_RIGHT] <> [] then
    AlignLine(TextLen);

  YMax := Y + Metrics.descent / Metrics.designUnitsPerEm * EmSize * PixelPerDip;

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

  if (Path <> nil) then
  begin
    TextPath.EndPath; // TODO : Why is this needed?

    if (Path <> TextPath) then
      Path.Assign(TextPath);

    TextPath.EndUpdate;

    OwnedPath.Free;
  end;
end;

procedure TextToPath(Font: HFONT; const FontHeight: Integer; Path: TCustomPath;
  const ARect: TFloatRect; const Text: WideString; Flags: Cardinal = 0);
var
  DC: HDC;
  SavedFont: HFONT;
begin
  DC := GetDC(0);
  try
    SavedFont := SelectObject(DC, Font);
    InternalTextToPath(DC, FontHeight, Path, ARect, Text, Flags);
    SelectObject(DC, SavedFont);
  finally
    ReleaseDC(0, DC);
  end;
end;

function TextToPolyPolygon(Font: HFONT; const FontHeight: Integer;
  const ARect: TFloatRect; const Text: WideString;
  Flags: Cardinal = 0): TArrayOfArrayOfFloatPoint;
var
  Path: TFlattenedPath;
begin
  Path := TFlattenedPath.Create;
  try
    TextToPath(Font, FontHeight, Path, ARect, Text, Flags);
    Result := Path.Path;
  finally
    Path.Free;
  end;
end;

function MeasureTextDC(DC: HDC; const FontHeight: Integer; const ARect: TFloatRect;
  const Text: WideString; Flags: Cardinal): TFloatRect;
begin
  Result := ARect;
  InternalTextToPath(DC, FontHeight, nil, Result, Text, Flags);
  Result.Left := Round(Result.Left);
  Result.Top := Round(Result.Top);
  Result.Right := Round(Result.Right);
  Result.Bottom := Round(Result.Bottom);
end;

function MeasureText(Font: HFONT; const FontHeight: Integer;
  const ARect: TFloatRect; const Text: WideString; Flags: Cardinal): TFloatRect;
var
  DC: HDC;
  SavedFont: HFONT;
begin
  DC := GetDC(0);
  try
    SavedFont := SelectObject(DC, Font);
    Result := MeasureTextDC(DC, FontHeight, ARect, Text, Flags);
    SelectObject(DC, SavedFont);
  finally
    ReleaseDC(0, DC);
  end;
end;

end.
