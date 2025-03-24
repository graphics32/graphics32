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

//------------------------------------------------------------------------------
//
//      Font provider for Windows platform
//
//------------------------------------------------------------------------------
//
//      This unit should be considered internal to Graphics32.
//
//      Use the corresponding functions in the backend instead.
//
//------------------------------------------------------------------------------

uses
  Generics.Defaults,
  Generics.Collections,
  Windows,
  Types,
  Graphics,

  GR32,
  GR32_Paths,
  GR32.Text.Types,
  GR32.Text.Layout,
  GR32.Text.FontFace,
  GR32.Text.Cache;


//------------------------------------------------------------------------------
//
//      TFontFace32
//
//------------------------------------------------------------------------------
//
// Implements IFontFace32 for Windows.
//
// Besides the basic IFontFace32 functionality, which is to get font data from
// Windows, the class employs a metadata cache. This avoids repeated calls to
// GDI to get font and glyph metrics resulting in a factor 10 speedup.
// The metadata cache is mandatory and data is never evicted from it but since
// cache entries are relatively small, and each entry represents a font, it
// shouldn't be necessary to limit the size.
// If need be the cache can be manually cleared with the TFontFace32.ClearCache
// method.
//
// In addition, the generic glyph cache is also used.
// This reduces the impact of the second-most costly operation: Retrieving glyph
// outlines and converting them to polygons. The result is an additional factor
// 10 speedup.
// The glyph cache is optional but enabled by default.
// The glyph cache can be enabled and disabled with EnableGlyphCache and
// DisablGlyphCache.
//
//------------------------------------------------------------------------------
type
  TFontData = record
    FontDC: HDC;
    Font: HFONT;

    LogFont: TLogFont;
    OutlineTextMetric: TOutlineTextMetric;
    EMSize: Cardinal;
  end;

  // Metadata cache item
  IFontItem = interface
    // The desired font size is specified by AFontData.LogFont.lfHeight.
    // Returns metrics in AFontData. These metrics must then be scaled
    // by the caller using the value returned in AScale.
    procedure GetFontData(var AFontData: TFontData; var AScale: Double);
  end;

type
  TFontFace32 = class(TInterfacedObject, IFontFace32)
  private class var
    // Font metadata cache
    FFontCache: TDictionary<TFontKey, IFontItem>;

  private
    class constructor Create;
    class destructor Destroy;

  private
    // Values copied from FFontItem
    FFontData: TFontData;
    FScale: Double;
    FScaleInv: Double;

  private
    // Font metadata cache
    FFontItem: IFontItem;
    FFontKey: TFontKey;

    function GetFontKey: TFontKey;

  private
    // Obsolete stuff
    FSessionCount: integer;

  private
    // Outline cache
    FGlyphCacheItem: IGlyphCacheFontItem;

  private type
    TKerningPairs = TArray<TKerningPair>;
  private
    // Kerning
    // TODO : Move to TFontItem
    FHasKerning: boolean;
    FKerningPairs: TKerningPairs;
{$ifndef FPC}
    FKerningComparer: IComparer<TKerningPair>;
{$endif}

  private
    procedure AssignGlyphMetrics(const GlyphMetrics: TGlyphMetrics; var AGlyphMetrics: TGlyphMetrics32);
    procedure CopyGlyphMetrics(const ASource: TGlyphMetrics32; out ADest: TGlyphMetrics32; AScale: Single);

  private
    // IFontFace32
    procedure BeginSession;
    procedure EndSession;

    function GetFontFaceMetrics(const ATextLayout: TTextLayout; var AFontFaceMetrics: TFontFaceMetrics32): boolean;
    function GetGlyphMetrics(AGlyph: Cardinal; var AGlyphMetrics: TGlyphMetrics32): boolean;
    function GetGlyphOutline(AGlyph: Cardinal; var AGlyphMetrics: TGlyphMetrics32; APath: TCustomPath; AOffsetX: Single = 0; AOffsetY: Single = 0): boolean;
    function GetKerning(AFirstGlyph, ASecondGlyph: Cardinal): Single;

  public
    constructor Create(AFont: TFont);

    class procedure ClearCache;
  end;


//------------------------------------------------------------------------------
//
//      TFontFaceProvider
//
//------------------------------------------------------------------------------
// Implements IFontFaceProvider32 for Windows
//------------------------------------------------------------------------------
type
  TFontFaceProvider = class(TInterfacedObject, IFontFaceProvider32)
  private
    // IFontFaceProvider32
    function CreateFontFace(AFont: TFont): IFontFace32;
  end;


//------------------------------------------------------------------------------
//
//      Text functions for Windows
//
//------------------------------------------------------------------------------
type
  TextToolsWin = record
    class procedure TextToPath(AFont: TFont; APath: TCustomPath; const ARect: TFloatRect; const AText: string; AFlags: Cardinal = 0); overload; static;
    class procedure TextToPath(AFont: TFont; APath: TCustomPath; const ARect: TFloatRect; const AText: string; const ALayout: TTextLayout); overload; static;

    class function TextToPolyPolygon(AFont: TFont; const ARect: TFloatRect; const AText: string; AFlags: Cardinal = 0): TArrayOfArrayOfFloatPoint; static; deprecated;

    class function MeasureText(AFont: TFont; const ARect: TFloatRect; const AText: string; AFlags: Cardinal = 0): TFloatRect; overload; static;
    class function MeasureText(AFont: TFont; const ARect: TFloatRect; const AText: string; const ALayout: TTextLayout): TFloatRect; overload; static;
  end;


//------------------------------------------------------------------------------
//
//      MaxEMSquare
//
//------------------------------------------------------------------------------
// Specifies the max resolution of the glyph data retrieved from Windows and
// later flattened to polypolygons.
//
// - A small value will make the rendering faster but will also distort the
//   glyphs.
//
// - A large value will make the rendering slower but will also produce higher
//   quality glyphs.
//
// The full font resolution (i.e. the resolution the font was designed at) is
// usually 2048.
// It is recommended that the max resolution not be set lower than half the
// maximum font size that will be rendered, although even then the degredation
// in quality can be hard to spot visually.
//------------------------------------------------------------------------------

var
  MaxEMSquare: Cardinal = 128;

//------------------------------------------------------------------------------

{$ifdef IGNORE_HINTING_DEPRECATED}
const
{$else}
var
{$endif}
  UseHinting: Boolean = False {$ifndef IGNORE_HINTING_DEPRECATED}deprecated 'Hinting is no longer supported. See IGNORE_HINTING_DEPRECATED in GR32.inc'{$endif};



//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  Math,
  SysUtils,
  GR32.Text.Unicode;

const
  VertFlip_mat2: TMat2 = (
    eM11: (Fract: 0; Value: 1);
    eM12: (Fract: 0; Value: 0);
    eM21: (Fract: 0; Value: 0);
    eM22: (Fract: 0; Value: -1); // Reversed Y axis
  );

const
  GGO_UNHINTED = $0100;

const
  TT_PRIM_CSPLINE = 3;

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

procedure SanitizeLogFont(var LogFont: TLogFont); {$IFDEF UseInlining} inline; {$ENDIF}
var
  i: integer;
  Clear: boolean;
begin
  // Clear junk from LOGFONT so we can produce a consistent hash from it
  Clear := False;
  for i := 0 to High(LogFont.lfFaceName) do
  begin
    if (Clear) then
      LogFont.lfFaceName[i] := #0
    else
      Clear := (LogFont.lfFaceName[i] = #0);
  end;
end;


//------------------------------------------------------------------------------
//
//      TextToolsWin
//
//------------------------------------------------------------------------------
class procedure TextToolsWin.TextToPath(AFont: TFont; APath: TCustomPath; const ARect: TFloatRect; const AText: string; const ALayout: TTextLayout);
var
  R: TFloatRect;
  FontFace: IFontFace32;
begin
  FontFace := TFontFace32.Create(AFont);

  FontFace.BeginSession;

  R := ARect;

  LayoutEngine.TextToPath(FontFace, APath, R, AText, ALayout);

  FontFace.EndSession;
end;

class procedure TextToolsWin.TextToPath(AFont: TFont; APath: TCustomPath; const ARect: TFloatRect; const AText: string; AFlags: Cardinal);
var
  TextLayout: TTextLayout;
begin
  TextLayout := DefaultTextLayout;
  TextFlagsToLayout(AFlags, TextLayout);

  TextToPath(AFont, APath, ARect, AText, TextLayout);
end;

//------------------------------------------------------------------------------

class function TextToolsWin.MeasureText(AFont: TFont; const ARect: TFloatRect; const AText: string; const ALayout: TTextLayout): TFloatRect;
var
  FontFace: IFontFace32;
begin
  Result := ARect;

  FontFace := TFontFace32.Create(AFont);

  LayoutEngine.TextToPath(FontFace, nil, Result, AText, ALayout);
end;

class function TextToolsWin.MeasureText(AFont: TFont; const ARect: TFloatRect; const AText: string; AFlags: Cardinal): TFloatRect;
var
  TextLayout: TTextLayout;
begin
  TextLayout := DefaultTextLayout;
  TextFlagsToLayout(AFlags, TextLayout);

  Result := MeasureText(AFont, ARect, AText, TextLayout);
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
//
//      TFontItem
//
//------------------------------------------------------------------------------
type
  TFontItem = class(TInterfacedObject, IFontItem)
  strict private
    FFontData: TFontData;

  private
    // IFontItem
    procedure GetFontData(var AFontData: TFontData; var AScale: Double);

  public
    constructor Create(AFont: HFONT; var AFontData: TFontData; var AScale: Double);
    destructor Destroy; override;
  end;

//------------------------------------------------------------------------------

constructor TFontItem.Create(AFont: HFONT; var AFontData: TFontData; var AScale: Double);
var
  Size: integer;
begin
  inherited Create;

  // It is assumed that AFontData.LogFont contains a valid LOGFONT that
  // describes the desired metadata; It should be possible to create
  // a font based on the values.
  // The desired font size is specified in AFontData.LogFont.lfHeight.
  // On exit the cached metrics is returned in AFontData. These metrics must
  // then be scaled by the caller using the value returned in AScale.

  FFontData.FontDC := CreateCompatibleDC(0);
  if (FFontData.FontDC = 0) then
    RaiseLastOSError;

  (*
  ** In order to get the most precise outline FFontData from the font, we use its
  ** design size and then scale all values back to the desired size.
  *)

  (*
  ** Fetch metrics for the source font so we can get the font design size (EMSquare).
  *)
  SelectObject(FFontData.FontDC, AFont);
  if (GetOutlineTextMetrics(FFontData.FontDC, SizeOf(FFontData.OutlineTextMetric), @FFontData.OutlineTextMetric) = 0) then
    RaiseLastOSError;

  // Limit EMSquare to something reasonable; We will everntually flatten the glyph
  // beziers at this resolution and if we do it at the full resolution (usually
  // 2048*2048 usually), then we will end up with far too many vertices that we
  // don't really need at the typical output resolution (~ 12*12-20x20)
  FFontData.EMSize := Min(MaxEMSquare, FFontData.OutlineTextMetric.otmEMSquare);

  // Return the scale that must be applied to values returned from the internal font.
  // We cannot store this value here since all metrics here are relative to EMSquare
  // regardless of the font size requested.
  AScale := Abs(AFontData.LogFont.lfHeight / FFontData.EMSize);


  (*
  ** Create a new internal font at the design size
  *)
  // Change the font size but keep everything else
  AFontData.LogFont.lfHeight := -FFontData.EMSize;
  AFontData.LogFont.lfWidth := 0; // Use default width for the height

  // Create the new font, in the new size
  FFontData.Font := CreateFontIndirect(AFontData.LogFont);
  if (FFontData.Font = 0) then
    RaiseLastOSError;

  (*
  ** Fetch LOGFONT for the new internal font
  *)
  Size := GetObject(FFontData.Font, SizeOf(TLogFont), @FFontData.LogFont);
  if (Size <> SizeOf(TLogFont)) then
    RaiseLastOSError;

  // Clear junk so we can produce a consistent hash
  SanitizeLogFont(FFontData.LogFont);

  (*
  ** Fetch OTM for the new internal font
  *)
  SelectObject(FFontData.FontDC, FFontData.Font);
  if (GetOutlineTextMetrics(FFontData.FontDC, SizeOf(FFontData.OutlineTextMetric), @FFontData.OutlineTextMetric) = 0) then
    RaiseLastOSError;

  // Return font data
  AFontData := FFontData;
end;

//------------------------------------------------------------------------------

destructor TFontItem.Destroy;
begin
  DeleteDC(FFontData.FontDC);
  DeleteObject(FFontData.Font);

  inherited;
end;

//------------------------------------------------------------------------------

procedure TFontItem.GetFontData(var AFontData: TFontData; var AScale: Double);
begin
  AScale := Abs(AFontData.LogFont.lfHeight / FFontData.EMSize);
  AFontData := FFontData;
end;


//------------------------------------------------------------------------------
//
//      TFontFace32
//
//------------------------------------------------------------------------------

constructor TFontFace32.Create(AFont: TFont);
var
  Size: integer;
begin

  // Temporarily reuse FFontData.LogFont. We will overwrite the value below
  Size := GetObject(AFont.Handle, SizeOf(TLogFont), @FFontData.LogFont);
  if (Size <> SizeOf(TLogFont)) then
    RaiseLastOSError;


  // Clear junk from LOGFONT so we can produce a consistent hash from it
  SanitizeLogFont(FFontData.LogFont);

  // Get data from metadata cache or add
  FFontKey := GetFontKey;

  if (not FFontCache.TryGetValue(FFontKey, FFontItem)) then
  begin
    FFontItem := TFontItem.Create(AFont.Handle, FFontData, FScale);
    FFontCache.Add(FFontKey, FFontItem);
  end else
    FFontItem.GetFontData(FFontData, FScale);

  if (FScale <> 0) then
    FScaleInv := 1 / FScale
  else
    FScaleInv := 0;


  // Register font in glyph cache
  if (GlyphCache <> nil) then
  begin

    FGlyphCacheItem := GlyphCache.FindItem(FFontKey);

    if (FGlyphCacheItem = nil) then
      FGlyphCacheItem := GlyphCache.AddItem(FFontKey);

  end;
end;

//------------------------------------------------------------------------------

class constructor TFontFace32.Create;
begin
  FFontCache := TDictionary<TFontKey, IFontItem>.Create;
end;

class destructor TFontFace32.Destroy;
begin
  FFontCache.Free;
end;

//------------------------------------------------------------------------------

class procedure TFontFace32.ClearCache;
begin
  FFontCache.Clear;
end;

//------------------------------------------------------------------------------

function TFontFace32.GetFontKey: TFontKey;
var
  Offset: NativeUInt;
begin
  // Both the metadata and glyph caches are size agnostic so we do not
  // include the font size fields in the key.
  Offset := NativeUInt(@TLogFont(nil^).lfEscapement); // OffsetOf(TLogFont, lfEscapement)
  SetLength(Result, SizeOf(TLogFont) - Offset);

  CopyMemory(@Result[0], PByte(@FFontData.LogFont) + Offset, Length(Result));
end;

//------------------------------------------------------------------------------

procedure TFontFace32.AssignGlyphMetrics(const GlyphMetrics: TGlyphMetrics; var AGlyphMetrics: TGlyphMetrics32);
begin
  AGlyphMetrics.Valid := True;

  AGlyphMetrics.OffsetX := GlyphMetrics.gmptGlyphOrigin.X * FScale;
  AGlyphMetrics.OffsetY := GlyphMetrics.gmptGlyphOrigin.Y * FScale;
  AGlyphMetrics.Width := GlyphMetrics.gmBlackBoxX * FScale;
  AGlyphMetrics.Height := GlyphMetrics.gmBlackBoxY * FScale;
  AGlyphMetrics.AdvanceX := GlyphMetrics.gmCellIncX * FScale;
  AGlyphMetrics.AdvanceY := GlyphMetrics.gmCellIncY * FScale;
end;

procedure TFontFace32.CopyGlyphMetrics(const ASource: TGlyphMetrics32; out ADest: TGlyphMetrics32; AScale: Single);
begin
  ADest.Valid := ASource.Valid;

  ADest.OffsetX := ASource.OffsetX * AScale;
  ADest.OffsetY := ASource.OffsetY * AScale;
  ADest.Width := ASource.Width * AScale;
  ADest.Height := ASource.Height * AScale;
  ADest.AdvanceX := ASource.AdvanceX * AScale;
  ADest.AdvanceY := ASource.AdvanceY * AScale;
end;
//------------------------------------------------------------------------------

procedure TFontFace32.BeginSession;
begin
  Inc(FSessionCount);
end;

procedure TFontFace32.EndSession;
begin
  Dec(FSessionCount);
end;

//------------------------------------------------------------------------------

function TFontFace32.GetGlyphMetrics(AGlyph: Cardinal; var AGlyphMetrics: TGlyphMetrics32): boolean;

  function ResolveFromGlyphCache: boolean;
  var
    GlyphCacheData: IGlyphCacheData;
  begin
    Result := False;
    if (FGlyphCacheItem = nil) then
      exit;

    GlyphCacheData := FGlyphCacheItem.FindGlyphData(AGlyph);

    if (GlyphCacheData <> nil) then
    begin
      CopyGlyphMetrics(GlyphCacheData.GlyphMetrics, AGlyphMetrics, FScale);
      Result := True;
    end;
  end;

  procedure UpdateGlyphCache;
  var
    CacheGlyphMetrics: TGlyphMetrics32;
  begin
    if (FGlyphCacheItem = nil) then
      exit;

    CopyGlyphMetrics(AGlyphMetrics, CacheGlyphMetrics, FScaleInv);

    FGlyphCacheItem.AddGlyphData(AGlyph, CacheGlyphMetrics);
  end;

var
  GlyphMetrics: TGlyphMetrics;
  Res: DWORD;
begin
  Result := False;

  (*
  ** Try glyph cache first
  *)
  if (ResolveFromGlyphCache) then
    Exit(True);


  BeginSession;
  try

    GlyphMetrics := Default(TGlyphMetrics);
    Res := Windows.GetGlyphOutline(FFontData.FontDC, AGlyph, GGO_METRICS, GlyphMetrics, 0, nil, VertFlip_mat2);

    if (Res <> GDI_ERROR) then
    begin
      AssignGlyphMetrics(GlyphMetrics, AGlyphMetrics);
      Result := True;
    end else
      AGlyphMetrics := Default(TGlyphMetrics32);

  finally
    EndSession;
  end;


  (*
  ** Add to glyph cache
  *)
  UpdateGlyphCache;
end;

//------------------------------------------------------------------------------

function TFontFace32.GetGlyphOutline(AGlyph: Cardinal; var AGlyphMetrics: TGlyphMetrics32; APath: TCustomPath; AOffsetX, AOffsetY: Single): boolean;
var
  GlyphCacheData: IGlyphCacheData;
  FirstGlyphIndex: integer;

  function ResolveFromGlyphCache: boolean;
  var
    i, j: integer;
    pp: PFloatPoint;
    P: TFloatPoint;
  begin
    Result := False;

    if (FGlyphCacheItem = nil) then
      exit;

    GlyphCacheData := FGlyphCacheItem.FindGlyphData(AGlyph);

    if (GlyphCacheData = nil) or (not GlyphCacheData.PathValid) then
      exit;

    APath.BeginUpdate;

    // Replay cache data
    for i := 0 to High(GlyphCacheData.Path) do
      if (Length(GlyphCacheData.Path[i]) > 0) then
      begin

        // We use a pointer to avoid dynamic array overhead
        pp := @GlyphCacheData.Path[i, 0];
        for j := 0 to High(GlyphCacheData.Path[i]) do
        begin
          P := pp^ * FScale;
          P.X := P.X + AOffsetX;
          P.Y := p.Y + AOffsetY;

          if (j = 0) then
            APath.MoveTo(P)
          else
            APath.LineTo(P);

          Inc(pp);
        end;

        APath.EndPath(GlyphCacheData.PathClosed[i]);
      end;

    APath.EndUpdate;

    Result := True;
  end;

  procedure UpdateGlyphCache;
  var
    CacheGlyphMetrics: TGlyphMetrics32;
  begin
    if (FGlyphCacheItem = nil) then
      exit;

    // If we got to here we know that:
    //   1) The glyph wasn't in the cache
    // or
    //   2) The glyph was in the cache but without path data

    if (GlyphCacheData = nil) then
    begin
      // 1) Not in cache; Create a new entry
      CopyGlyphMetrics(AGlyphMetrics, CacheGlyphMetrics, FScaleInv);
      GlyphCacheData := FGlyphCacheItem.AddGlyphData(AGlyph, CacheGlyphMetrics);
    end;

    // 2) Copy path data to cache
//    GlyphCacheData.LoadFromPath(TFlattenedPath(APath), FirstGlyphIndex, AOffsetX, AOffsetY, FScaleInv);
    GlyphCacheData.LoadFromPath(TFlattenedPath(APath), FirstGlyphIndex, 0, 0, 1.0);
  end;

  procedure TransformPoint(var APoint: TFloatPoint);
  begin
    APoint.X := APoint.X * FScale + AOffsetX;
    APoint.Y := APoint.Y * FScale + AOffsetY;
  end;

  procedure TransformGlyph;
  var
    i, j: integer;
  begin
    for i := FirstGlyphIndex to High(TFlattenedPath(APath).Path) do
      for j := 0 to High(TFlattenedPath(APath).Path[i]) do
        TransformPoint(TFlattenedPath(APath).Path[i, j]);
  end;


var
  GlyphMetrics: TGlyphMetrics;
  PolygonHeaderAlloc: PTTPolygonHeader;
  PolygonHeader: PTTPolygonHeader;
  PolygonHeaderSize: DWORD;
  CurvePtr: PTTPolyCurve;
  P1, P2, P3, PNext: TFloatPoint;
  i, k: Integer;
  Size: Integer;
const
  OneHalf: Single = 0.5; // Typed constant to avoid Double/Extended
begin
  BeginSession;
  try

    if (APath = nil) then
    begin
      Result := GetGlyphMetrics(AGlyph, AGlyphMetrics);
      exit;
    end;


    (*
    ** Try to resolve from glyph cache
    *)
    if (FGlyphCacheItem <> nil) and (ResolveFromGlyphCache) then
      Exit(True);


    (*
    ** Get outline data from Windows
    *)
    GlyphMetrics := Default(TGlyphMetrics);
    PolygonHeaderSize := Windows.GetGlyphOutline(FFontData.FontDC, AGlyph, GGO_NATIVE or GGO_UNHINTED, GlyphMetrics, 0, nil, VertFlip_mat2);

    if (PolygonHeaderSize <> GDI_ERROR) then
      AssignGlyphMetrics(GlyphMetrics, AGlyphMetrics)
    else
      AGlyphMetrics := Default(TGlyphMetrics32);

    Result := (PolygonHeaderSize <> GDI_ERROR) and (PolygonHeaderSize <> 0);

    if (not Result) then
      exit;

    GetMem(PolygonHeaderAlloc, PolygonHeaderSize);
    try
      PolygonHeader := PolygonHeaderAlloc;
      PolygonHeaderSize := Windows.GetGlyphOutline(FFontData.FontDC, AGlyph, GGO_NATIVE or GGO_UNHINTED, GlyphMetrics, PolygonHeaderSize, PolygonHeader, VertFlip_mat2);

      if (PolygonHeaderSize = GDI_ERROR) or (PolygonHeader.dwType <> TT_POLYGON_TYPE) then
        exit;

      // Batch each glyph to ensure that the polypolygons of the glyph are rendered
      // as one and not as individual independent polygons.
      // We're doing this here for completeness but since the path will also be
      // batched at an outer level it isn't really necessary here.
      APath.BeginUpdate;

      FirstGlyphIndex := High(TFlattenedPath(APath).Path) + 1;

      while (PolygonHeaderSize > 0) do
      begin
        Size := PolygonHeader.cb - SizeOf(TTTPolygonHeader);
        PByte(CurvePtr) := PByte(PolygonHeader) + SizeOf(TTTPolygonHeader);

        // First point is part of header
        P1 := PointFXtoPointF(PolygonHeader.pfxStart);
        if (FGlyphCacheItem = nil) then
          TransformPoint(P1);
        APath.MoveTo(P1);

        while (Size > 0) do
        begin
          case CurvePtr.wType of
            TT_PRIM_LINE:
              for i := 0 to CurvePtr.cpfx-1 do
              begin
                P1 := PointFXtoPointF(CurvePtr.apfx[i]);
                if (FGlyphCacheItem = nil) then
                  TransformPoint(P1);
                APath.LineTo(P1);
              end;

            TT_PRIM_QSPLINE:
              if (CurvePtr.cpfx > 1) then
              begin
                PNext := PointFXtoPointF(CurvePtr.apfx[0]);
                if (FGlyphCacheItem = nil) then
                  TransformPoint(PNext);

                for i := 0 to CurvePtr.cpfx-2 do
                begin
                  P1 := PNext;

                  P2 := PointFXtoPointF(CurvePtr.apfx[i+1]);
                  if (FGlyphCacheItem = nil) then
                    TransformPoint(P2);

                  PNext := P2;

                  if (i < CurvePtr.cpfx-2) then
                  begin
                    P2.x := (P1.x + P2.x) * OneHalf;
                    P2.y := (P1.y + P2.y) * OneHalf;
                  end;

                  APath.ConicTo(P1, P2);
                end;
              end;

            TT_PRIM_CSPLINE:
              if (CurvePtr.cpfx > 2) then
              begin
                PNext := PointFXtoPointF(CurvePtr.apfx[0]);
                if (FGlyphCacheItem = nil) then
                  TransformPoint(PNext);

                i := 0;
                while (i < CurvePtr.cpfx-2) do
                begin
                  P1 := PNext;

                  P2 := PointFXtoPointF(CurvePtr.apfx[i+1]);
                  if (FGlyphCacheItem = nil) then
                    TransformPoint(P2);

                  P3 := PointFXtoPointF(CurvePtr.apfx[i+2]);
                  if (FGlyphCacheItem = nil) then
                    TransformPoint(P3);

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


    (*
    ** Update glyph cache
    *)
    if (FGlyphCacheItem <> nil) then
      UpdateGlyphCache;

    if (FGlyphCacheItem <> nil) then
      TransformGlyph;

  finally
    EndSession;
  end;

  Result := True;
end;

//------------------------------------------------------------------------------

{$ifdef FPC}

function CompareKerningPair(A, B: PKerningPair): integer;
begin
  Result := (A.wFirst - B.wFirst);
  if (Result = 0) then
    Result := (A.wSecond - B.wSecond);
end;

// Adapted from classes.pas
procedure SortKerningPairs(List: TFontFace32.TKerningPairs);

  procedure QuickSortKerningPairs(L, R: NativeInt);
  var
    I, J: NativeInt;
    T: TKerningPair;
    P: PKerningPair;
  begin
    if L < R then
    begin
      repeat
        if (R - L) = 1 then
        begin
          if CompareKerningPair(@List[L], @List[R]) > 0 then
          begin
            T := List[L];
            List[L] := List[R];
            List[R] := T;
          end;
          break;
        end;
        I := L;
        J := R;
        P := @List[(L + R) shr 1];
        repeat
          while CompareKerningPair(@List[I], P) < 0 do
            Inc(I);
          while CompareKerningPair(@List[J], P) > 0 do
            Dec(J);
          if I <= J then
          begin
            if I <> J then
            begin
              T := List[I];
              List[I] := List[J];
              List[J] := T;
            end;
            Inc(I);
            Dec(J);
          end;
        until I > J;
        if (J - L) > (R - I) then
        begin
          if I < R then
            QuickSortKerningPairs(I, R);
          R := J;
        end
        else
        begin
          if L < J then
            QuickSortKerningPairs(L, J);
          L := I;
        end;
      until L >= R;
    end;
  end;

begin
  if Length(List) > 1 then
    QuickSortKerningPairs(0, High(List));
end;

function BinarySearchKerningPairs(List: TFontFace32.TKerningPairs; const Item: TKerningPair; out FoundIndex: NativeInt): boolean;
var
  L, H, mid: NativeInt;
  cmp: NativeInt;
begin
  if Length(List) = 0 then
  begin
    FoundIndex := 0;
    Exit(False);
  end;

  Result := False;
  L := 0;
  H := High(List);
  while L <= H do
  begin
    mid := L + (H - L) div 2;
    cmp := CompareKerningPair(@List[mid], @Item);
    if cmp < 0 then
      L := mid + 1
    else
    begin
      H := mid - 1;
      if cmp = 0 then
        Result := True;
    end;
  end;
  FoundIndex := L;
end;

{$endif}


function TFontFace32.GetKerning(AFirstGlyph, ASecondGlyph: Cardinal): Single;
var
  Count: integer;
  KerningPair: TKerningPair;
  KerningIndex: NativeInt;
begin
  Result := 0;

  if (AFirstGlyph > High(Word)) or (ASecondGlyph > High(Word)) then
    exit;

  if (not FHasKerning) then
  begin
    FHasKerning := True;

    Count := GetKerningPairs(FFontData.FontDC, 0, nil);

    if (Count > 0) then
    begin

      SetLength(FKerningPairs, Count);

      Count := GetKerningPairs(FFontData.FontDC, Count, @FKerningPairs[0]);

      // Second call might return a different number. This is undocumented :-/
      SetLength(FKerningPairs, Count);

      if (Count = 0) then
        RaiseLastOSError;

{$ifndef FPC}

      FKerningComparer := TComparer<TKerningPair>.Construct(
        function(const A, B: TKerningPair): integer
        begin
          Result := (A.wFirst - B.wFirst);
          if (Result = 0) then
            Result := (A.wSecond - B.wSecond);
        end);

      // The array returned by GetKerningPairs appears to be sorted on First but not
      // on Second so we have to sort it ourselves.
      TArray.Sort<TKerningPair>(FKerningPairs, FKerningComparer);

{$else}

      SortKerningPairs(FKerningPairs);

{$endif}

    end;
  end;

  if (Length(FKerningPairs) = 0) then
    exit;

  KerningPair.wFirst := AFirstGlyph;
  KerningPair.wSecond := ASecondGlyph;

{$ifndef FPC}
  if TArray.BinarySearch<TKerningPair>(FKerningPairs, KerningPair, KerningIndex, FKerningComparer) then
{$else}
  if BinarySearchKerningPairs(FKerningPairs, KerningPair, KerningIndex) then
{$endif}
    Result := FKerningPairs[KerningIndex].iKernAmount * FScale;
end;

//------------------------------------------------------------------------------

function TFontFace32.GetFontFaceMetrics(const ATextLayout: TTextLayout; var AFontFaceMetrics: TFontFaceMetrics32): boolean;
var
  GlyphMetrics: TGlyphMetrics32;
begin
  BeginSession;
  try

    if (not GetGlyphMetrics(Graphics32Unicode.cpEmSpace, GlyphMetrics)) then
      // Fall back to regular space
      GetGlyphMetrics(32, GlyphMetrics);

  finally
    EndSession;
  end;

  AFontFaceMetrics.Valid := True;

  if (ATextLayout.VerticalMetrics = vmTypographic) then
  begin
    AFontFaceMetrics.Ascent := FFontData.OutlineTextMetric.otmAscent * FScale;
    AFontFaceMetrics.Descent := FFontData.OutlineTextMetric.otmDescent * FScale;
    AFontFaceMetrics.LineGap := FFontData.OutlineTextMetric.otmLineGap * FScale;
  end else
  begin
    AFontFaceMetrics.Ascent := FFontData.OutlineTextMetric.otmTextMetrics.tmAscent * FScale;
    AFontFaceMetrics.Descent := FFontData.OutlineTextMetric.otmTextMetrics.tmDescent * FScale;
    AFontFaceMetrics.LineGap := (FFontData.OutlineTextMetric.otmTextMetrics.tmHeight - FFontData.OutlineTextMetric.otmTextMetrics.tmAscent + FFontData.OutlineTextMetric.otmTextMetrics.tmDescent) * FScale;
  end;
  AFontFaceMetrics.EMSize := FFontData.EMSize;

  if (GlyphMetrics.Valid) then
    AFontFaceMetrics.EMSpaceWidth := GlyphMetrics.AdvanceX
  else
    AFontFaceMetrics.EMSpaceWidth := FFontData.OutlineTextMetric.otmTextMetrics.tmAveCharWidth * FScale; // Better than nothing :-/

  Result := True;
end;


//------------------------------------------------------------------------------
//
//      TFontFaceProvider
//
//------------------------------------------------------------------------------
function TFontFaceProvider.CreateFontFace(AFont: TFont): IFontFace32;
begin
  Result := TFontFace32.Create(AFont);
end;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization

// Although the cache compiles on FPC it fails at run-time due to
// bugs in the FPC compiler's generics codegen.
{$ifndef _FPC}

  EnableGlyphCache;

{$endif}

end.
