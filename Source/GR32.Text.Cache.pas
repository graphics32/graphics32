unit GR32.Text.Cache;

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
 * The Initial Developer of the Original Code is Anders Melander
 *
 * Portions created by the Initial Developer are Copyright (C) 2022
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
//------------------------------------------------------------------------------

uses
  Generics.Defaults,
  Generics.Collections,
  Windows,
  Graphics,
  GR32,
  GR32_Paths;


//------------------------------------------------------------------------------
//
//      TDoubleLinked<T>
//
//------------------------------------------------------------------------------
type
  TDoubleLinked<T: class> = record
    Prev: ^TDoubleLinked<T>; // From Head: Least recently used
    Next: ^TDoubleLinked<T>; // From Head: Most recently used
    Value: T;
    procedure InitializeHead;
    function IsEmpty: boolean; inline; // Does not have a value
    function IsHead: boolean; inline; // Is the empty head node
    function IsMRU: boolean; inline; // Is Most Recently Used
    function IsLRU: boolean; inline; // Is Least Recently Used
    procedure Unlink; inline;
    procedure Add(var Link: TDoubleLinked<T>); inline;
  end;


//------------------------------------------------------------------------------
//
//      Platform independent font API
//
//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
type
  TGlyphMetrics32 = record
    Valid: boolean;
    AdvanceWidthX: integer;
  end;

  TFontFaceMetric32 = record
    Valid: boolean;
    Height: integer;
    Ascent: integer;
    Descent: integer;
  end;

  IFontContext32 = interface
    function Comparer: IEqualityComparer<IFontContext32>;

    procedure BeginSession;
    procedure EndSession;

    function GetFontFaceMetric(var AFontFaceMetric: TFontFaceMetric32): boolean;
    function GetGlyphMetric(AGlyph: Cardinal; var AGlyphMetrics: TGlyphMetrics32): boolean;
    function GetGlyphOutline(AGlyph: Cardinal; var AGlyphMetrics: TGlyphMetrics32; APath: TCustomPath; OffsetX: Single = 0; OffsetY: Single = 0; MaxX: Single = -1): boolean;
  end;

  IFontOutlineProvider32 = interface
    function CreateContext(AFont: TFont): IFontContext32;
  end;

//------------------------------------------------------------------------------
//
//
//
//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------
var
  //
  // Default cache size:
  //
  // CacheMaxSize       Max size of cached TrueType polygon data in bytes.
  // CacheMinPurge      Default size to shrink below if entries need to be purged.
  //                    Must be less or equal to CacheMaxSize.
  //
  // Size of a single glyph is typically 400-500 bytes
  //
  CacheMaxSize: uint64 = 256*1024;
  CacheMinPurge: uint64 = 256*1024;


type
  TFontCache = class;
  TFontCacheItem = class;

  TGlyphInfo = class
  private
    FFontCacheItem: TFontCacheItem;
    FGlyph: integer;
    FValid: boolean;
    FGlyphMetrics: TGlyphMetrics32;
    FTTPolygonHeader: PTTPolygonHeader;
    FTTPolygonHeaderSize: DWORD;
{$ifdef FONT_CACHE_PATH}
    FPath: TArrayOfArrayOfFloatPoint;
    FPathClosed: TArray<boolean>;
    FPathValid: boolean;
{$endif FONT_CACHE_PATH}
    FHits: uint64;
    FLRUlink: TDoubleLinked<TGlyphInfo>;
  protected
    property LRUlink: TDoubleLinked<TGlyphInfo> read FLRUlink;
    procedure Hit; // Register a cache hit
  public
    constructor Create(AFontCacheItem: TFontCacheItem; ADC: HDC; AGlyph: integer);
    destructor Destroy; override;

{$ifdef FONT_CACHE_PATH}
    procedure AssignPath(APath: TFlattenedPath; First: integer; OriginX, OriginY: Single);
{$endif FONT_CACHE_PATH}

    property Glyph: integer read FGlyph;
    property Valid: boolean read FValid;

    property GlyphMetrics: TGlyphMetrics32 read FGlyphMetrics;
    property TTPolygonHeader: PTTPolygonHeader read FTTPolygonHeader;
    property TTPolygonHeaderSize: DWORD read FTTPolygonHeaderSize;

{$ifdef FONT_CACHE_PATH}
    property Path: TArrayOfArrayOfFloatPoint read FPath;
    property PathClosed: TArray<boolean> read FPathClosed;
    property PathValid: boolean read FPathValid write FPathValid;
{$endif FONT_CACHE_PATH}
  end;

  TFontCacheItem = class
  private
    FFontCache: TFontCache;
    FFontFaceMetric: TFontFaceMetric32;
    FGlyphCache: TObjectDictionary<integer, TGlyphInfo>;
    FHits: uint64;
  protected
    procedure Hit; // Register a cache hit
    procedure ReserveCacheSpace(Size: uint64);
    procedure UnreserveCacheSpace(Size: uint64);
  public
    constructor Create(AFontCache: TFontCache; DC: HDC);
    destructor Destroy; override;

    function GetGlyphInfo(DC: HDC; Glyph: integer): TGlyphInfo;

    property FontFaceMetric: TFontFaceMetric32 read FFontFaceMetric;
  end;

  TFontCache = class
  private
    FCache: TObjectDictionary<TLogFont, TFontCacheItem>;
    FLRUList: TDoubleLinked<TGlyphInfo>;
    FCacheHits: uint64;
    FCacheMisses: uint64;
    FCachePurges: uint64;
    FCacheSize: uint64;
    FCacheCount: uint64;
    FCacheMaxSize: uint64;
    FCacheMinPurge: uint64;
  protected
    property LRUList: TDoubleLinked<TGlyphInfo> read FLRUList;
    procedure AddCacheItem(GlyphInfo: TGlyphInfo);
    procedure RegisterCacheHit(GlyphInfo: TGlyphInfo);
    procedure RegisterCacheMiss;
    procedure RegisterCachePurge;
    procedure ReserveCacheSpace(Size: uint64);
    procedure UnreserveCacheSpace(Size: uint64);
  public
    constructor Create; overload;
    constructor Create(AMaxSize, AMinPurge: uint64); overload;
    destructor Destroy; override;

    procedure Clear;

    function GetItemByFont(DC: HDC; Font: HFont): TFontCacheItem;
    function GetItemByDC(DC: HDC): TFontCacheItem;

    property CacheSize: uint64 read FCacheSize;
    property CacheCount: uint64 read FCacheCount;
  end;

var
  FontCache: TFontCache = nil;



//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
//  GR32_LowLevel,
  SysUtils,
  GR32.Text.Win;

//------------------------------------------------------------------------------
//
//      TDoubleLinked<T>
//
//------------------------------------------------------------------------------
procedure TDoubleLinked<T>.InitializeHead;
begin
  Value := nil;
  Next := @Self;
  Prev := @Self;
end;

function TDoubleLinked<T>.IsEmpty: boolean;
begin
  Result := (Prev = Next);
end;

function TDoubleLinked<T>.IsHead: boolean;
begin
  // Head is just an item that holds the pointer to the first and last entries
  // and connects them to form a circular list.
  Result := (Value = nil);
end;

function TDoubleLinked<T>.IsMRU: boolean;
begin
  Result := (Prev <> nil) and (Prev.IsHead);
end;

function TDoubleLinked<T>.IsLRU: boolean;
begin
  Result := (Next <> nil) and (Next.IsHead);
end;

procedure TDoubleLinked<T>.Unlink;
begin
  if (Prev <> nil) then
    Prev.Next := Next;
  if (Next <> nil) then
    Next.Prev := Prev;
  Prev := nil;
  Next := nil;
end;

procedure TDoubleLinked<T>.Add(var Link: TDoubleLinked<T>);
begin
  Link.Next := Next;
  Link.Prev := @Self;

  Next.Prev := @Link;
  Next := @Link;
end;

//------------------------------------------------------------------------------
//
//      TGlyphInfo
//
//------------------------------------------------------------------------------
constructor TGlyphInfo.Create(AFontCacheItem: TFontCacheItem; ADC: HDC; AGlyph: integer);
var
  GlyphMetrics: TGlyphMetrics;
begin
  inherited Create;

  FFontCacheItem := AFontCacheItem;
  FGlyph := AGlyph;
  FLRUlink.Value := Self;

  GlyphMetrics := Default(TGlyphMetrics);
  FTTPolygonHeaderSize := GetGlyphOutline(ADC, FGlyph, GGODefaultFlags[UseHinting], GlyphMetrics, 0, nil, VertFlip_mat2);

  if (FTTPolygonHeaderSize <> GDI_ERROR) then
    FGlyphMetrics.AdvanceWidthX := GlyphMetrics.gmCellIncX
  else
    FGlyphMetrics := Default(TGlyphMetrics32);

  if (FTTPolygonHeaderSize <> 0) then
  begin
    FFontCacheItem.ReserveCacheSpace(FTTPolygonHeaderSize);

    GetMem(FTTPolygonHeader, FTTPolygonHeaderSize);
    try
      try

        FTTPolygonHeaderSize := GetGlyphOutline(ADC, FGlyph, GGODefaultFlags[UseHinting], GlyphMetrics, FTTPolygonHeaderSize, FTTPolygonHeader, VertFlip_mat2);

        FValid := (FTTPolygonHeaderSize <> GDI_ERROR) and (FTTPolygonHeader^.dwType = TT_POLYGON_TYPE);

      except
        FValid := False;
        raise;
      end;
    finally
      if (not FValid) then
      begin
        FreeMem(FTTPolygonHeader);
        FTTPolygonHeader := nil;
      end;
    end;
  end else
    FValid := False;
end;

destructor TGlyphInfo.Destroy;
begin
  FLRUlink.Unlink;

  if (FValid) then
    FreeMem(FTTPolygonHeader);

  FFontCacheItem.UnreserveCacheSpace(TTPolygonHeaderSize);
  FFontCacheItem.FGlyphCache.ExtractPair(Glyph);

  inherited;
end;

{$ifdef FONT_CACHE_PATH}
procedure TGlyphInfo.AssignPath(APath: TFlattenedPath; First: integer; OriginX, OriginY: Single);
var
  i, j, i2: integer;
begin
  SetLength(FPath, Length(APath.Path)-First);
  SetLength(FPathClosed, Length(FPath));
  i2 := 0;
  for i := First to High(APath.Path) do
  begin
    SetLength(FPath[i2], Length(APath.Path[i]));
    for j := 0 to High(APath.Path[i]) do
    begin
      FPath[i2][j] := APath.Path[i][j];
      FPath[i2][j].X := FPath[i2][j].X - OriginX;
      FPath[i2][j].Y := FPath[i2][j].Y - OriginY;
    end;
    FPathClosed[i2] := APath.PathClosed[i];
    inc(i2);
  end;
  FPathValid := True;
end;
{$endif FONT_CACHE_PATH}

procedure TGlyphInfo.Hit;
begin
  Inc(FHits);
end;


//------------------------------------------------------------------------------
//
//      TFontCacheItem
//
//------------------------------------------------------------------------------
constructor TFontCacheItem.Create(AFontCache: TFontCache; DC: HDC);
var
  FontFaceMetric: TTextMetric;
begin
  inherited Create;

  FFontCache := AFontCache;
  FGlyphCache := TObjectDictionary<integer, TGlyphInfo>.Create([doOwnsValues]);

  GetTextMetrics(DC, FontFaceMetric);

  FFontFaceMetric.Height := FontFaceMetric.tmHeight;
  FFontFaceMetric.Ascent := FontFaceMetric.tmAscent;
  FFontFaceMetric.Descent := FontFaceMetric.tmDescent;
end;

destructor TFontCacheItem.Destroy;
begin
  FGlyphCache.Clear;
  FGlyphCache.Free;

  inherited;
end;

function TFontCacheItem.GetGlyphInfo(DC: HDC; Glyph: integer): TGlyphInfo;
begin
  if (FGlyphCache.TryGetValue(Glyph, Result)) then
  begin
    FFontCache.RegisterCacheHit(Result);
    exit;
  end;

  Result := TGlyphInfo.Create(Self, DC, Glyph);
  FGlyphCache.Add(Glyph, Result);
  FFontCache.AddCacheItem(Result);
end;

procedure TFontCacheItem.Hit;
begin
  Inc(FHits);
end;

procedure TFontCacheItem.ReserveCacheSpace(Size: uint64);
begin
  FFontCache.ReserveCacheSpace(Size);
end;

procedure TFontCacheItem.UnreserveCacheSpace(Size: uint64);
begin
  FFontCache.UnreserveCacheSpace(Size);
end;


//------------------------------------------------------------------------------
//
//      TFontCache
//
//------------------------------------------------------------------------------
constructor TFontCache.Create;
begin
  Create(CacheMaxSize, CacheMinPurge);
end;

constructor TFontCache.Create(AMaxSize, AMinPurge: uint64);
begin
  Assert(AMaxSize >= AMinPurge);

  inherited Create;

  FCacheMaxSize := AMaxSize;
  FCacheMinPurge := AMinPurge;
  FCache := TObjectDictionary<TLogFont, TFontCacheItem>.Create([doOwnsValues]);
  FLRUList.InitializeHead;
end;

destructor TFontCache.Destroy;
begin
  FCache.Free;
  inherited;
end;

procedure TFontCache.Clear;
begin
  FCache.Clear;
end;

function TFontCache.GetItemByFont(DC: HDC; Font: HFont): TFontCacheItem;
var
  LogFont: TLogFont;
  Size: integer;
  i: integer;
  Clear: boolean;
begin
  FillChar(LogFont, SizeOf(TLogFont), 0);
  Size := GetObject(Font, SizeOf(TLogFont), @LogFont);
  if (Size <> SizeOf(TLogFont)) then
    raise Exception.Create('Failed to retrieve LOGFONT');

  // Clear junk
  Clear := False;
  for i := 0 to High(LogFont.lfFaceName) do
  begin
    if (Clear) then
      LogFont.lfFaceName[i] := #0
    else
      Clear := (LogFont.lfFaceName[i] = #0);
  end;

  if (FCache.TryGetValue(LogFont, Result)) then
  begin
    Result.Hit;
    exit;
  end;

  Result := TFontCacheItem.Create(Self, DC);
  FCache.Add(LogFont, Result);
end;

procedure TFontCache.AddCacheItem(GlyphInfo: TGlyphInfo);
begin
  // Insert item at start of LRU list
  FLRUList.Add(GlyphInfo.FLRUlink);
end;

procedure TFontCache.RegisterCacheHit(GlyphInfo: TGlyphInfo);
begin
  GlyphInfo.Hit;
  Inc(FCacheHits);

  // Move item to start of LRU list unless it's already there
  // Make item the MRU
  if (not GlyphInfo.LRUlink.IsMRU) then
  begin
    GlyphInfo.LRUlink.Unlink;
    FLRUList.Add(GlyphInfo.FLRUlink);
  end;
end;

procedure TFontCache.RegisterCacheMiss;
begin
  Inc(FCacheMisses);
end;

procedure TFontCache.RegisterCachePurge;
begin
  Inc(FCachePurges);
end;

procedure TFontCache.ReserveCacheSpace(Size: uint64);
begin
  Inc(FCacheCount);
  Inc(FCacheSize, Size);

  // Make sure there's room for the new item in the cache
  if (FCacheSize <= FCacheMaxSize) then
    exit;

  // Purge LRU items from cache until total size has reached threshold
  while (FCacheSize >= FCacheMinPurge) and (not FLRUList.IsEmpty) do
    FLRUList.Prev.Value.Free;
end;

procedure TFontCache.UnreserveCacheSpace(Size: uint64);
begin
  Dec(FCacheCount);
  Dec(FCacheSize, Size);
  Assert(FCacheSize >= 0);
end;

function TFontCache.GetItemByDC(DC: HDC): TFontCacheItem;
var
  Font: HFONT;
begin
  Font := GetCurrentObject(DC, OBJ_FONT);
  Result := GetItemByFont(DC, Font);
end;


end.
