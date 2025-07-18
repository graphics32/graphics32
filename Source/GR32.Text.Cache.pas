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
 * The Original Code is Text Layout Engine for Graphics32
 *
 * The Initial Developer of the Original Code is Anders Melander
 *
 * Portions created by the Initial Developer are Copyright (C) 2022-2025
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

//------------------------------------------------------------------------------
//
//      This unit should be considered internal to Graphics32.
//
//------------------------------------------------------------------------------

uses
  Generics.Collections,
  Graphics,
  GR32,
  GR32_Paths,
  GR32.Text.FontFace;


//------------------------------------------------------------------------------
//
//      TDoubleLinked<T>
//
//------------------------------------------------------------------------------
type
  TDoubleLinked<T: class> = record
{$if defined(GENERIC_POINTERS)}
    Prev: ^TDoubleLinked<T>; // From Head: Least recently used
    Next: ^TDoubleLinked<T>; // From Head: Most recently used
{$else}
    Prev: pointer;
    Next: pointer;
{$ifend}
    Value: T;
    procedure InitializeHead;
    function IsEmpty: boolean; inline; // Does not have a value
    function IsHead: boolean; inline; // Is the empty head node
    function IsMRU: boolean; inline; // Is Most Recently Used
    function IsLRU: boolean; inline; // Is Least Recently Used
    procedure Unlink; inline;
    procedure Add(var Link: TDoubleLinked<T>); inline;
    function PrevValue: T;
  end;


//------------------------------------------------------------------------------
//
//      Glyph cache
//
//------------------------------------------------------------------------------
// Implements a two level hierarchical LRU cache of font glyph outline data:
//
//   1) Font face.
//      Cache identity is a byte string, typically containing font name, weight,
//      etc.
//      Level 1 data is never evicted from the cache.
//
//   2) Glyph.
//      Cache identity is the glyph ID.
//      Level 2 data is evicted based on LRU; The Least Recently Used
//      items are evicted until the target criteria is met.
//
//  The eviction criteria is cache size in bytes.
//
//------------------------------------------------------------------------------
// The cache is not thread safe.
//------------------------------------------------------------------------------
var
  //
  // Default cache sizes:
  //
  //   CacheMaxSize     Max size of cached glyph polygon data in bytes.
  //
  //   CacheMinPurge    Default size to shrink below if entries needs to be
  //                    purged.
  //                    Must be less or equal to CacheMaxSize.
  //
  // The size of a single glyph is typically 500 - 1000 bytes
  //
  CacheMaxSize: uint64 = 256*1024;
  CacheMinPurge: uint64 = 256*1024;

  // Initial capacity of font cache dictionary
  CacheDefaultCount: Cardinal = 32;
  // Initial capacity of glyph cache dictionary
  CacheDefaultGlyphCount: Cardinal = 64;


//------------------------------------------------------------------------------
//
//      CacheEMSize
//
//------------------------------------------------------------------------------
// Specifies the EM size of the glyph data stored in the cache.
//
// - Smaller value: Faster rendering, lower quality, less memory (negligible).
//   glyphs.
//
// - Larger value: Slower rendering, higher quality, more memory (negligible).
//
// The font EM size (the resolution the font was designed at) is usually 2048.
// It is recommended that the max resolution not be set lower than half the
// maximum font size that will be rendered, although even then the degradation
// in quality can be hard to spot visually.
//------------------------------------------------------------------------------
var
  CacheEMSize: Cardinal = 128;


//------------------------------------------------------------------------------
//
//      IGlyphCacheData
//
//------------------------------------------------------------------------------
// Contains glyph metrics and outline data for a single glyph.
// The scale of values returned is implementation dependent but they are usually
// in EM size.
//------------------------------------------------------------------------------
type
  IGlyphCacheData = interface
    function GetGlyphMetrics: TGlyphMetrics32;
    function GetPath: TArrayOfArrayOfFloatPoint;
    function GetPathClosed: TArray<boolean>;
    function GetPathValid: boolean;
    function GetInstance: TObject;

    // Assign from path to cache
    procedure LoadFromPath(APath: TFlattenedPath; AFirst: integer; AOriginX, AOriginY: Single; AScale: Double);

    property GlyphMetrics: TGlyphMetrics32 read GetGlyphMetrics;
    property Path: TArrayOfArrayOfFloatPoint read GetPath;
    property PathClosed: TArray<boolean> read GetPathClosed;
    property PathValid: boolean read GetPathValid;

    // FPC doesn't support object-to-interface casting (but also doesn't warn us when we do it. Pfft!)
    // Regardless, using an instance getter is much faster.
    property Instance: TObject read GetInstance;
  end;


//------------------------------------------------------------------------------
//
//      IGlyphCacheFontItem
//
//------------------------------------------------------------------------------
// Represent a single font face.
//------------------------------------------------------------------------------
type
  IGlyphCacheFontItem = interface
    function GetInstance: TObject;

    function FindGlyphData(AGlyph: Cardinal): IGlyphCacheData;
    function AddGlyphData(AGlyph: Cardinal; const AGlyphMetrics: TGlyphMetrics32): IGlyphCacheData;

    property Instance: TObject read GetInstance;
  end;


//------------------------------------------------------------------------------
//
//      IGlyphCache
//
//------------------------------------------------------------------------------
// Represents the glyph cache.
//------------------------------------------------------------------------------
type
  IGlyphCache = interface
    function GetInstance: TObject;

    function FindItem(const AKey: TFontKey): IGlyphCacheFontItem;
    function AddItem(const AKey: TFontKey): IGlyphCacheFontItem;

    property Instance: TObject read GetInstance;
  end;


//------------------------------------------------------------------------------
//
//      GlyphCache
//
//------------------------------------------------------------------------------
// Glyph cache access point.
//------------------------------------------------------------------------------

// Returns a reference to the glyph cache, if enabled. Otherwise returns nil.
// The cache is lazily instantiated on the first call to GlyphCache.
function GlyphCache: IGlyphCache;

// Registers a custom glyph cache and return the existing, if any.
// Note that registering a custom glyph cache will not implicitly enable caching.
// If AGlyphCache is nil then the default cache will be used.
function RegisterGlyphCache(const AGlyphCache: IGlyphCache): IGlyphCache;

// Enable the glyph cache.
procedure EnableGlyphCache;

// Disable the glyph cache.
procedure DisableGlyphCache;


//------------------------------------------------------------------------------
//
//      TGlyphCache
//
//------------------------------------------------------------------------------
// Implements IGlyphCache
//------------------------------------------------------------------------------
type
  TGlyphCache = class;
  TGlyphCacheFontItem = class;

  TGlyphCacheData = class(TInterfacedObject, IGlyphCacheData)
  private
    FFontCacheItem: TGlyphCacheFontItem;
    FGlyph: Cardinal;
    FGlyphMetrics: TGlyphMetrics32;

    FPath: TArrayOfArrayOfFloatPoint;
    FPathClosed: TArray<boolean>;
    FPathValid: boolean;
    FSizeInBytes: UInt64;

    FHits: UInt64;
    FLRUlink: TDoubleLinked<TGlyphCacheData>;

  protected
    property LRUlink: TDoubleLinked<TGlyphCacheData> read FLRUlink;
    procedure Hit; // Register a cache hit
    procedure Evict;

    property SizeInBytes: UInt64 read FSizeInBytes;

  private
    // IGlyphCacheData
    function GetGlyphMetrics: TGlyphMetrics32;
    function GetPath: TArrayOfArrayOfFloatPoint;
    function GetPathClosed: TArray<boolean>;
    function GetPathValid: boolean;
    function GetInstance: TObject;
    procedure LoadFromPath(APath: TFlattenedPath; AFirst: integer; AOriginX, AOriginY: Single; AScale: Double);
  public
    constructor Create(AFontCacheItem: TGlyphCacheFontItem; AGlyph: Cardinal; const AGlyphMetrics: TGlyphMetrics32);
    destructor Destroy; override;

    property Glyph: Cardinal read FGlyph;

    property GlyphMetrics: TGlyphMetrics32 read FGlyphMetrics;

    property Path: TArrayOfArrayOfFloatPoint read FPath;
    property PathClosed: TArray<boolean> read FPathClosed;
    property PathValid: boolean read FPathValid write FPathValid;
  end;

//------------------------------------------------------------------------------

  TGlyphCacheFontItem = class(TInterfacedObject, IGlyphCacheFontItem)
  private
    FFontCache: TGlyphCache;
    FGlyphCache: TDictionary<Cardinal, IGlyphCacheData>;
    FHits: UInt64;

  protected
    procedure Hit; // Register a cache hit
    procedure ReserveCacheSpace(Size: UInt64);
    procedure UnreserveCacheSpace(Size: UInt64);
    procedure EvictItem(Item: TGlyphCacheData);

  private
    // IGlyphCacheFontItem
    function GetInstance: TObject;
    function FindGlyphData(AGlyph: Cardinal): IGlyphCacheData;
    function AddGlyphData(AGlyph: Cardinal; const AGlyphMetrics: TGlyphMetrics32): IGlyphCacheData;

  public
    constructor Create(AFontCache: TGlyphCache);
    destructor Destroy; override;
  end;

//------------------------------------------------------------------------------

  TGlyphCache = class(TInterfacedObject, IGlyphCache)
  private
    FCache: TDictionary<TFontKey, IGlyphCacheFontItem>;
    FLRUList: TDoubleLinked<TGlyphCacheData>;
    FCacheHits: UInt64;
    FCacheMisses: UInt64;
    FCachePurges: UInt64;
    FCacheSize: UInt64;
    FCacheCount: UInt64;
    FCacheMaxSize: UInt64;
    FCacheMinPurge: UInt64;

  protected
    property LRUList: TDoubleLinked<TGlyphCacheData> read FLRUList;
    procedure AddCacheItem(GlyphInfo: TGlyphCacheData);
    procedure RegisterCacheHit(GlyphInfo: TGlyphCacheData);
    procedure RegisterCacheMiss;
    procedure RegisterCachePurge;
    procedure ReserveCacheSpace(Size: UInt64);
    procedure UnreserveCacheSpace(Size: UInt64);

  private
    // IGlyphCache
    function GetInstance: TObject;
    function FindItem(const AKey: TFontKey): IGlyphCacheFontItem;
    function AddItem(const AKey: TFontKey): IGlyphCacheFontItem;

  public
    constructor Create; overload;
    constructor Create(AMaxSize, AMinPurge: UInt64); overload;
    destructor Destroy; override;

    procedure Clear;

    property CacheSize: UInt64 read FCacheSize;
    property CacheCount: UInt64 read FCacheCount;
  end;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  SysUtils;

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
{$if defined(GENERIC_POINTERS)}
  Result := (Prev <> nil) and (Prev.IsHead);
{$else}
  Result := (Prev <> nil) and (TDoubleLinked<T>(Prev^).IsHead);
{$ifend}
end;

function TDoubleLinked<T>.PrevValue: T;
begin
{$if defined(GENERIC_POINTERS)}
  Result := Prev.Value;
{$else}
  Result := TDoubleLinked<T>(Prev^).Value;
{$ifend}
end;

function TDoubleLinked<T>.IsLRU: boolean;
begin
{$if defined(GENERIC_POINTERS)}
  Result := (Next <> nil) and (Next.IsHead);
{$else}
  Result := (Next <> nil) and (TDoubleLinked<T>(Next^).IsHead);
{$ifend}
end;

procedure TDoubleLinked<T>.Unlink;
begin
{$if defined(GENERIC_POINTERS)}
  if (Prev <> nil) then
    Prev.Next := Next;
  if (Next <> nil) then
    Next.Prev := Prev;
{$else}
  if (Prev <> nil) then
    TDoubleLinked<T>(Prev^).Next := Next;
  if (Next <> nil) then
    TDoubleLinked<T>(Next^).Prev := Prev;
{$ifend}
  Prev := nil;
  Next := nil;
end;

procedure TDoubleLinked<T>.Add(var Link: TDoubleLinked<T>);
begin
  Link.Next := Next;
  Link.Prev := @Self;

{$if defined(GENERIC_POINTERS)}
  Next.Prev := @Link;
{$else}
  TDoubleLinked<T>(Next^).Prev := @Link;
{$ifend}
  Next := @Link;
end;


//------------------------------------------------------------------------------
//
//      TGlyphCacheData
//
//------------------------------------------------------------------------------
constructor TGlyphCacheData.Create(AFontCacheItem: TGlyphCacheFontItem; AGlyph: Cardinal; const AGlyphMetrics: TGlyphMetrics32);
begin
  inherited Create;

  FFontCacheItem := AFontCacheItem;
  FGlyph := AGlyph;
  FGlyphMetrics := AGlyphMetrics;

  FLRUlink.Value := Self;

  FFontCacheItem.ReserveCacheSpace(SizeOf(FGlyphMetrics));
end;

destructor TGlyphCacheData.Destroy;
begin
  FLRUlink.Unlink;

  FFontCacheItem.UnreserveCacheSpace(FSizeInBytes);
  FFontCacheItem.FGlyphCache.ExtractPair(Glyph);

  inherited;
end;

procedure TGlyphCacheData.Evict;
begin
  FLRUlink.Unlink;
  FFontCacheItem.EvictItem(Self);
end;

function TGlyphCacheData.GetGlyphMetrics: TGlyphMetrics32;
begin
  Result := FGlyphMetrics;
end;

function TGlyphCacheData.GetInstance: TObject;
begin
  Result := Self;
end;

function TGlyphCacheData.GetPath: TArrayOfArrayOfFloatPoint;
begin
  Result := FPath;
end;

function TGlyphCacheData.GetPathClosed: TArray<boolean>;
begin
  Result := FPathClosed;
end;

function TGlyphCacheData.GetPathValid: boolean;
begin
  Result := FPathValid;
end;

procedure TGlyphCacheData.LoadFromPath(APath: TFlattenedPath; AFirst: integer; AOriginX, AOriginY: Single; AScale: Double);
var
  i, j, i2: integer;
  pDest, pSource: PFloatPoint;
  Path: TArrayOfArrayOfFloatPoint;
begin
  if (FSizeInBytes > 0) then
  begin
    FFontCacheItem.UnreserveCacheSpace(FSizeInBytes - SizeOf(FGlyphMetrics));
    Dec(FSizeInBytes, SizeOf(FGlyphMetrics));
  end;

  Path := APath.Path; // Avoid going through the getter repeatedly

  for i := AFirst to High(Path) do
    Inc(FSizeInBytes, Length(Path[i]) * (SizeOf(TFloatPoint) + SizeOf(Boolean)));

  FFontCacheItem.ReserveCacheSpace(FSizeInBytes);

  SetLength(FPath, Length(Path) - AFirst);
  SetLength(FPathClosed, Length(FPath));

  i2 := 0;
  for i := AFirst to High(Path) do
  begin
    if (Length(Path[i]) = 0) then
      continue;

    SetLength(FPath[i2], Length(Path[i]));

    pDest := @FPath[i2][0];
    pSource := @APath.Path[i][0];

    for j := 0 to High(Path[i]) do
    begin
      pDest.X := (pSource.X - AOriginX) * AScale;
      pDest.Y := (pSource.Y - AOriginY) * AScale;

      Inc(pDest);
      Inc(pSource);
    end;

    FPathClosed[i2] := APath.PathClosed[i];
    inc(i2);
  end;

  // Trim arrays in case any of the source arrays were empty
  SetLength(FPath, i2);
  SetLength(FPathClosed, i2);

  FPathValid := True;
end;

procedure TGlyphCacheData.Hit;
begin
  Inc(FHits);
end;


//------------------------------------------------------------------------------
//
//      TGlyphCacheFontItem
//
//------------------------------------------------------------------------------
constructor TGlyphCacheFontItem.Create(AFontCache: TGlyphCache);
begin
  inherited Create;

  FFontCache := AFontCache;
  FGlyphCache := TDictionary<Cardinal, IGlyphCacheData>.Create(CacheDefaultGlyphCount);
end;

destructor TGlyphCacheFontItem.Destroy;
begin
  FGlyphCache.Clear;
  FGlyphCache.Free;

  inherited;
end;

procedure TGlyphCacheFontItem.EvictItem(Item: TGlyphCacheData);
begin
  FGlyphCache.Remove(Item.Glyph);
end;

function TGlyphCacheFontItem.FindGlyphData(AGlyph: Cardinal): IGlyphCacheData;
begin
  if (FGlyphCache.TryGetValue(AGlyph, Result)) then
    FFontCache.RegisterCacheHit(TGlyphCacheData(Result.Instance))
  else
    Result := nil;
end;

function TGlyphCacheFontItem.GetInstance: TObject;
begin
  Result := Self;
end;

function TGlyphCacheFontItem.AddGlyphData(AGlyph: Cardinal; const AGlyphMetrics: TGlyphMetrics32): IGlyphCacheData;
begin
  Result := TGlyphCacheData.Create(Self, AGlyph, AGlyphMetrics);
  FGlyphCache.Add(AGlyph, Result);

  FFontCache.AddCacheItem(TGlyphCacheData(Result.Instance));
end;

procedure TGlyphCacheFontItem.Hit;
begin
  Inc(FHits);
end;

procedure TGlyphCacheFontItem.ReserveCacheSpace(Size: UInt64);
begin
  FFontCache.ReserveCacheSpace(Size);
end;

procedure TGlyphCacheFontItem.UnreserveCacheSpace(Size: UInt64);
begin
  FFontCache.UnreserveCacheSpace(Size);
end;


//------------------------------------------------------------------------------
//
//      TGlyphCache
//
//------------------------------------------------------------------------------
constructor TGlyphCache.Create;
begin
  Create(CacheMaxSize, CacheMinPurge);
end;

constructor TGlyphCache.Create(AMaxSize, AMinPurge: UInt64);
begin
  Assert(AMaxSize >= AMinPurge);

  inherited Create;

  FCacheMaxSize := AMaxSize;
  FCacheMinPurge := AMinPurge;

  FCache := TDictionary<TFontKey, IGlyphCacheFontItem>.Create(CacheDefaultCount);

  FLRUList.InitializeHead;
end;

destructor TGlyphCache.Destroy;
begin
  FCache.Free;
  inherited;
end;

procedure TGlyphCache.Clear;
begin
  FCache.Clear;
end;

function TGlyphCache.FindItem(const AKey: TFontKey): IGlyphCacheFontItem;
begin
  if (FCache.TryGetValue(AKey, Result)) then
    TGlyphCacheFontItem(Result.Instance).Hit
  else
    Result := nil;
end;

function TGlyphCache.GetInstance: TObject;
begin
  Result := Self;
end;

function TGlyphCache.AddItem(const AKey: TFontKey): IGlyphCacheFontItem;
begin
  Result := TGlyphCacheFontItem.Create(Self);
  FCache.Add(AKey, Result);
end;

procedure TGlyphCache.AddCacheItem(GlyphInfo: TGlyphCacheData);
begin
  // Insert item at start of LRU list
  FLRUList.Add(GlyphInfo.FLRUlink);
end;

procedure TGlyphCache.RegisterCacheHit(GlyphInfo: TGlyphCacheData);
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

procedure TGlyphCache.RegisterCacheMiss;
begin
  Inc(FCacheMisses);
end;

procedure TGlyphCache.RegisterCachePurge;
begin
  Inc(FCachePurges);
end;

procedure TGlyphCache.ReserveCacheSpace(Size: UInt64);
var
  TargetPurgeSize: Int64;
  Item: TGlyphCacheData;
begin
  Inc(FCacheCount);
  Inc(FCacheSize, Size);

  // Make sure there's room for the new item in the cache
  if (FCacheSize <= FCacheMaxSize) then
    exit;

  TargetPurgeSize := FCacheSize - FCacheMinPurge;

  // Purge LRU items from cache until total size has reached threshold
  while (TargetPurgeSize > 0) and (FCacheSize >= FCacheMinPurge) and (not FLRUList.IsEmpty) do
  begin

    // The item to be evicted
    Item := FLRUList.PrevValue;

    // The cache size potentially doesn't decrease after eviction if the
    // cache object is being held alive by an external reference.
    // Since we need to avoid an endless loop here, we calculate the
    // theoretical purged size manually here instead of just monitoring
    // the cache size.
    Dec(TargetPurgeSize, Item.SizeInBytes);

    Item.Evict;

  end;
end;

procedure TGlyphCache.UnreserveCacheSpace(Size: UInt64);
begin
  Assert(Size <= FCacheSize);
  Dec(FCacheCount);
  Dec(FCacheSize, Size);
end;


//------------------------------------------------------------------------------
//
//      GlyphCache
//
//------------------------------------------------------------------------------
var
  FGlyphCacheEnabled: boolean = False;
  FGlyphCache: IGlyphCache = nil;
  FCustomGlyphCache: IGlyphCache = nil;

procedure EnableGlyphCache;
begin
  FGlyphCacheEnabled := True;
end;

//------------------------------------------------------------------------------

procedure DisableGlyphCache;
begin
  if (FGlyphCache <> nil) then
  begin
    TGlyphCache(FGlyphCache.Instance).Clear;
    FGlyphCache := nil;
  end;

  FGlyphCacheEnabled := False;
end;

//------------------------------------------------------------------------------

function RegisterGlyphCache(const AGlyphCache: IGlyphCache): IGlyphCache;
begin
  Result := FCustomGlyphCache;

  if (AGlyphCache = FCustomGlyphCache) then
    exit;

  FCustomGlyphCache := AGlyphCache;
end;

//------------------------------------------------------------------------------

function GlyphCache: IGlyphCache;
begin
  if (FGlyphCacheEnabled) then
  begin
    if (FCustomGlyphCache <> nil) then
      Result := FCustomGlyphCache
    else
    if (FGlyphCache <> nil) then
      Result := FGlyphCache
    else
    begin
      FGlyphCache := TGlyphCache.Create;
      Result := FGlyphCache;
    end;
  end else
    Result := nil;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization

finalization

  DisableGlyphCache;

end.
