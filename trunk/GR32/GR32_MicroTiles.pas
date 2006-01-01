unit GR32_MicroTiles;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
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
 * The Original Code is MicroTiles Repaint Optimizer Extension for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Andre Beckedorf - metaException OHG
 * Andre@metaException.de
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2006
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}
{-$DEFINE CODESITE}
{-$DEFINE CODESITE_HIGH}
{-$DEFINE PROFILINGDRYRUN}
{-$DEFINE MICROTILES_DEBUGDRAW}
  {-$DEFINE MICROTILES_DEBUGDRAW_UNOPTIMIZED}
{-$DEFINE MICROTILES_NO_ADAPTION}
  {-$DEFINE MICROTILES_NO_ADAPTION_FORCE_WHOLETILES}

uses
  {$IFDEF CLX}
  Qt, {$IFDEF LINUX}Libc, {$ELSE}Windows, {$ENDIF}
  {$ELSE}
  Windows,
  {$ENDIF}
  {$IFDEF CODESITE}CSIntf, {$ENDIF}
  {$IFDEF COMPILER2005}Types, {$ENDIF}
  SysUtils, Classes,
  GR32, GR32_System, GR32_Containers, GR32_Layers, GR32_RepaintOpt;

const
  MICROTILE_SHIFT = 5;
  MICROTILE_SIZE = 1 shl MICROTILE_SHIFT;

  MICROTILE_EMPTY = 0;
  // MICROTILE_EMPTY -> Left: 0, Top: 0, Right:  0, Bottom:  0

  MICROTILE_FULL = MICROTILE_SIZE shl 8 or MICROTILE_SIZE;
  // MICROTILE_FULL -> Left: 0, Top: 0, Right: MICROTILE_SIZE, Bottom: MICROTILE_SIZE

type
  PMicroTile = ^TMicroTile;
  TMicroTile = type Integer;

  PMicroTileArray = ^TMicroTileArray;
  TMicroTileArray = array[0..MaxListSize - 1] of TMicroTile;

  PPMicroTiles = ^PMicroTiles;
  PMicroTiles = ^TMicroTiles;
  TMicroTiles = record
    BoundsRect: TRect;
    Columns, Rows: Integer;
    BoundsUsedTiles: TRect;
    Count: Integer;
    Tiles: PMicroTileArray;
  end;

// MicroTile auxiliary routines
function MakeMicroTile(const Left, Top, Right, Bottom: Integer): TMicroTile; {$IFDEF USEINLINING} inline; {$ENDIF}
function MicroTileHeight(const Tile: TMicroTile): Integer; {$IFDEF USEINLINING} inline; {$ENDIF}
function MicroTileWidth(const Tile: TMicroTile): Integer; {$IFDEF USEINLINING} inline; {$ENDIF}

var
  MicroTileUnion: procedure(var DstTile: TMicroTile; const SrcTile: TMicroTile);

// MicroTiles auxiliary routines
function MakeEmptyMicroTiles: TMicroTiles;
procedure MicroTilesCreate(var MicroTiles: TMicroTiles);
procedure MicroTilesDestroy(var MicroTiles: TMicroTiles);
procedure MicroTilesSetSize(var MicroTiles: TMicroTiles; const DstRect: TRect);
procedure MicroTilesClear(var MicroTiles: TMicroTiles);
procedure MicroTilesClearUsed(var MicroTiles: TMicroTiles);
procedure MicroTilesCopy(var DstTiles: TMicroTiles; SrcTiles: TMicroTiles);
procedure MicroTilesAddLine(var MicroTiles: TMicroTiles; X1, Y1, X2, Y2: Integer; LineWidth: Integer; RoundToWholeTiles: Boolean = False);
procedure MicroTilesAddRect(var MicroTiles: TMicroTiles; Rect: TRect; RoundToWholeTiles: Boolean = False);
procedure MicroTilesUnion(var DstTiles: TMicroTiles; const SrcTiles: TMicroTiles; RoundToWholeTiles: Boolean = False);
function MicroTilesCalcRects(const MicroTiles: TMicroTiles; DstRects: TRectList; CountOnly: Boolean = False; RoundToWholeTiles: Boolean = False): Integer; overload;
function MicroTilesCalcRects(const MicroTiles: TMicroTiles; DstRects: TRectList; const Clip: TRect; CountOnly: Boolean = False; RoundToWholeTiles: Boolean = False): Integer; overload;
function MicroTilesCountEmptyTiles(const MicroTiles: TMicroTiles): Integer;

type
  { TMicroTilesMap }
  { associative array that is used to map Layers to their MicroTiles }
  TMicroTilesMap = class(TPointerMap)
  private
    function GetData(Item: Pointer): PMicroTiles;
    procedure SetData(Item: Pointer; const Data: PMicroTiles);
  protected
    function Delete(BucketIndex: Integer; ItemIndex: Integer): Pointer; override;
  public
    function Add(Item: Pointer): PPMicroTiles;
    property Data[Item: Pointer]: PMicroTiles read GetData write SetData; default;
  end;


type
  { TMicroTilesRepaintOptimizer }
  { Repaint manager that optimizes the repaint process using MicroTiles }
  TMicroTilesRepaintOptimizer = class(TCustomRepaintOptimizer)
  private
    // working tiles
    FWorkMicroTiles: PMicroTiles; // used by DrawLayerToMicroTiles
    FTempTiles: TMicroTiles;
    FInvalidTiles: TMicroTiles;
    FForcedInvalidTiles: TMicroTiles;

    // list of invalid layers
    FInvalidLayers: TList;

    // association that maps layers to their old invalid tiles
    FOldInvalidTilesMap: TMicroTilesMap;

    FWorkingTilesValid: Boolean;
    FOldInvalidTilesValid: Boolean;
    FUseInvalidTiles: Boolean;

    // adaptive stuff...
    FAdaptiveMode: Boolean;

    FPerfTimer: TPerfTimer;
    FPerformanceLevel: Integer;
    FTimeNeededForLastRepaint: Int64;
    FTimeNeededForFullSceneRepaint: Int64;
    FAdaptionFailed: Boolean;

    // vars for time based approach
    FTimedCheck: Boolean;
    FTimeDelta: Integer;
    FNextCheck: Integer;
    FTimeNeededOnLastPenalty: Int64;

    // vars for invalid rect difference approach
    FOldInvalidRectsCount: Integer;

{$IFDEF MICROTILES_DEBUGDRAW}
    FDebugWholeTiles: Boolean;
    FDebugMicroTiles: TMicroTiles;
    FDebugInvalidRects: TRectList;
{$ENDIF}

    procedure DrawLayerToMicroTiles(var DstTiles: TMicroTiles; Layer: TCustomLayer);
    procedure DrawMeasuringHandler(Sender: TObject; const Area: TRect; const Info: Cardinal);

    procedure ValidateWorkingTiles;
    procedure UpdateOldInvalidTiles;
    procedure SetAdaptiveMode(const Value: Boolean);
    procedure ResetAdaptiveMode;
    procedure BeginAdaption;
    procedure EndAdaption;

    procedure AddArea(var Tiles: TMicroTiles; const Area: TRect; const Info: Cardinal);
  protected
    procedure SetEnabled(const Value: Boolean); override;

    // LayerCollection handler
    procedure LayerCollectionNotifyHandler(Sender: TLayerCollection;
      Action: TLayerListNotification; Layer: TCustomLayer; Index: Integer); override;
  public
    constructor Create(Buffer: TBitmap32; InvalidRects: TRectList); override;
    destructor Destroy; override;

    procedure RegisterLayerCollection(Layers: TLayerCollection); override;
    procedure UnregisterLayerCollection(Layers: TLayerCollection); override;

    procedure Reset; override;

    function  UpdatesAvailable: Boolean; override;
    procedure PerformOptimization; override;

    procedure BeginPaintBuffer; override;
    procedure EndPaintBuffer; override;

    // handlers
    procedure AreaUpdateHandler(Sender: TObject; const Area: TRect; const Info: Cardinal); override;
    procedure LayerUpdateHandler(Sender: TObject; Layer: TCustomLayer); override;
    procedure BufferResizedHandler(const NewWidth, NewHeight: Integer); override;

    // custom settings:
    property AdaptiveMode: Boolean read FAdaptiveMode write SetAdaptiveMode;
  end;


// only for direct debugging - will be removed on final:
var
  MicroTilesU: procedure(var DstTiles: TMicroTiles; const SrcTiles: TMicroTiles);

procedure _MicroTileUnion(var DstTile: TMicroTile; const SrcTile: TMicroTile);
procedure M_MicroTileUnion(var DstTile: TMicroTile; const SrcTile: TMicroTile);
procedure _MicroTilesUnion(var DstTiles: TMicroTiles; const SrcTiles: TMicroTiles);
procedure M_MicroTilesUnion(var DstTiles: TMicroTiles; const SrcTiles: TMicroTiles);

implementation

uses
  GR32_LowLevel, GR32_Math, Math;

{ MicroTile auxiliary routines }

function MakeMicroTile(const Left, Top, Right, Bottom: Integer): TMicroTile;
begin
  Result := Left shl 24 or Top shl 16 or Right shl 8 or Bottom;
end;

function MicroTileHeight(const Tile: TMicroTile): Integer;
begin
  Result := (Tile and $FF) - (Tile shr 16 and $FF);
end;

function MicroTileWidth(const Tile: TMicroTile): Integer;
begin
  Result := (Tile shr 8 and $FF) - (Tile shr 24);
end;

procedure _MicroTileUnion(var DstTile: TMicroTile; const SrcTile: TMicroTile);
var
  SrcLeft, SrcTop, SrcRight, SrcBottom: Integer;
begin
  SrcLeft := SrcTile shr 24;
  SrcTop := (SrcTile and $FF0000) shr 16;
  SrcRight := (SrcTile and $FF00) shr 8;
  SrcBottom := SrcTile and $FF;

  if (DstTile <> MICROTILE_FULL) and (SrcTile <> MICROTILE_EMPTY) and
     (SrcRight - SrcLeft <> 0) and (SrcBottom - SrcTop <> 0) then
  begin
    if (DstTile = MICROTILE_EMPTY) or (SrcTile = MICROTILE_FULL) then
      DstTile := SrcTile
    else
    begin
      DstTile := Min(DstTile shr 24, SrcLeft) shl 24 or
                 Min(DstTile shr 16 and $FF, SrcTop) shl 16 or
                 Max(DstTile shr 8 and $FF, SrcRight) shl 8 or
                 Max(DstTile and $FF, SrcBottom);
    end;
  end;
end;

procedure M_MicroTileUnion(var DstTile: TMicroTile; const SrcTile: TMicroTile);
var
  SrcLeft, SrcTop, SrcRight, SrcBottom: Integer;
begin
  SrcLeft := SrcTile shr 24;
  SrcTop := (SrcTile and $FF0000) shr 16;
  SrcRight := (SrcTile and $FF00) shr 8;
  SrcBottom := SrcTile and $FF;

  if (DstTile <> MICROTILE_FULL) and (SrcTile <> MICROTILE_EMPTY) and
     (SrcRight - SrcLeft <> 0) and (SrcBottom - SrcTop <> 0) then
  begin
    if (DstTile = MICROTILE_EMPTY) or (SrcTile = MICROTILE_FULL) then
      DstTile := SrcTile
    else
    asm
      {$IFDEF COMPILER6}
      MOVD   MM1,[SrcTile]
      {$ELSE}
      MOV    EAX,[SrcTile]
      db $0F,$6E,$C8           /// MOVD   MM1,EAX
      {$ENDIF}

      MOV    EAX,[DstTile]
      db $0F,$6E,$10           /// MOVD   MM2, [EAX]

      db $0F,$6F,$D9           /// MOVQ   MM3, MM1

      MOV    ECX,$FFFF0000   // Mask
      db $0F,$6E,$C1           /// MOVD   MM0, ECX
      db $0F,$DA,$CA           /// PMINUB MM1, MM2
      db $0F,$DB,$C8           /// PAND   MM1, MM0

      db $0F,$72,$D0,$10       /// PSRLD  MM0, 16         // shift mask right by 16 bits
      db $0F,$DE,$D3           /// PMAXUB MM2, MM3
      db $0F,$DB,$D0           /// PAND   MM2, MM0

      db $0F,$EB,$CA           /// POR    MM1, MM2

      db $0F,$7E,$08           /// MOVD   [EAX], MM1

      db $0F,$77               /// EMMS
    end;
  end;
end;

{ MicroTiles auxiliary routines }

function MakeEmptyMicroTiles: TMicroTiles;
begin
  FillChar(Result, SizeOf(TMicroTiles), 0);
  ReallocMem(Result.Tiles, 0);
end;

procedure MicroTilesCreate(var MicroTiles: TMicroTiles);
begin
  FillChar(MicroTiles, SizeOf(TMicroTiles), 0);
  ReallocMem(MicroTiles.Tiles, 0);
end;

procedure MicroTilesDestroy(var MicroTiles: TMicroTiles);
begin
  ReallocMem(MicroTiles.Tiles, 0);
end;

procedure MicroTilesSetSize(var MicroTiles: TMicroTiles; const DstRect: TRect);
begin
  MicroTiles.BoundsRect := DstRect;
  MicroTiles.Columns := ((DstRect.Right - DstRect.Left) shr MICROTILE_SHIFT) + 1;
  MicroTiles.Rows := ((DstRect.Bottom - DstRect.Top) shr MICROTILE_SHIFT) + 1;

  MicroTiles.Count := (MicroTiles.Columns + 1) * (MicroTiles.Rows + 1);
  ReallocMem(MicroTiles.Tiles, MicroTiles.Count * SizeOf(TMicroTile));

  MicroTilesClear(MicroTiles)
end;

procedure MicroTilesClear(var MicroTiles: TMicroTiles);
begin
  MicroTiles.BoundsUsedTiles := MakeRect(MicroTiles.Columns, MicroTiles.Rows, 0, 0);
  FillLongword(MicroTiles.Tiles^[0], MicroTiles.Count, MICROTILE_EMPTY);
end;

procedure MicroTilesClearUsed(var MicroTiles: TMicroTiles);
var
  I: Integer;
begin
  for I := MicroTiles.BoundsUsedTiles.Top to MicroTiles.BoundsUsedTiles.Bottom do
    FillLongword(MicroTiles.Tiles^[I * MicroTiles.Columns + MicroTiles.BoundsUsedTiles.Left],
      MicroTiles.BoundsUsedTiles.Right - MicroTiles.BoundsUsedTiles.Left + 1, MICROTILE_EMPTY);

  MicroTiles.BoundsUsedTiles := MakeRect(MicroTiles.Columns, MicroTiles.Rows, 0, 0);
end;

procedure MicroTilesCopy(var DstTiles: TMicroTiles; SrcTiles: TMicroTiles);
var
  CurRow, Width: Integer;
  SrcTilePtr, DstTilePtr: PMicroTile;
begin
  if Assigned(DstTiles.Tiles) and (DstTiles.Count > 0) then
    MicroTilesClearUsed(DstTiles);

  DstTiles.BoundsRect := SrcTiles.BoundsRect;
  DstTiles.Columns := SrcTiles.Columns;
  DstTiles.Rows := SrcTiles.Rows;
  DstTiles.BoundsUsedTiles := SrcTiles.BoundsUsedTiles;

  ReallocMem(DstTiles.Tiles, SrcTiles.Count * SizeOf(TMicroTile));

  if DstTiles.Count < SrcTiles.Count then
    FillLongword(DstTiles.Tiles^[DstTiles.Count], SrcTiles.Count - DstTiles.Count, MICROTILE_EMPTY);

  DstTiles.Count := SrcTiles.Count;

  SrcTilePtr := @SrcTiles.Tiles^[SrcTiles.BoundsUsedTiles.Top * SrcTiles.Columns + SrcTiles.BoundsUsedTiles.Left];
  DstTilePtr := @DstTiles.Tiles^[SrcTiles.BoundsUsedTiles.Top * DstTiles.Columns + SrcTiles.BoundsUsedTiles.Left];
  Width := SrcTiles.BoundsUsedTiles.Right - SrcTiles.BoundsUsedTiles.Left + 1;

  for CurRow := SrcTiles.BoundsUsedTiles.Top to SrcTiles.BoundsUsedTiles.Bottom do
  begin
    MoveLongword(SrcTilePtr^, DstTilePtr^, Width);
    Inc(DstTilePtr, DstTiles.Columns);
    Inc(SrcTilePtr, SrcTiles.Columns);
  end
end;

procedure MicroTilesAddLine(var MicroTiles: TMicroTiles; X1, Y1, X2, Y2: Integer; LineWidth: Integer; RoundToWholeTiles: Boolean = False);
var
  I: Integer;
  Dx, Dy: Integer;
  Sx, Sy: Integer;
  DeltaX, DeltaY: Integer;
  Rects: Integer;
  NewX, NewY: Integer;
  TempRect: TRect;
  Swapped: Boolean;
begin
  Dx := X2 - X1;
  Dy := Y2 - Y1;

  LineWidth := LineWidth shl 1;

  if Dx > 0 then
    Sx := 1
  else if Dx < 0 then
  begin
    Dx := -Dx;
    Sx := -1;
  end
  else // Dx = 0
  begin
    TempRect := MakeRect(X1, Y1, X2, Y2);
    InflateArea(TempRect, LineWidth, LineWidth);
    MicroTilesAddRect(MicroTiles, TempRect, RoundToWholeTiles);
    Exit;
  end;

  if Dy > 0 then
    Sy := 1
  else if Dy < 0 then
  begin
    Dy := -Dy;
    Sy := -1;
  end
  else // Dy = 0
  begin
    TempRect := MakeRect(X1, Y1, X2, Y2);
    InflateArea(TempRect, LineWidth, LineWidth);
    MicroTilesAddRect(MicroTiles, TempRect, RoundToWholeTiles);
    Exit;
  end;

  X1 := X1 * FixedOne;
  Y1 := Y1 * FixedOne;

  Dx := Dx * FixedOne;
  Dy := Dy * FixedOne;

  if Dx < Dy then
  begin
    Swapped := True;
    Swap(Dx, Dy);
  end
  else
    Swapped := False;

  Rects := Dx div MICROTILE_SIZE;

  DeltaX := MICROTILE_SIZE * FixedOne;
  DeltaY := FixedDiv(Dy, Rects);

  if Swapped then
    Swap(DeltaX, DeltaY);

  DeltaX := Sx * DeltaX;
  DeltaY := Sy * DeltaY;

  for I := 1 to FixedCeil(Rects) do
  begin
    NewX := X1 + DeltaX;
    NewY := Y1 + DeltaY;

    TempRect := MakeRect(FixedRect(X1, Y1, NewX, NewY));
    InflateArea(TempRect, LineWidth, LineWidth);
    MicroTilesAddRect(MicroTiles, TempRect, RoundToWholeTiles);

    X1 := NewX;
    Y1 := NewY;
  end;
end;

procedure MicroTilesAddRect(var MicroTiles: TMicroTiles; Rect: TRect; RoundToWholeTiles: Boolean);
var
  ModLeft, ModRight, ModTop, ModBottom, Temp: Integer;
  LeftTile, TopTile, RightTile, BottomTile, ColSpread, RowSpread: Integer;
  CurRow, CurCol: Integer;
  TilePtr, TilePtr2: PMicroTile;
begin
  if MicroTiles.Count = 0 then Exit;

  with Rect do
  begin
    TestSwap(Left, Right);
    TestSwap(Top, Bottom);

    if Left < 0 then Left := 0;
    if Top < 0 then Top := 0;
    Temp := MicroTiles.Columns shl MICROTILE_SHIFT;
    if Right > Temp then Right := Temp;
    Temp := MicroTiles.Rows shl MICROTILE_SHIFT;
    if Bottom > Temp then Bottom := Temp;
    
    if (Left > Right) or (Top > Bottom) then Exit;
  end;

  LeftTile := Rect.Left shr MICROTILE_SHIFT;
  TopTile := Rect.Top shr MICROTILE_SHIFT;
  RightTile := Rect.Right shr MICROTILE_SHIFT;
  BottomTile := Rect.Bottom shr MICROTILE_SHIFT;

  TilePtr := @MicroTiles.Tiles^[TopTile * MicroTiles.Columns + LeftTile];

  if RoundToWholeTiles then
  begin
    for CurRow := TopTile to BottomTile do
    begin
      FillLongword(TilePtr^, RightTile - LeftTile + 1, MICROTILE_FULL);
      Inc(TilePtr, MicroTiles.Columns);
    end;
  end
  else
  begin
    // calculate number of tiles needed in columns and rows
    ColSpread := ((Rect.Right + MICROTILE_SIZE) shr MICROTILE_SHIFT) -
             (Rect.Left shr MICROTILE_SHIFT);
    RowSpread := ((Rect.Bottom + MICROTILE_SIZE) shr MICROTILE_SHIFT) -
              (Rect.Top shr MICROTILE_SHIFT);

    ModLeft := Rect.Left mod MICROTILE_SIZE;
    ModTop := Rect.Top mod MICROTILE_SIZE;
    ModRight := Rect.Right mod MICROTILE_SIZE;
    ModBottom := Rect.Bottom mod MICROTILE_SIZE;

    if (ColSpread = 1) and (RowSpread = 1) then
      MicroTileUnion(TilePtr^, MakeMicroTile(ModLeft, ModTop, ModRight, ModBottom))
    else if ColSpread = 1 then
    begin
      MicroTileUnion(TilePtr^, MakeMicroTile(ModLeft, ModTop, ModRight, MICROTILE_SIZE));
      Inc(TilePtr, MicroTiles.Columns);

      if RowSpread > 2 then
        for CurCol := TopTile + 1 to BottomTile - 1 do
        begin
          MicroTileUnion(TilePtr^, MakeMicroTile(ModLeft, 0, ModRight, MICROTILE_SIZE));
          Inc(TilePtr, MicroTiles.Columns);
        end;

      MicroTileUnion(TilePtr^, MakeMicroTile(ModLeft, 0, ModRight, ModBottom));
    end
    else if RowSpread = 1 then
    begin
      MicroTileUnion(TilePtr^, MakeMicroTile(ModLeft, ModTop, MICROTILE_SIZE, ModBottom));
      Inc(TilePtr);

      if ColSpread > 2 then
        for CurRow := LeftTile + 1 to RightTile - 1 do
        begin
          MicroTileUnion(TilePtr^, MakeMicroTile(0, ModTop, MICROTILE_SIZE, ModBottom));
          Inc(TilePtr);
        end;

      MicroTileUnion(TilePtr^, MakeMicroTile(0, ModTop, ModRight, ModBottom));
    end
    else
    begin
      TilePtr2 := TilePtr;

      // TOP:
      // render top-left corner
      MicroTileUnion(TilePtr2^, MakeMicroTile(ModLeft, ModTop, MICROTILE_SIZE, MICROTILE_SIZE));
      Inc(TilePtr2);

      // render top edge
      if ColSpread > 2 then
        for CurRow := LeftTile + 1 to RightTile - 1 do
        begin
          MicroTileUnion(TilePtr2^, MakeMicroTile(0, ModTop, MICROTILE_SIZE, MICROTILE_SIZE));
          Inc(TilePtr2);
        end;

      // render top-right corner
      MicroTileUnion(TilePtr2^, MakeMicroTile(0, ModTop, ModRight, MICROTILE_SIZE));

      Inc(TilePtr, MicroTiles.Columns);

      // INTERMEDIATE AREA:
      if RowSpread > 2 then
        for CurCol := TopTile + 1 to BottomTile - 1 do
        begin
          TilePtr2 := TilePtr;

          // render left edge
          MicroTileUnion(TilePtr2^, MakeMicroTile(ModLeft, 0, MICROTILE_SIZE, MICROTILE_SIZE));
          Inc(TilePtr2);

          // render content
          if ColSpread > 2 then
          begin
            FillLongword(TilePtr2^, RightTile - LeftTile - 1, MICROTILE_FULL);
            Inc(TilePtr2, RightTile - LeftTile - 1);
          end;

          // render right edge
          MicroTileUnion(TilePtr2^, MakeMicroTile(0, 0, ModRight, MICROTILE_SIZE));

          Inc(TilePtr, MicroTiles.Columns);
        end;

      TilePtr2 := TilePtr;

      // BOTTOM:
      // render bottom-left corner
      MicroTileUnion(TilePtr2^, MakeMicroTile(ModLeft, 0, MICROTILE_SIZE, ModBottom));
      Inc(TilePtr2);

      // render bottom edge
      if ColSpread > 2 then
        for CurRow := LeftTile + 1 to RightTile - 1 do
        begin
          MicroTileUnion(TilePtr2^, MakeMicroTile(0, 0, MICROTILE_SIZE, ModBottom));
          Inc(TilePtr2);
        end;

      // render bottom-right corner
      MicroTileUnion(TilePtr2^, MakeMicroTile(0, 0, ModRight, ModBottom));
    end;
  end;

  with MicroTiles.BoundsUsedTiles do
  begin
    if LeftTile < Left then Left := LeftTile;
    if TopTile < Top then Top := TopTile;
    if RightTile > Right then Right := RightTile;
    if BottomTile > Bottom then Bottom := BottomTile;
  end;
end;


procedure _MicroTilesUnion(var DstTiles: TMicroTiles; const SrcTiles: TMicroTiles);
var
  SrcTilePtr, DstTilePtr: PMicroTile;
  SrcTilePtr2, DstTilePtr2: PMicroTile;
  X, Y: Integer;
  SrcLeft, SrcTop, SrcRight, SrcBottom: Integer;
  SrcTile: TMicroTile;
begin
  SrcTilePtr := @SrcTiles.Tiles^[SrcTiles.BoundsUsedTiles.Top * SrcTiles.Columns + SrcTiles.BoundsUsedTiles.Left];
  DstTilePtr := @DstTiles.Tiles^[SrcTiles.BoundsUsedTiles.Top * DstTiles.Columns + SrcTiles.BoundsUsedTiles.Left];

  for Y := SrcTiles.BoundsUsedTiles.Top to SrcTiles.BoundsUsedTiles.Bottom do
  begin
    SrcTilePtr2 := SrcTilePtr;
    DstTilePtr2 := DstTilePtr;
    for X := SrcTiles.BoundsUsedTiles.Left to SrcTiles.BoundsUsedTiles.Right do
    begin
      SrcTile := SrcTilePtr2^;
      SrcLeft := SrcTile shr 24;
      SrcTop := (SrcTile and $FF0000) shr 16;
      SrcRight := (SrcTile and $FF00) shr 8;
      SrcBottom := SrcTile and $FF;

      if (DstTilePtr2^ <> MICROTILE_FULL) and (SrcTilePtr2^ <> MICROTILE_EMPTY) and
         (SrcRight - SrcLeft <> 0) and (SrcBottom - SrcTop <> 0) then
      begin
        if (DstTilePtr2^ = MICROTILE_EMPTY) or (SrcTilePtr2^ = MICROTILE_FULL) then
          DstTilePtr2^ := SrcTilePtr2^
        else
          DstTilePtr2^ := Min(DstTilePtr2^ shr 24, SrcLeft) shl 24 or
                          Min(DstTilePtr2^ shr 16 and $FF, SrcTop) shl 16 or
                          Max(DstTilePtr2^ shr 8 and $FF, SrcRight) shl 8 or
                          Max(DstTilePtr2^ and $FF, SrcBottom);
      end;

      Inc(DstTilePtr2);
      Inc(SrcTilePtr2);
    end;
    Inc(DstTilePtr, DstTiles.Columns);
    Inc(SrcTilePtr, SrcTiles.Columns);
  end;
end;

procedure M_MicroTilesUnion(var DstTiles: TMicroTiles; const SrcTiles: TMicroTiles);
var
  SrcTilePtr, DstTilePtr: PMicroTile;
  SrcTilePtr2, DstTilePtr2: PMicroTile;
  X, Y: Integer;
  SrcLeft, SrcTop, SrcRight, SrcBottom: Integer;
begin
  SrcTilePtr := @SrcTiles.Tiles^[SrcTiles.BoundsUsedTiles.Top * SrcTiles.Columns + SrcTiles.BoundsUsedTiles.Left];
  DstTilePtr := @DstTiles.Tiles^[SrcTiles.BoundsUsedTiles.Top * DstTiles.Columns + SrcTiles.BoundsUsedTiles.Left];

  asm
    MOV    ECX, $FFFF  // Mask
    db $0F,$6E,$C1           /// MOVD   MM0, ECX
    db $0F,$6F,$E0           /// MOVQ   MM4, MM0
    db $0F,$72,$F4,$10       /// PSLLD  MM4, 16     // shift mask left by 16 bits
  end;

  for Y := SrcTiles.BoundsUsedTiles.Top to SrcTiles.BoundsUsedTiles.Bottom do
  begin
    SrcTilePtr2 := SrcTilePtr;
    DstTilePtr2 := DstTilePtr;
    for X := SrcTiles.BoundsUsedTiles.Left to SrcTiles.BoundsUsedTiles.Right do
    begin
      SrcLeft := SrcTilePtr2^ shr 24;
      SrcTop := (SrcTilePtr2^ and $FF0000) shr 16;
      SrcRight := (SrcTilePtr2^ and $FF00) shr 8;
      SrcBottom := SrcTilePtr2^ and $FF;

      if (DstTilePtr2^ <> MICROTILE_FULL) and (SrcTilePtr2^ <> MICROTILE_EMPTY) and
         (SrcRight - SrcLeft <> 0) and (SrcBottom - SrcTop <> 0) then
      begin
        if (DstTilePtr2^ = MICROTILE_EMPTY) or (SrcTilePtr2^ = MICROTILE_FULL) then
          DstTilePtr2^ := SrcTilePtr2^
        else
        asm
          MOV    EAX, [DstTilePtr2]
          db $0F,$6E,$10           /// MOVD   MM2, [EAX]

          MOV    ECX, [SrcTilePtr2]
          db $0F,$6E,$09           /// MOVD   MM1, [ECX]
          db $0F,$6F,$D9           /// MOVQ   MM3, MM1

          db $0F,$DA,$CA           /// PMINUB MM1, MM2
          db $0F,$DB,$CC           /// PAND   MM1, MM4

          db $0F,$DE,$D3           /// PMAXUB MM2, MM3
          db $0F,$DB,$D0           /// PAND   MM2, MM0

          db $0F,$EB,$CA           /// POR    MM1, MM2

          db $0F,$7E,$08           /// MOVD   [EAX], MM1
        end;
      end;

      Inc(DstTilePtr2);
      Inc(SrcTilePtr2);
    end;
    Inc(DstTilePtr, DstTiles.Columns);
    Inc(SrcTilePtr, SrcTiles.Columns);
  end;

  asm
    db $0F,$77               /// EMMS
  end;
end;

procedure MicroTilesUnion(var DstTiles: TMicroTiles; const SrcTiles: TMicroTiles; RoundToWholeTiles: Boolean);
var
  SrcTilePtr, DstTilePtr: PMicroTile;
  SrcTilePtr2, DstTilePtr2: PMicroTile;
  X, Y: Integer;
  SrcLeft, SrcTop, SrcRight, SrcBottom: Integer;
begin
  if SrcTiles.Count = 0 then Exit;

  if RoundToWholeTiles then
  begin
    SrcTilePtr := @SrcTiles.Tiles^[SrcTiles.BoundsUsedTiles.Top * SrcTiles.Columns + SrcTiles.BoundsUsedTiles.Left];
    DstTilePtr := @DstTiles.Tiles^[SrcTiles.BoundsUsedTiles.Top * DstTiles.Columns + SrcTiles.BoundsUsedTiles.Left];

    for Y := SrcTiles.BoundsUsedTiles.Top to SrcTiles.BoundsUsedTiles.Bottom do
    begin
      SrcTilePtr2 := SrcTilePtr;
      DstTilePtr2 := DstTilePtr;
      for X := SrcTiles.BoundsUsedTiles.Left to SrcTiles.BoundsUsedTiles.Right do
      begin
        SrcLeft := SrcTilePtr2^ shr 24;
        SrcTop := (SrcTilePtr2^ and $FF0000) shr 16;
        SrcRight := (SrcTilePtr2^ and $FF00) shr 8;
        SrcBottom := SrcTilePtr2^ and $FF;

        if (DstTilePtr2^ <> MICROTILE_FULL) and (SrcTilePtr2^ <> MICROTILE_EMPTY) and
           (SrcRight - SrcLeft <> 0) and (SrcBottom - SrcTop <> 0) then
          DstTilePtr2^ := MICROTILE_FULL;

        Inc(DstTilePtr2);
        Inc(SrcTilePtr2);
      end;
      Inc(DstTilePtr, DstTiles.Columns);
      Inc(SrcTilePtr, SrcTiles.Columns);
    end
  end
  else
    MicroTilesU(DstTiles, SrcTiles);

  with DstTiles.BoundsUsedTiles do
  begin
    if SrcTiles.BoundsUsedTiles.Left < Left then Left := SrcTiles.BoundsUsedTiles.Left;
    if SrcTiles.BoundsUsedTiles.Top < Top then Top := SrcTiles.BoundsUsedTiles.Top;
    if SrcTiles.BoundsUsedTiles.Right > Right then Right := SrcTiles.BoundsUsedTiles.Right;
    if SrcTiles.BoundsUsedTiles.Bottom > Bottom then Bottom := SrcTiles.BoundsUsedTiles.Bottom;
  end;
end;

function MicroTilesCalcRects(const MicroTiles: TMicroTiles; DstRects: TRectList;
  CountOnly, RoundToWholeTiles: Boolean): Integer;
begin
  Result := MicroTilesCalcRects(MicroTiles, DstRects, MicroTiles.BoundsRect, CountOnly);
end;


function MicroTilesCalcRects(const MicroTiles: TMicroTiles; DstRects: TRectList;
  const Clip: TRect; CountOnly, RoundToWholeTiles: Boolean): Integer;
var
  Rects: Array Of TRect;
  Rect: PRect;
  CombLUT: Array Of Integer;
  StartIndex: Integer;
  CurTile, TempTile: TMicroTile;
  Temp: Integer;
  NewLeft, NewTop, NewRight, NewBottom: Integer;
  CurCol, CurRow, I, RectsCount: Integer;
begin
  Result := 0;

  if (MicroTiles.Count = 0) or
     (MicroTiles.BoundsUsedTiles.Right - MicroTiles.BoundsUsedTiles.Left < 0) or
     (MicroTiles.BoundsUsedTiles.Bottom - MicroTiles.BoundsUsedTiles.Top < 0) then Exit;

  SetLength(Rects, MicroTiles.Columns * MicroTiles.Rows);
  SetLength(CombLUT, MicroTiles.Columns * MicroTiles.Rows);
  FillLongword(CombLUT[0], Length(CombLUT), Cardinal(-1));

  I := 0;
  RectsCount := 0;

  if not RoundToWholeTiles then
    for CurRow := 0 to MicroTiles.Rows - 1 do
    begin
      CurCol := 0;
      while CurCol < MicroTiles.Columns do
      begin
        CurTile := MicroTiles.Tiles[I];

        if CurTile <> MICROTILE_EMPTY then
        begin
          Temp := CurRow shl MICROTILE_SHIFT;
          NewTop := Constrain(Temp + CurTile shr 16 and $FF, Clip.Top, Clip.Bottom);
          NewBottom := Constrain(Temp + CurTile and $FF, Clip.Top, Clip.Bottom);
          NewLeft := Constrain(CurCol shl MICROTILE_SHIFT + CurTile shr 24, Clip.Left, Clip.Right);

          StartIndex := I;

          if (CurTile shr 8 and $FF = MICROTILE_SIZE) and (CurCol <> MicroTiles.Columns - 1) then
          begin
            while True do
            begin
              Inc(CurCol);
              Inc(I);

              TempTile := MicroTiles.Tiles[I];
              if (CurCol = MicroTiles.Columns) or
                 (TempTile shr 16 and $FF <> CurTile shr 16 and $FF) or
                 (TempTile and $FF <> CurTile and $FF) or
                 (TempTile shr 24 <> 0) then
              begin
                Dec(CurCol);
                Dec(I);
                Break;
              end;
            end;
          end;

          NewRight := Constrain(CurCol shl MICROTILE_SHIFT + MicroTiles.Tiles[I] shr 8 and $FF, Clip.Left, Clip.Right);

          Temp := CombLUT[StartIndex];

          Rect := nil;
          if Temp <> -1 then Rect := @Rects[Temp];

          if Assigned(Rect) and
             (Rect.Left = NewLeft) and
             (Rect.Right = NewRight) and
             (Rect.Bottom = NewTop) then
          begin
            Rect.Bottom := NewBottom;

            if CurRow <> MicroTiles.Rows - 1 then
              CombLUT[StartIndex + MicroTiles.Columns] := Temp;
          end
          else
            with Rects[RectsCount] do
            begin
              Left := NewLeft;    Top := NewTop;
              Right := NewRight;  Bottom := NewBottom;

              if CurRow <> MicroTiles.Rows - 1 then
                CombLUT[StartIndex + MicroTiles.Columns] := RectsCount;

              Inc(RectsCount);
            end;
        end;

        Inc(I);
        Inc(CurCol);
      end;
    end
  else
    for CurRow := 0 to MicroTiles.Rows - 1 do
    begin
      CurCol := 0;
      while CurCol < MicroTiles.Columns do
      begin
        CurTile := MicroTiles.Tiles[I];

        if CurTile <> MICROTILE_EMPTY then
        begin
          Temp := CurRow shl MICROTILE_SHIFT;
          NewTop := Constrain(Temp, Clip.Top, Clip.Bottom);
          NewBottom := Constrain(Temp + MICROTILE_SIZE, Clip.Top, Clip.Bottom);
          NewLeft := Constrain(CurCol shl MICROTILE_SHIFT, Clip.Left, Clip.Right);

          StartIndex := I;

          if CurCol <> MicroTiles.Columns - 1 then
          begin
            while True do
            begin
              Inc(CurCol);
              Inc(I);

              TempTile := MicroTiles.Tiles[I];
              if (CurCol = MicroTiles.Columns) or (TempTile = MICROTILE_EMPTY) then
              begin
                Dec(CurCol);
                Dec(I);
                Break;
              end;
            end;
          end;

          NewRight := Constrain(CurCol shl MICROTILE_SHIFT + MICROTILE_SIZE, Clip.Left, Clip.Right);

          Temp := CombLUT[StartIndex];

          Rect := nil;
          if Temp <> -1 then Rect := @Rects[Temp];

          if Assigned(Rect) and
             (Rect.Left = NewLeft) and
             (Rect.Right = NewRight) and
             (Rect.Bottom = NewTop) then
          begin
            Rect.Bottom := NewBottom;

            if CurRow <> MicroTiles.Rows - 1 then
              CombLUT[StartIndex + MicroTiles.Columns] := Temp;
          end
          else
            with Rects[RectsCount] do
            begin
              Left := NewLeft;    Top := NewTop;
              Right := NewRight;  Bottom := NewBottom;

              if CurRow <> MicroTiles.Rows - 1 then
                CombLUT[StartIndex + MicroTiles.Columns] := RectsCount;

              Inc(RectsCount);
            end;
        end;

        Inc(I);
        Inc(CurCol);
      end;
    end;


  Result := RectsCount;

  if not CountOnly then
    for I := 0 to RectsCount - 1 do DstRects.Add(Rects[I]);
end;

function MicroTilesCountEmptyTiles(const MicroTiles: TMicroTiles): Integer;
var
  CurRow, CurCol: Integer;
  TilePtr: PMicroTile;
begin
  Result := 0;
  if MicroTiles.Count > 0 then
  begin
    TilePtr := @MicroTiles.Tiles^[0];
    for CurRow := 0 to MicroTiles.Rows - 1 do
      for CurCol := 0 to MicroTiles.Columns - 1 do
      begin
        if TilePtr^ = MICROTILE_EMPTY then Inc(Result);
        Inc(TilePtr);
      end;
  end;
end;

{$IFDEF MICROTILES_DEBUGDRAW}
procedure MicroTilesDebugDraw(const MicroTiles: TMicroTiles; DstBitmap: TBitmap32; DrawOptimized, RoundToWholeTiles: Boolean);
var
  I: Integer;
  TempRect: TRect;
  Rects: TRectList;
begin
  if DrawOptimized then
  begin
    Rects := TRectList.Create;
    MicroTilesCalcRects(MicroTiles, Rects, False, RoundToWholeTiles);
    try
      if Rects.Count > 0 then
      begin
        for I := 0 to Rects.Count - 1 do
        begin
          DstBitmap.FillRectTS(Rects[I]^, $20FF0000);
          DstBitmap.FrameRectTS(Rects[I]^, $90FF0000);
        end;
      end
    finally
      Rects.Free;
    end;
  end
  else
    for I := 0 to MicroTiles.Count - 1 do
    begin
      if MicroTiles.Tiles^[i] <> MICROTILE_EMPTY then
      begin
        TempRect.Left := ((I mod MicroTiles.Columns) shl MICROTILE_SHIFT) + (MicroTiles.Tiles[i] shr 24);
        TempRect.Top := ((I div MicroTiles.Columns) shl MICROTILE_SHIFT) + (MicroTiles.Tiles[i] shr 16 and $FF);
        TempRect.Right := ((I mod MicroTiles.Columns) shl MICROTILE_SHIFT) + (MicroTiles.Tiles[i] shr 8 and $FF);
        TempRect.Bottom := ((I div MicroTiles.Columns) shl MICROTILE_SHIFT) + (MicroTiles.Tiles[i] and $FF);

        DstBitmap.FillRectTS(TempRect, $20FF0000);
        DstBitmap.FrameRectTS(TempRect, $90FF0000);
      end;
    end;
end;
{$ENDIF}

{ TMicroTilesMap }

function TMicroTilesMap.Add(Item: Pointer): PPMicroTiles;
var
  TilesPtr: PMicroTiles;
  IsNew: Boolean;
begin
  Result := PPMicroTiles(inherited Add(Item, IsNew));
  if IsNew then
  begin
    New(TilesPtr);
    MicroTilesCreate(TilesPtr^);
    Result^ := TilesPtr;
  end;
end;

function TMicroTilesMap.Delete(BucketIndex, ItemIndex: Integer): Pointer;
var
  TilesPtr: PMicroTiles;
begin
  TilesPtr := inherited Delete(BucketIndex, ItemIndex);
  MicroTilesDestroy(TilesPtr^);
  Dispose(TilesPtr);
  Result := nil;
end;

procedure TMicroTilesMap.SetData(Item: Pointer; const Data: PMicroTiles);
begin
  inherited SetData(Item, Data);
end;

function TMicroTilesMap.GetData(Item: Pointer): PMicroTiles;
begin
  Result := inherited GetData(Item);
end;



{ TMicroTilesRepaintManager }

type
  TLayerCollectionAccess = class(TLayerCollection);
  TCustomLayerAccess = class(TCustomLayer);

const
  PL_MICROTILES         = 0;
  PL_WHOLETILES         = 1;
  PL_FULLSCENE          = 2;

  TIMER_PENALTY         = 250;
  TIMER_LOWLIMIT        = 1000;
  TIMER_HIGHLIMIT       = 5000;

  INVALIDRECTS_DELTA    = 10;

constructor TMicroTilesRepaintOptimizer.Create(Buffer: TBitmap32; InvalidRects: TRectList);
begin
  inherited;
  FOldInvalidTilesMap := TMicroTilesMap.Create;
  FInvalidLayers := TList.Create;
  FPerfTimer := TPerfTimer.Create;
{$IFNDEF MICROTILES_DEBUGDRAW}
  {$IFNDEF MICROTILES_NO_ADAPTION}
  FAdaptiveMode := True;
  {$ENDIF}
{$ENDIF}

  MicroTilesCreate(FInvalidTiles);
  MicroTilesCreate(FTempTiles);
  MicroTilesCreate(FForcedInvalidTiles);

{$IFDEF MICROTILES_DEBUGDRAW}
  MicroTilesCreate(FDebugMicroTiles);
  FDebugInvalidRects := TRectList.Create;
{$ENDIF}
end;

destructor TMicroTilesRepaintOptimizer.Destroy;
begin
  MicroTilesDestroy(FForcedInvalidTiles);
  MicroTilesDestroy(FTempTiles);
  MicroTilesDestroy(FInvalidTiles);

  FPerfTimer.Free;
  FInvalidLayers.Free;
  FOldInvalidTilesMap.Free;

{$IFDEF MICROTILES_DEBUGDRAW}
  FDebugInvalidRects.Free;
  MicroTilesDestroy(FDebugMicroTiles);
{$ENDIF}

  inherited;
end;

procedure TMicroTilesRepaintOptimizer.AreaUpdateHandler(Sender: TObject; const Area: TRect;
  const Info: Cardinal);
begin
  ValidateWorkingTiles;
  AddArea(FForcedInvalidTiles, Area, Info);
  FUseInvalidTiles := True;
end;

procedure TMicroTilesRepaintOptimizer.AddArea(var Tiles: TMicroTiles; const Area: TRect;
  const Info: Cardinal);
var
  LineWidth: Integer;
  TempRect: TRect;
begin
  if Info and AREAINFO_LINE <> 0 then
  begin
    LineWidth := Info and $00FFFFFF;
    TempRect := Area;
    InflateArea(TempRect, LineWidth, LineWidth);
    with TempRect do
      MicroTilesAddLine(Tiles, Left, Top, Right, Bottom, LineWidth, FPerformanceLevel > PL_MICROTILES);
  end
  else
    MicroTilesAddRect(Tiles, Area, FPerformanceLevel > PL_MICROTILES);
end;

procedure TMicroTilesRepaintOptimizer.LayerUpdateHandler(Sender: TObject; Layer: TCustomLayer);
begin
  if FOldInvalidTilesValid and not TCustomLayerAccess(Layer).Invalid then
  begin
    FInvalidLayers.Add(Layer);
    TCustomLayerAccess(Layer).Invalid := True;
    FUseInvalidTiles := True;
  end;
end;

procedure TMicroTilesRepaintOptimizer.LayerCollectionNotifyHandler(Sender: TLayerCollection;
  Action: TLayerListNotification; Layer: TCustomLayer; Index: Integer);
var
  TilesPtr: PMicroTiles;
begin
  case Action of
    lnLayerAdded, lnLayerInserted:
      begin
        TilesPtr := FOldInvalidTilesMap.Add(Layer)^;
        MicroTilesSetSize(TilesPtr^, Buffer.BoundsRect);
        FOldInvalidTilesValid := True;
      end;

    lnLayerDeleted:
      begin
        if FOldInvalidTilesValid then
        begin
          // force repaint of tiles that the layer did previously allocate
          MicroTilesUnion(FInvalidTiles, FOldInvalidTilesMap[Layer]^);
          FUseInvalidTiles := True;
        end;
        FInvalidLayers.Remove(Layer);
        FOldInvalidTilesMap.Remove(Layer);
      end;

    lnCleared:
      begin
        if FOldInvalidTilesValid then
        begin
          with TPointerMapIterator.Create(FOldInvalidTilesMap) do
          try
            while Next do
              MicroTilesUnion(FInvalidTiles, PMicroTiles(Data)^);
          finally
            Free;
          end;

          FUseInvalidTiles := True;
          ResetAdaptiveMode;
        end;
        FOldInvalidTilesMap.Clear;
        FOldInvalidTilesValid := True;
      end;
  end;
end;

procedure TMicroTilesRepaintOptimizer.ValidateWorkingTiles;
var
  BoundsRect: TRect;
begin
  if not FWorkingTilesValid then  // check if working microtiles need resize...
  begin
    BoundsRect := Buffer.BoundsRect; // save into local var since TBitmap32.BoundsRect is a method...
    MicroTilesSetSize(FTempTiles, BoundsRect);
    MicroTilesSetSize(FInvalidTiles, BoundsRect);
    MicroTilesSetSize(FForcedInvalidTiles, BoundsRect);
    FWorkingTilesValid := True;
  end;
end;

procedure TMicroTilesRepaintOptimizer.UpdateOldInvalidTiles;
var
  I, J: Integer;
  BoundsRect: TRect;
  TilesPtr: PMicroTiles;
  Layer: TCustomLayer;
begin
  If not FOldInvalidTilesValid then  // check if old Invalid tiles need resize and rerendering...
  begin
    ValidateWorkingTiles;

    BoundsRect := Buffer.BoundsRect; // save into local var since TBitmap32.BoundsRect is a method...

    for I := 0 to LayerCollections.Count - 1 do
    with TLayerCollection(LayerCollections[I]) do
      for J := 0 to Count - 1 do
      begin
        Layer := Items[J];
        TilesPtr := FOldInvalidTilesMap.Add(Layer)^;
        MicroTilesSetSize(TilesPtr^, BoundsRect);
        DrawLayerToMicroTiles(TilesPtr^, Layer);
        TCustomLayerAccess(Layer).Invalid := False;
      end;

    FOldInvalidTilesValid := True;
    FUseInvalidTiles := False;
  end;
end;

function TMicroTilesRepaintOptimizer.UpdatesAvailable: Boolean;
begin
  UpdateOldInvalidTiles;
  Result := FUseInvalidTiles;
end;

procedure TMicroTilesRepaintOptimizer.Reset;
var
  I: Integer;
begin
  // Unmark Invalid layers
  for I := 0 to FInvalidLayers.Count - 1 do
    TCustomLayerAccess(FInvalidLayers[I]).Invalid := False;

  FInvalidLayers.Clear;
  FUseInvalidTiles := False;
  FOldInvalidTilesValid := False;
end;

procedure TMicroTilesRepaintOptimizer.BufferResizedHandler(const NewWidth, NewHeight: Integer);
begin
  FWorkingTilesValid := False;  // force resizing of working microtiles
  FOldInvalidTilesValid := False; // force resizing of old Invalid tiles -> force complete repaint
end;

procedure TMicroTilesRepaintOptimizer.RegisterLayerCollection(Layers: TLayerCollection);
begin
  inherited;

  if Enabled then
    with TLayerCollectionAccess(Layers) do
    begin
      OnLayerUpdated := LayerUpdateHandler;
      OnAreaUpdated := AreaUpdateHandler;
      OnListNotify := LayerCollectionNotifyHandler;
    end;
end;

procedure TMicroTilesRepaintOptimizer.UnregisterLayerCollection(Layers: TLayerCollection);
begin
  with TLayerCollectionAccess(Layers) do
  begin
    OnLayerUpdated := nil;
    OnAreaUpdated := nil;
    OnListNotify := nil;
  end;

  inherited;
end;

procedure TMicroTilesRepaintOptimizer.SetEnabled(const Value: Boolean);
var
  I: Integer;
begin
  if Value <> Enabled then
  begin
    if Value then
    begin
      // initialize:
      for I := 0 to LayerCollections.Count - 1 do
      with TLayerCollectionAccess(LayerCollections[I]) do
      begin
        OnLayerUpdated := LayerUpdateHandler;
        OnAreaUpdated := AreaUpdateHandler;
        OnListNotify := LayerCollectionNotifyHandler;
      end;

      FWorkingTilesValid := False;
      FOldInvalidTilesValid := False;
      UpdateOldInvalidTiles;
      FUseInvalidTiles := True; // force repaint of new layers on invalidation...
    end
    else
    begin
      // clean up:
      for I := 0 to LayerCollections.Count - 1 do
      with TLayerCollectionAccess(LayerCollections[I]) do
      begin
        OnLayerUpdated := nil;
        OnAreaUpdated := nil;
        OnListNotify := nil;
      end;

      MicroTilesDestroy(FInvalidTiles);
      MicroTilesDestroy(FTempTiles);
      MicroTilesDestroy(FForcedInvalidTiles);

      FUseInvalidTiles := False;
      FOldInvalidTilesValid := False;
      FOldInvalidTilesMap.Clear;
      FInvalidLayers.Clear;
    end;
    inherited;
  end;
end;

procedure TMicroTilesRepaintOptimizer.SetAdaptiveMode(const Value: Boolean);
begin
  if FAdaptiveMode <> Value then
  begin
    FAdaptiveMode := Value;
    ResetAdaptiveMode;
  end;
end;

procedure TMicroTilesRepaintOptimizer.ResetAdaptiveMode;
begin
  FTimeDelta := TIMER_LOWLIMIT;
  FAdaptionFailed := False;
  FPerformanceLevel := PL_MICROTILES;
end;

procedure TMicroTilesRepaintOptimizer.BeginPaintBuffer;
begin
  if AdaptiveMode then FPerfTimer.Start;
end;

procedure TMicroTilesRepaintOptimizer.EndPaintBuffer;
begin
  FUseInvalidTiles := False;

{$IFDEF MICROTILES_DEBUGDRAW}
  {$IFDEF MICROTILES_DEBUGDRAW_UNOPTIMIZED}
    MicroTilesDebugDraw(FDebugMicroTiles, Buffer, False, FDebugWholeTiles);
  {$ELSE}
    MicroTilesDebugDraw(FDebugMicroTiles, Buffer, True, FDebugWholeTiles);
  {$ENDIF}
{$ENDIF}

{$IFNDEF MICROTILES_NO_ADAPTION}
  EndAdaption;
{$ENDIF}
end;

procedure TMicroTilesRepaintOptimizer.DrawLayerToMicroTiles(var DstTiles: TMicroTiles; Layer: TCustomLayer);
begin
  Buffer.BeginMeasuring(DrawMeasuringHandler);
  FWorkMicroTiles := @DstTiles;
  TCustomLayerAccess(Layer).DoPaint(Buffer);
  Buffer.EndMeasuring;
end;

procedure TMicroTilesRepaintOptimizer.DrawMeasuringHandler(Sender: TObject; const Area: TRect;
  const Info: Cardinal);
begin
  AddArea(FWorkMicroTiles^, Area, Info);
end;

procedure TMicroTilesRepaintOptimizer.PerformOptimization;
var
  I: Integer;
  Layer: TCustomLayer;
  UseWholeTiles: Boolean;
  LayerTilesPtr: PMicroTiles;
begin
  if FUseInvalidTiles then
  begin
    ValidateWorkingTiles;
    // Determine if the use of whole tiles is better for current performance level
{$IFNDEF MICROTILES_NO_ADAPTION}
    UseWholeTiles := FPerformanceLevel > PL_MICROTILES;
{$ELSE}
  {$IFDEF MICROTILES_NO_ADAPTION_FORCE_WHOLETILES}
    UseWholeTiles := True;
  {$ELSE}
    UseWholeTiles := False;
  {$ENDIF}
{$ENDIF}

    if FInvalidLayers.Count > 0 then
    begin
      for I := 0 to FInvalidLayers.Count - 1 do
      begin
        Layer := FInvalidLayers[I];

        // Clear temporary tiles
        MicroTilesClearUsed(FTempTiles);
        // Draw layer to temporary tiles
        DrawLayerToMicroTiles(FTempTiles, Layer);

        // Combine temporary tiles with the global invalid tiles
        MicroTilesUnion(FInvalidTiles, FTempTiles, UseWholeTiles);

        // Retrieve old invalid tiles for the current layer
        LayerTilesPtr := FOldInvalidTilesMap[Layer];

        // Combine old invalid tiles with the global invalid tiles
        MicroTilesUnion(FInvalidTiles, LayerTilesPtr^, UseWholeTiles);

        // Copy temporary (current) invalid tiles to the layer
        MicroTilesCopy(LayerTilesPtr^, FTempTiles);

        // Unmark layer as invalid
        TCustomLayerAccess(Layer).Invalid := False;
      end;
      FInvalidLayers.Clear;
    end;

{$IFDEF MICROTILES_DEBUGDRAW}
    MicroTilesCalcRects(FInvalidTiles, InvalidRects, False, UseWholeTiles);
    MicroTilesCalcRects(FForcedInvalidTiles, InvalidRects, False, UseWholeTiles);
    MicroTilesCopy(FDebugMicroTiles, FInvalidTiles);
    MicroTilesUnion(FDebugMicroTiles, FForcedInvalidTiles);
    FDebugWholeTiles := UseWholeTiles;
{$ELSE}
    // Calculate optimized rectangles from global invalid tiles
    MicroTilesCalcRects(FInvalidTiles, InvalidRects, False, UseWholeTiles);
    // Calculate optimized rectangles from forced invalid tiles
    MicroTilesCalcRects(FForcedInvalidTiles, InvalidRects, False, UseWholeTiles);
{$ENDIF}
  end;

{$IFNDEF MICROTILES_NO_ADAPTION}
  BeginAdaption;
{$ENDIF}

{$IFDEF MICROTILES_DEBUGDRAW}
  if InvalidRects.Count > 0 then
  begin
    FDebugInvalidRects.Count := InvalidRects.Count;
    Move(InvalidRects[0]^, FDebugInvalidRects[0]^, InvalidRects.Count * SizeOf(TRect));
    InvalidRects.Clear;
  end;
{$ENDIF}

  // Rects have been created, so we don't need the tiles any longer, clear them.
  MicroTilesClearUsed(FInvalidTiles);
  MicroTilesClearUsed(FForcedInvalidTiles);
end;

procedure TMicroTilesRepaintOptimizer.BeginAdaption;
begin
  if AdaptiveMode and (FPerformanceLevel > PL_MICROTILES) then
  begin
    if Integer(GetTickCount) > FNextCheck then
    begin
      FPerformanceLevel := Constrain(FPerformanceLevel - 1, PL_MICROTILES, PL_FULLSCENE);
      {$IFDEF CODESITE}
      CodeSite.SendInteger('PrepareInvalidRects(Timed): FPerformanceLevel', FPerformanceLevel);
      {$ENDIF}
      FTimedCheck := True;
    end
    else if not FAdaptionFailed and (InvalidRects.Count < FOldInvalidRectsCount - INVALIDRECTS_DELTA) then
    begin
      FPerformanceLevel := Constrain(FPerformanceLevel - 1, PL_MICROTILES, PL_FULLSCENE);
      {$IFDEF CODESITE}
      CodeSite.SendInteger('PrepareInvalidRects: FPerformanceLevel', FPerformanceLevel);
      {$ENDIF}
    end
    else if FPerformanceLevel = PL_FULLSCENE then
      // we need a full scene rendition, so clear the invalid rects
      InvalidRects.Clear;
  end;
end;

procedure TMicroTilesRepaintOptimizer.EndAdaption;
var
  TimeNeeded: Int64;
  Level: Integer;
begin
  // our KISS(TM) repaint mode balancing starts here...
  TimeNeeded := FPerfTimer.ReadValue;

{$IFDEF MICROTILES_DEBUGDRAW}
  if FDebugInvalidRects.Count = 0 then
{$ELSE}
  if InvalidRects.Count = 0 then
{$ENDIF}
    FTimeNeededForFullSceneRepaint := TimeNeeded
  else if AdaptiveMode then
  begin
    if TimeNeeded > FTimeNeededForFullSceneRepaint then
    begin
      Level := Constrain(FPerformanceLevel + 1, PL_MICROTILES, PL_FULLSCENE);
      // did performance level change from previous level?
      if Level <> FPerformanceLevel then
      begin
{$IFDEF MICROTILES_DEBUGDRAW}
        FOldInvalidRectsCount := FDebugInvalidRects.Count;
{$ELSE}
        // save count of old invalid rects so we can use it in PrepareInvalidRects
        // the next time...
        FOldInvalidRectsCount := InvalidRects.Count;
{$ENDIF}
        FPerformanceLevel := Level;
        {$IFDEF CODESITE}
        CodeSite.SendInteger('EndPaintBuffer: FPerformanceLevel', FPerformanceLevel);
        {$ENDIF}
        // was this a timed check?
        if FTimedCheck then
        begin
          // time based approach failed, so add penalty
          FTimeDelta := Constrain(Integer(FTimeDelta + TIMER_PENALTY), TIMER_LOWLIMIT, TIMER_HIGHLIMIT);
          // schedule next check
          FNextCheck := Integer(GetTickCount) + FTimeDelta;
          FTimeNeededOnLastPenalty := TimeNeeded;
          FTimedCheck := False;
          {$IFDEF CODESITE}
          CodeSite.SendInteger('timed check failed, new delta', FTimeDelta);
          {$ENDIF}
        end;
        {$IFDEF CODESITE}
        CodeSite.AddSeparator;
        {$ENDIF}
        FAdaptionFailed := True;
      end;
    end
    else if TimeNeeded < FTimeNeededForFullSceneRepaint then
    begin
      if FTimedCheck then
      begin
        // time based approach had success!!
        // reset time delta back to lower limit, ie. remove penalties
        FTimeDelta := TIMER_LOWLIMIT;
        // schedule next check
        FNextCheck := Integer(GetTickCount) + FTimeDelta;
        FTimedCheck := False;
        {$IFDEF CODESITE}
        CodeSite.SendInteger('timed check succeeded, new delta', FTimeDelta);
        CodeSite.AddSeparator;
        {$ENDIF}
        FAdaptionFailed := False;
      end
      else
      begin
        // invalid rect count approach had success!!
        // shorten time for next check to benefit nonetheless in case we have a fallback...
        if FTimeDelta > TIMER_LOWLIMIT then
        begin
          // remove the penalty value 4 times from the current time delta
          FTimeDelta := Constrain(FTimeDelta - 4 * TIMER_PENALTY, TIMER_LOWLIMIT, TIMER_HIGHLIMIT);
          // schedule next check
          FNextCheck := Integer(GetTickCount) + FTimeDelta;
          {$IFDEF CODESITE}
          CodeSite.SendInteger('invalid rect count approach succeeded, new timer delta', FTimeDelta);
          CodeSite.AddSeparator;
          {$ENDIF}
        end;
        FAdaptionFailed := False;
      end;
    end
    else if (TimeNeeded < FTimeNeededOnLastPenalty) and FTimedCheck then
    begin
      // time approach had success optimizing the situation, so shorten time until next check
      FTimeDelta := Constrain(FTimeDelta - TIMER_PENALTY, TIMER_LOWLIMIT, TIMER_HIGHLIMIT);
      // schedule next check
      FNextCheck := Integer(GetTickCount) + FTimeDelta;
      FTimedCheck := False;
      {$IFDEF CODESITE}
      CodeSite.SendInteger('timed check succeeded, new delta', FTimeDelta);
      CodeSite.AddSeparator;
      {$ENDIF}
    end;
  end;

  FTimeNeededForLastRepaint := TimeNeeded;
end;

procedure SetupFunctions;
begin
  if HasEMMX then
  begin
    MicroTileUnion := M_MicroTileUnion;
    MicroTilesU := M_MicroTilesUnion;    // internal
  end
  else
  begin
    MicroTileUnion := _MicroTileUnion;
    MicroTilesU := _MicroTilesUnion;     // internal
  end;
end;

initialization
  SetupFunctions;

end.
