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
 * The Original Code is MicroTiles Extension for Graphics32
 *
 * The Initial Developer of the Original Code is
 * Andre Beckedorf - metaException OHG
 * Andre@metaException.de
 *
 * Portions created by the Initial Developer are Copyright (C) 2005
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

uses
  {$IFDEF CLX}
  Qt, Types, {$IFDEF LINUX}Libc, {$ELSE}Windows, {$ENDIF}
  {$ELSE}
  Windows,
  {$ENDIF}
  {$IFDEF CODESITE}CSIntf,{$ENDIF}
  SysUtils, Classes, Math, RTLConsts, GR32, GR32_LowLevel, GR32_System,
  GR32_Containers, GR32_Layers, GR32_Image;

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
procedure MicroTileUnion(var DstTile: TMicroTile; const SrcTile: TMicroTile);

// MicroTiles auxiliary routines
function MakeEmptyMicroTiles: TMicroTiles;
procedure MicroTilesCreate(var MicroTiles: TMicroTiles);
procedure MicroTilesDestroy(var MicroTiles: TMicroTiles);
procedure MicroTilesSetSize(var MicroTiles: TMicroTiles; const DstRect: TRect);
procedure MicroTilesClear(var MicroTiles: TMicroTiles);
procedure MicroTilesCopy(var DstTiles: TMicroTiles; SrcTiles: TMicroTiles);
procedure MicroTilesAddRect(var MicroTiles: TMicroTiles; Rect: TRect; RoundToWholeTiles: Boolean = False);
procedure MicroTilesUnion(var DstTiles: TMicroTiles; const SrcTiles: TMicroTiles; RoundToWholeTiles: Boolean = False);
function MicroTilesCalcRects(const MicroTiles: TMicroTiles; DstRects: TRectList; CountOnly: Boolean = False): Integer; overload;
function MicroTilesCalcRects(const MicroTiles: TMicroTiles; DstRects: TRectList; const Clip: TRect; CountOnly: Boolean = False): Integer; overload;
function MicroTilesCountEmptyTiles(const MicroTiles: TMicroTiles): Integer;
function MicroTilesCoherency(const MicroTiles: TMicroTiles): Single;

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

const
  PL_MICROTILES         = 0;
  PL_WHOLETILES         = 1;
  PL_FULLSCENE          = 2;

  TIMER_PENALTY         = 250;
  TIMER_LOWLIMIT        = 1000;
  TIMER_HIGHLIMIT       = 5000;

  DIRTYRECTS_DELTA      = 10;

type
  { TMicroTilesRepaintOptimizer }
  { Repaint manager that optimizes the repaint process using MicroTiles }
  TMicroTilesRepaintOptimizer = class(TCustomRepaintOptimizer)
  private
    // working tiles
    FWorkMicroTiles: PMicroTiles; // used by DrawLayerToMicroTiles
    FTempTiles: TMicroTiles;
    FDirtyTiles: TMicroTiles;
    FForcedDirtyTiles: TMicroTiles;

    // list of dirty layers
    FDirtyLayers: TList;

    // association that maps layers to their old dirty tiles
    FOldDirtyTilesMap: TMicroTilesMap;

    FWorkingTilesValid: Boolean;
    FOldDirtyTilesValid: Boolean;
    FUseDirtyTiles: Boolean;

    // adaptive stuff...
    FAdaptiveRepaint: Boolean;

    FPerfTimer: TPerfTimer;
    FPerformanceLevel: Integer;
    FTimeNeededForLastRepaint: Int64;
    FTimeNeededForFullSceneRepaint: Int64;
    FAdaptionFailed: Boolean;

    // vars for time based approach
    FTimedCheck: Boolean;
    FTimeDelta: Cardinal;
    FNextCheck: Cardinal;
    FTimeNeededOnLastPenalty: Int64;

    // vars for dirty rect difference approach
    FOldDirtyRectsCount: Integer;

{$IFDEF MICROTILES_DEBUGDRAW}
    FDebugMicroTiles: TMicroTiles;
    FDebugDirtyRects: TRectList;
{$ENDIF}

    procedure DrawLayerToMicroTiles(var DstTiles: TMicroTiles; Layer: TCustomLayer);
    procedure ValidateWorkingTiles;
    procedure UpdateOldDirtyTiles;
    procedure SetAdaptiveRepaint(const Value: Boolean);
    procedure BeginAdaption;
    procedure EndAdaption;
  protected
    procedure SetEnabled(const Value: Boolean); override;
  public
    constructor Create(Buffer: TBitmap32; DirtyRects: TRectList); override;
    destructor Destroy; override;

    procedure RegisterLayerCollection(Layers: TLayerCollection); override;
    procedure UnregisterLayerCollection(Layers: TLayerCollection); override;

    procedure Reset; override;
    procedure Resized(const NewWidth, NewHeight: Integer); override;

    function  CustomRepaintNeeded: Boolean; override;
    procedure PrepareDirtyRects; override;

    procedure BeginPaintBuffer; override;
    procedure EndPaintBuffer; override;

    // Layer handlers
    procedure ForcedUpdateHandler(Sender: TObject; const Rect: TRect); override;
    procedure LayerUpdateHandler(Sender: TObject; Layer: TCustomLayer); override;

    // Buffer handler
    procedure DrawMeasuringHandler(Sender: TObject; const DstRect: TRect); override;

    // LayerCollection handler
    procedure LayerCollectionNotifyHandler(Sender: TLayerCollection;
      Action: TLayerListNotification; Layer: TCustomLayer; Index: Integer); override;

    // custom settings:
    property AdaptiveRepaint: Boolean read FAdaptiveRepaint write SetAdaptiveRepaint;
  end;


implementation

{ MicroTile auxiliary routines }

function MakeMicroTile(const Left, Top, Right, Bottom: Integer): TMicroTile;
begin
  Result := Left shl 24 or Top shl 16 or Right shl 8 or Bottom;
end;

function MicroTileHeight(const Tile: TMicroTile): Integer;
begin
  Result := (Tile and $FF) - (Tile shl 16 and $FF);
end;

function MicroTileWidth(const Tile: TMicroTile): Integer;
begin
  Result := (Tile shl 8 and $FF) - (Tile shl 24);
end;

procedure MicroTileUnion(var DstTile: TMicroTile; const SrcTile: TMicroTile);
begin
  if not((DstTile = MICROTILE_FULL) or (SrcTile = MICROTILE_EMPTY) or
         (MicroTileWidth(SrcTile) = 0) or (MicroTileHeight(SrcTile) = 0)) then
  begin
    if (DstTile = MICROTILE_EMPTY) or (SrcTile = MICROTILE_FULL) then
      DstTile := SrcTile
    else
      DstTile := Min(DstTile shr 24, SrcTile shr 24) shl 24 or
                 Min(DstTile shr 16 and $FF, SrcTile shr 16 and $FF) shl 16 or
                 Max(DstTile shr 8 and $FF, SrcTile shr 8 and $FF) shl 8 or
                 Max(DstTile and $FF, SrcTile and $FF);
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

  MicroTiles.BoundsUsedTiles := MakeRect(MicroTiles.Columns, MicroTiles.Rows, 0, 0);
  FillLongword(MicroTiles.Tiles^[0], MicroTiles.Count, MICROTILE_EMPTY);
end;

procedure MicroTilesClear(var MicroTiles: TMicroTiles);
begin
  MicroTiles.BoundsUsedTiles := MakeRect(MicroTiles.Columns, MicroTiles.Rows, 0, 0);
  FillLongword(MicroTiles.Tiles^[0], MicroTiles.Count, MICROTILE_EMPTY);
end;

procedure MicroTilesCopy(var DstTiles: TMicroTiles; SrcTiles: TMicroTiles);
var
  CurRow, Width: Integer;
  SrcTilePtr, DstTilePtr: PMicroTile;
begin
  DstTiles.BoundsRect := SrcTiles.BoundsRect;
  DstTiles.Columns := SrcTiles.Columns;
  DstTiles.Rows := SrcTiles.Rows;
  DstTiles.BoundsUsedTiles := SrcTiles.BoundsUsedTiles;

  DstTiles.Count := SrcTiles.Count;
  ReallocMem(DstTiles.Tiles, SrcTiles.Count * SizeOf(TMicroTile));
  FillLongword(DstTiles.Tiles^[0], SrcTiles.Count, MICROTILE_EMPTY);

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
      TilePtr2 := TilePtr;
      for CurCol := LeftTile to RightTile do
      begin
        TilePtr2^ := MICROTILE_FULL;
        Inc(TilePtr2);
      end;
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

      // top-left corner
      MicroTileUnion(TilePtr2^, MakeMicroTile(ModLeft, ModTop, MICROTILE_SIZE, MICROTILE_SIZE));
      Inc(TilePtr2);

      // top edge
      if ColSpread > 2 then
        for CurRow := LeftTile + 1 to RightTile - 1 do
        begin
          MicroTileUnion(TilePtr2^, MakeMicroTile(0, ModTop, MICROTILE_SIZE, MICROTILE_SIZE));
          Inc(TilePtr2);
        end;

      // top-right corner
      MicroTileUnion(TilePtr2^, MakeMicroTile(0, ModTop, ModRight, MICROTILE_SIZE));

      Inc(TilePtr, MicroTiles.Columns);

      // left edge, content, right edge
      if RowSpread > 2 then
        for CurCol := TopTile + 1 to BottomTile - 1 do
        begin
          TilePtr2 := TilePtr;

          // left edge
          MicroTileUnion(TilePtr2^, MakeMicroTile(ModLeft, 0, MICROTILE_SIZE, MICROTILE_SIZE));
          Inc(TilePtr2);

          // content
          if ColSpread > 2 then
            for CurRow := LeftTile + 1 to RightTile - 1 do
            begin
              TilePtr2^ := MICROTILE_FULL;
              Inc(TilePtr2);
            end;

          // right edge
          MicroTileUnion(TilePtr2^, MakeMicroTile(0, 0, ModRight, MICROTILE_SIZE));

          Inc(TilePtr, MicroTiles.Columns);
        end;

      TilePtr2 := TilePtr;

      // bottom-left corner
      MicroTileUnion(TilePtr2^, MakeMicroTile(ModLeft, 0, MICROTILE_SIZE, ModBottom));
      Inc(TilePtr2);

      // bottom edge
      if ColSpread > 2 then
        for CurRow := LeftTile + 1 to RightTile - 1 do
        begin
          MicroTileUnion(TilePtr2^, MakeMicroTile(0, 0, MICROTILE_SIZE, ModBottom));
          Inc(TilePtr2);
        end;

      // bottom-right corner
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


procedure MicroTilesUnion(var DstTiles: TMicroTiles; const SrcTiles: TMicroTiles; RoundToWholeTiles: Boolean);

var
  SrcTilePtr, DstTilePtr: PMicroTile;
  SrcTilePtr2, DstTilePtr2: PMicroTile;
  X, Y: Integer;
begin
  if SrcTiles.Count = 0 then Exit;
  
  SrcTilePtr := @SrcTiles.Tiles^[SrcTiles.BoundsUsedTiles.Top * SrcTiles.Columns + SrcTiles.BoundsUsedTiles.Left];
  DstTilePtr := @DstTiles.Tiles^[SrcTiles.BoundsUsedTiles.Top * DstTiles.Columns + SrcTiles.BoundsUsedTiles.Left];

  if RoundToWholeTiles then
    for Y := SrcTiles.BoundsUsedTiles.Top to SrcTiles.BoundsUsedTiles.Bottom do
    begin
      SrcTilePtr2 := SrcTilePtr;
      DstTilePtr2 := DstTilePtr;
      for X := SrcTiles.BoundsUsedTiles.Left to SrcTiles.BoundsUsedTiles.Right do
      begin
        if not((DstTilePtr2^ = MICROTILE_FULL) or (SrcTilePtr2^ = MICROTILE_EMPTY) or
               (MicroTileWidth(SrcTilePtr2^) = 0) or (MicroTileHeight(SrcTilePtr2^) = 0)) then
          DstTilePtr2^ := MICROTILE_FULL;

        Inc(DstTilePtr2);
        Inc(SrcTilePtr2);
      end;
      Inc(DstTilePtr, DstTiles.Columns);
      Inc(SrcTilePtr, SrcTiles.Columns);
    end
  else
    for Y := SrcTiles.BoundsUsedTiles.Top to SrcTiles.BoundsUsedTiles.Bottom do
    begin
      SrcTilePtr2 := SrcTilePtr;
      DstTilePtr2 := DstTilePtr;
      for X := SrcTiles.BoundsUsedTiles.Left to SrcTiles.BoundsUsedTiles.Right do
      begin
        if not((DstTilePtr2^ = MICROTILE_FULL) or (SrcTilePtr2^ = MICROTILE_EMPTY) or
               (MicroTileWidth(SrcTilePtr2^) = 0) or (MicroTileHeight(SrcTilePtr2^) = 0)) then
        begin
          if (DstTilePtr2^ = MICROTILE_EMPTY) or (SrcTilePtr2^ = MICROTILE_FULL) then
            DstTilePtr2^ := SrcTilePtr2^
          else
            DstTilePtr2^ := Min(DstTilePtr2^ shr 24, SrcTilePtr2^ shr 24) shl 24 or
                            Min(DstTilePtr2^ shr 16 and $FF, SrcTilePtr2^ shr 16 and $FF) shl 16 or
                            Max(DstTilePtr2^ shr 8 and $FF, SrcTilePtr2^ shr 8 and $FF) shl 8 or
                            Max(DstTilePtr2^ and $FF, SrcTilePtr2^ and $FF);
        end;

        Inc(DstTilePtr2);
        Inc(SrcTilePtr2);
      end;
      Inc(DstTilePtr, DstTiles.Columns);
      Inc(SrcTilePtr, SrcTiles.Columns);
    end;

  with DstTiles.BoundsUsedTiles do
  begin
    if SrcTiles.BoundsUsedTiles.Left < Left then Left := SrcTiles.BoundsUsedTiles.Left;
    if SrcTiles.BoundsUsedTiles.Top < Top then Top := SrcTiles.BoundsUsedTiles.Top;
    if SrcTiles.BoundsUsedTiles.Right > Right then Right := SrcTiles.BoundsUsedTiles.Right;
    if SrcTiles.BoundsUsedTiles.Bottom > Bottom then Bottom := SrcTiles.BoundsUsedTiles.Bottom;
  end;
end;


function MicroTilesCalcRects(const MicroTiles: TMicroTiles; DstRects: TRectList;
  CountOnly: Boolean = False): Integer;
begin
  Result := MicroTilesCalcRects(MicroTiles, DstRects, MicroTiles.BoundsRect, CountOnly);
end;


function MicroTilesCalcRects(const MicroTiles: TMicroTiles; DstRects: TRectList;
  const Clip: TRect; CountOnly: Boolean = False): Integer;
var
  Rects: Array Of TRect;
  Glom: Array Of Integer;
  StartIndex: Integer;
  RectPtr: PRect;
  TempTile: TMicroTile;
  NewLeft, NewTop, NewRight, NewBottom: Integer;
  CurCol, CurRow, I, RectsCount: Integer;
begin
  Result := 0;

  if (MicroTiles.Count = 0) or
     (MicroTiles.BoundsUsedTiles.Right - MicroTiles.BoundsUsedTiles.Left < 0) or
     (MicroTiles.BoundsUsedTiles.Bottom - MicroTiles.BoundsUsedTiles.Top < 0) then Exit;
  
  SetLength(Rects, MicroTiles.Columns * MicroTiles.Rows);
  SetLength(Glom, MicroTiles.Columns * MicroTiles.Rows);
  FillLongword(Glom[0], Length(Glom), Cardinal(-1));

  I := 0;
  RectsCount := 0;

  for CurRow := 0 to MicroTiles.Rows - 1 do
  begin
    CurCol := 0;
    while CurCol < MicroTiles.Columns do
    begin
      TempTile := MicroTiles.Tiles[I];

      if not(MicroTiles.Tiles[I] = MICROTILE_EMPTY) then
      begin
        NewLeft := Constrain(CurCol shl MICROTILE_SHIFT + MicroTiles.Tiles[I] shr 24,
                             Clip.Left, Clip.Right);

        NewTop := Constrain(CurRow shl MICROTILE_SHIFT + MicroTiles.Tiles[I] shr 16 and $FF,
                            Clip.Top, Clip.Bottom);

        NewBottom := Constrain(CurRow shl MICROTILE_SHIFT + MicroTiles.Tiles[I] and $FF,
                               Clip.Top, Clip.Bottom);

        StartIndex := I;

        if not((MicroTiles.Tiles[I] shr 8 and $FF <> MICROTILE_SIZE) or
               (CurCol = MicroTiles.Columns - 1)) then
        begin
          while True do
          begin
            Inc(CurCol);
            Inc(I);

            if (CurCol = MicroTiles.Columns) or
               ((MicroTiles.Tiles[I] shr 16 and $FF) <> (TempTile shr 16 and $FF)) or
               ((MicroTiles.Tiles[I] and $FF) <> (TempTile and $FF)) or
               ((MicroTiles.Tiles[I] shr 24) <> 0) then
            begin
              Dec(CurCol);
              Dec(I);
              Break;
            end;
          end;
        end;

        NewRight := Constrain(CurCol shl MICROTILE_SHIFT + MicroTiles.Tiles[I] shr 8 and $FF,
                              Clip.Left, Clip.Right);

        if (Glom[StartIndex] <> -1) and
           (Rects[Glom[StartIndex]].Left = NewLeft) and
           (Rects[Glom[StartIndex]].Right = NewRight) and
           (Rects[Glom[StartIndex]].Bottom = NewTop) then
        begin
          Rects[Glom[StartIndex]].Bottom := NewBottom;

          if CurRow <> MicroTiles.Rows - 1 then
            Glom[StartIndex + MicroTiles.Columns] := Glom[StartIndex];
        end
        else
          with Rects[RectsCount] do
          begin
            Left := NewLeft;
            Top := NewTop;
            Right := NewRight;
            Bottom := NewBottom;

            if CurRow <> MicroTiles.Rows - 1 then
              Glom[StartIndex + MicroTiles.Columns] := RectsCount;

            Inc(RectsCount);
          end;
      end;

      Inc(I);
      Inc(CurCol);
    end;
  end;

  Result := RectsCount;
  
  if not CountOnly then
    for I := 0 to RectsCount do DstRects.Add(Rects[I]);
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

function MicroTilesCoherency(const MicroTiles: TMicroTiles): Single;
var
  EmptyTilesCount: Integer;
begin
  { TODO : Needs work. }
  if MicroTiles.Count > 0 then
  begin
    EmptyTilesCount := MicroTilesCountEmptyTiles(MicroTiles);
    Result := EmptyTilesCount / MicroTiles.Count;
  end
  else
    Result := 0;
end;

{$IFDEF MICROTILES_DEBUGDRAW}
procedure MicroTilesDebugDraw(const MicroTiles: TMicroTiles; DstBitmap: TBitmap32; DrawOptimized: Boolean);
var
  I: Integer;
  TempRect: TRect;
  Rects: TRectList;
begin
  if DrawOptimized then
  begin
    Rects := TRectList.Create;
    MicroTilesCalcRects(MicroTiles, Rects);
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


type
  TLayerCollectionAccess = class(TLayerCollection);
  TCustomLayerAccess = class(TCustomLayer);

  
{ TMicroTilesRepaintManager }

constructor TMicroTilesRepaintOptimizer.Create(Buffer: TBitmap32; DirtyRects: TRectList);
begin
  inherited;
  FOldDirtyTilesMap := TMicroTilesMap.Create;
  FDirtyLayers := TList.Create;
  FPerfTimer := TPerfTimer.Create;
  FAdaptiveRepaint := True;

  MicroTilesCreate(FDirtyTiles);
  MicroTilesCreate(FTempTiles);
  MicroTilesCreate(FForcedDirtyTiles);

{$IFDEF MICROTILES_DEBUGDRAW}
  MicroTilesCreate(FDebugMicroTiles);
  FDebugDirtyRects := TRectList.Create;
{$ENDIF}
end;

destructor TMicroTilesRepaintOptimizer.Destroy;
begin
  MicroTilesDestroy(FForcedDirtyTiles);
  MicroTilesDestroy(FTempTiles);
  MicroTilesDestroy(FDirtyTiles);

  FPerfTimer.Free;
  FDirtyLayers.Free;
  FOldDirtyTilesMap.Free;

{$IFDEF MICROTILES_DEBUGDRAW}
  FDebugDirtyRects.Free;
  MicroTilesDestroy(FDebugMicroTiles);
{$ENDIF}

  inherited;
end;

procedure TMicroTilesRepaintOptimizer.ForcedUpdateHandler(Sender: TObject; const Rect: TRect);
begin
  ValidateWorkingTiles;
  MicroTilesAddRect(FForcedDirtyTiles, Rect);
  FUseDirtyTiles := True;
end;

procedure TMicroTilesRepaintOptimizer.LayerUpdateHandler(Sender: TObject; Layer: TCustomLayer);
begin
  if FOldDirtyTilesValid and not TCustomLayerAccess(Layer).Dirty then
  begin
    FDirtyLayers.Add(Layer);
    TCustomLayerAccess(Layer).Dirty := True;
    FUseDirtyTiles := True;
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
        TilesPtr := FOldDirtyTilesMap.Add(Layer)^;
        MicroTilesSetSize(TilesPtr^, Buffer.BoundsRect);
        FOldDirtyTilesValid := True;
      end;

    lnLayerDeleted:
      begin
        if FOldDirtyTilesValid then
        begin
          // force repaint of tiles that the layer did previously allocate
          MicroTilesUnion(FDirtyTiles, FOldDirtyTilesMap[Layer]^);
          FUseDirtyTiles := True;
        end;
        FDirtyLayers.Remove(Layer);
        FOldDirtyTilesMap.Remove(Layer);
      end;

    lnCleared:
      begin
        if FOldDirtyTilesValid then
        begin
          with TPointerMapIterator.Create(FOldDirtyTilesMap) do
          try
            while Next do
              MicroTilesUnion(FDirtyTiles, PMicroTiles(Data)^);
          finally
            Free;
          end;

          FUseDirtyTiles := True;
        end;
        FOldDirtyTilesMap.Clear;
        FOldDirtyTilesValid := True;
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
    MicroTilesSetSize(FDirtyTiles, BoundsRect);
    MicroTilesSetSize(FForcedDirtyTiles, BoundsRect);
    FWorkingTilesValid := True;
  end;
end;

procedure TMicroTilesRepaintOptimizer.UpdateOldDirtyTiles;
var
  I, J: Integer;
  BoundsRect: TRect;
  TilesPtr: PMicroTiles;
  Layer: TCustomLayer;
begin
  If not FOldDirtyTilesValid then  // check if old dirty tiles need resize and rerendering...
  begin
    ValidateWorkingTiles;

    BoundsRect := Buffer.BoundsRect; // save into local var since TBitmap32.BoundsRect is a method...

    for I := 0 to LayerCollections.Count - 1 do
    with TLayerCollection(LayerCollections[I]) do
      for J := 0 to Count - 1 do
      begin
        Layer := Items[J];
        TilesPtr := FOldDirtyTilesMap.Add(Layer)^;
        MicroTilesSetSize(TilesPtr^, BoundsRect);
        DrawLayerToMicroTiles(TilesPtr^, Layer);
        TCustomLayerAccess(Layer).Dirty := False;
      end;

    FOldDirtyTilesValid := True;
    FUseDirtyTiles := False;
  end;
end;

function TMicroTilesRepaintOptimizer.CustomRepaintNeeded: Boolean;
begin
  UpdateOldDirtyTiles;
  Result := FUseDirtyTiles;
end;

procedure TMicroTilesRepaintOptimizer.Reset;
var
  I: Integer;
begin
  // Unmark dirty layers
  for I := 0 to FDirtyLayers.Count - 1 do
    TCustomLayerAccess(FDirtyLayers[I]).Dirty := False;

  FDirtyLayers.Clear;
  FUseDirtyTiles := False;
  FOldDirtyTilesValid := False;
end;

procedure TMicroTilesRepaintOptimizer.Resized(const NewWidth, NewHeight: Integer);
begin
  FWorkingTilesValid := False;  // force resizing of working microtiles
  FOldDirtyTilesValid := False; // force resizing of old dirty tiles -> force complete repaint
end;

procedure TMicroTilesRepaintOptimizer.RegisterLayerCollection(Layers: TLayerCollection);
begin
  inherited;

  if Enabled then
    with TLayerCollectionAccess(Layers) do
    begin
      OnLayerUpdated := LayerUpdateHandler;
      OnLayerResized := LayerUpdateHandler;
      OnForcedUpdate := ForcedUpdateHandler;
      OnListNotify := LayerCollectionNotifyHandler;
    end;
end;

procedure TMicroTilesRepaintOptimizer.UnregisterLayerCollection(Layers: TLayerCollection);
begin
  with TLayerCollectionAccess(Layers) do
  begin
    OnLayerUpdated := nil;
    OnLayerResized := nil;
    OnForcedUpdate := nil;
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
        OnLayerResized := LayerUpdateHandler;
        OnForcedUpdate := ForcedUpdateHandler;
        OnListNotify := LayerCollectionNotifyHandler;
      end;

      FWorkingTilesValid := False;
      FOldDirtyTilesValid := False;
      UpdateOldDirtyTiles;
      FUseDirtyTiles := True; // force repaint of new layers on invalidation...
    end
    else
    begin
      // clean up:
      for I := 0 to LayerCollections.Count - 1 do
      with TLayerCollectionAccess(LayerCollections[I]) do
      begin
        OnLayerUpdated := nil;
        OnLayerResized := nil;
        OnForcedUpdate := nil;
        OnListNotify := nil;
      end;

      MicroTilesDestroy(FDirtyTiles);
      MicroTilesDestroy(FTempTiles);
      MicroTilesDestroy(FForcedDirtyTiles);

      FUseDirtyTiles := False;
      FOldDirtyTilesValid := False;
      FOldDirtyTilesMap.Clear;
      FDirtyLayers.Clear;
    end;
    inherited;
  end;
end;

procedure TMicroTilesRepaintOptimizer.SetAdaptiveRepaint(const Value: Boolean);
begin
  if FAdaptiveRepaint <> Value then
  begin
    FAdaptiveRepaint := Value;
    FTimeDelta := TIMER_LOWLIMIT;
    FAdaptionFailed := False;
    FPerformanceLevel := PL_MICROTILES;
  end;
end;

procedure TMicroTilesRepaintOptimizer.BeginPaintBuffer;
begin
  if AdaptiveRepaint then FPerfTimer.Start;
end;

procedure TMicroTilesRepaintOptimizer.EndPaintBuffer;
begin
  FUseDirtyTiles := False;

{$IFDEF MICROTILES_DEBUGDRAW}
  {$IFDEF MICROTILES_DEBUGDRAW_UNOPTIMIZED}
    MicroTilesDebugDraw(FDebugMicroTiles, Buffer, False);
  {$ELSE}
    MicroTilesDebugDraw(FDebugMicroTiles, Buffer, True);
  {$ENDIF}
{$ENDIF}

  EndAdaption;
end;

procedure TMicroTilesRepaintOptimizer.DrawLayerToMicroTiles(var DstTiles: TMicroTiles; Layer: TCustomLayer);
begin
  Buffer.BeginMeasuring(DrawMeasuringHandler);
  FWorkMicroTiles := @DstTiles;
  TCustomLayerAccess(Layer).DoPaint(Buffer);
  Buffer.EndMeasuring;
end;

procedure TMicroTilesRepaintOptimizer.DrawMeasuringHandler(Sender: TObject; const DstRect: TRect);
var
  TempRect: TRect;
begin
  TempRect := DstRect;
  TestSwap(TempRect.Left, TempRect.Right);
  TestSwap(TempRect.Top, TempRect.Bottom);
  InflateRect(TempRect, 2, 2);
  MicroTilesAddRect(FWorkMicroTiles^, TempRect, FPerformanceLevel = PL_FULLSCENE);
end;

procedure TMicroTilesRepaintOptimizer.PrepareDirtyRects;
var
  I: Integer;
  Layer: TCustomLayer;
  UseWholeTiles: Boolean;
  LayerTilesPtr: PMicroTiles;
begin
  if FUseDirtyTiles then
  begin
    ValidateWorkingTiles;

    if FDirtyLayers.Count > 0 then
    begin
      // Determine if the use of whole tiles is better for current performance level
      UseWholeTiles := FPerformanceLevel > PL_MICROTILES;

      for I := 0 to FDirtyLayers.Count - 1 do
      begin
        Layer := FDirtyLayers[I];

        // Clear temporary tiles
        MicroTilesClear(FTempTiles);
        // Draw layer to temporary tiles
        DrawLayerToMicroTiles(FTempTiles, Layer);

        // Combine temporary tiles with the global dirty tiles
        MicroTilesUnion(FDirtyTiles, FTempTiles, UseWholeTiles);

        // Retrieve old dirty tiles for the current layer
        LayerTilesPtr := FOldDirtyTilesMap[Layer];

        // Combine old dirty tiles with the global dirty tiles
        MicroTilesUnion(FDirtyTiles, LayerTilesPtr^, UseWholeTiles);

        // Copy temporary (current) dirty tiles to the layer
        MicroTilesCopy(LayerTilesPtr^, FTempTiles);

        // Unmark layer as dirty
        TCustomLayerAccess(Layer).Dirty := False;
      end;
      FDirtyLayers.Clear;
    end;
    
{$IFDEF MICROTILES_DEBUGDRAW}
    MicroTilesCalcRects(FDirtyTiles, DirtyRects);
    MicroTilesCopy(FDebugMicroTiles, FDirtyTiles);
    MicroTilesUnion(FDebugMicroTiles, FForcedDirtyTiles);
{$ELSE}
    // Calculate optimized rectangles from global dirty tiles
    MicroTilesCalcRects(FDirtyTiles, DirtyRects);
    // Calculate optimized rectangles from forced dirty tiles
    MicroTilesCalcRects(FForcedDirtyTiles, DirtyRects);
{$ENDIF}
  end;

  BeginAdaption;

{$IFDEF MICROTILES_DEBUGDRAW}
  if DirtyRects.Count > 0 then
  begin
    FDebugDirtyRects.Count := DirtyRects.Count;
    Move(DirtyRects[0]^, FDebugDirtyRects[0]^, DirtyRects.Count * SizeOf(TRect));
    DirtyRects.Clear;
  end;
{$ENDIF}

  // Rects have been created, so we don't need the tiles any longer, clear them.
  MicroTilesClear(FDirtyTiles);
  MicroTilesClear(FForcedDirtyTiles);
end;

procedure TMicroTilesRepaintOptimizer.BeginAdaption;
begin
  if AdaptiveRepaint and (FPerformanceLevel > PL_MICROTILES) then
  begin
    if GetTickCount > FNextCheck then
    begin
      FPerformanceLevel := Constrain(FPerformanceLevel - 1, PL_MICROTILES, PL_FULLSCENE);
      {$IFDEF CODESITE}
      CodeSite.SendInteger('PrepareDirtyRects(Timed): FPerformanceLevel', FPerformanceLevel);
      {$ENDIF}
      FTimedCheck := True;
    end
    else if not FAdaptionFailed and (DirtyRects.Count < FOldDirtyRectsCount - DIRTYRECTS_DELTA) then
    begin
      FPerformanceLevel := Constrain(FPerformanceLevel - 1, PL_MICROTILES, PL_FULLSCENE);
      {$IFDEF CODESITE}
      CodeSite.SendInteger('PrepareDirtyRects: FPerformanceLevel', FPerformanceLevel);
      {$ENDIF}
    end
    else if FPerformanceLevel = PL_FULLSCENE then
      // we need a full scene rendition, so clear the dirty rects
      DirtyRects.Clear;
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
  if FDebugDirtyRects.Count = 0 then
{$ELSE}
  if DirtyRects.Count = 0 then
{$ENDIF}
    FTimeNeededForFullSceneRepaint := TimeNeeded
  else if AdaptiveRepaint then
  begin
    if TimeNeeded > FTimeNeededForFullSceneRepaint then
    begin
      Level := Constrain(FPerformanceLevel + 1, PL_MICROTILES, PL_FULLSCENE);
      // did performance level change from previous level?
      if Level <> FPerformanceLevel then
      begin
{$IFDEF MICROTILES_DEBUGDRAW}
        FOldDirtyRectsCount := FDebugDirtyRects.Count;
{$ELSE}
        // save count of old dirty rects so we can use it in PrepareDirtyRects
        // the next time...
        FOldDirtyRectsCount := DirtyRects.Count;
{$ENDIF}
        FPerformanceLevel := Level;
        {$IFDEF CODESITE}
        CodeSite.SendInteger('EndPaintBuffer: FPerformanceLevel', FPerformanceLevel);
        {$ENDIF}
        // was this a timed check?
        if FTimedCheck then
        begin
          // time based approach failed, so add penalty
          FTimeDelta := Constrain(FTimeDelta + TIMER_PENALTY, TIMER_LOWLIMIT, TIMER_HIGHLIMIT);
          // schedule next check
          FNextCheck := GetTickCount + FTimeDelta;
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
        FNextCheck := GetTickCount + FTimeDelta;
        FTimedCheck := False;
        {$IFDEF CODESITE}
        CodeSite.SendInteger('timed check succeeded, new delta', FTimeDelta);
        CodeSite.AddSeparator;
        {$ENDIF}
        FAdaptionFailed := False;
      end
      else
      begin
        // dirty rect count approach had success!!
        // shorten time for next check to benefit nonetheless in case we have a fallback...
        if FTimeDelta > TIMER_LOWLIMIT then
        begin
          // remove the penalty value 4 times from the current time delta
          FTimeDelta := Constrain(FTimeDelta - 4 * TIMER_PENALTY, TIMER_LOWLIMIT, TIMER_HIGHLIMIT);
          // schedule next check
          FNextCheck := GetTickCount + FTimeDelta;
          {$IFDEF CODESITE}
          CodeSite.SendInteger('dirty rect count approach succeeded, new timer delta', FTimeDelta);
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
      FNextCheck := GetTickCount + FTimeDelta;
      FTimedCheck := False;
      {$IFDEF CODESITE}
      CodeSite.SendInteger('timed check succeeded, new delta', FTimeDelta);
      CodeSite.AddSeparator;
      {$ENDIF}
    end;
  end;

  FTimeNeededForLastRepaint := TimeNeeded;
end;

end.
