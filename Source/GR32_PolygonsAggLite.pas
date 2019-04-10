unit GR32_PolygonsAggLite;

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
 * The Original Code is a mixture of AggLite and the other polygon renderers of
 * Graphics32
 *
 * The Initial Developer is
 * Christian-W. Budde <Christian@savioursofsoul.de>
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2012
 * the Initial Developer. All Rights Reserved.
 *
 * AggLite is based on Anti-Grain Geometry (Version 2.0)
 * Copyright (C) 2002-2004 Maxim Shemanarev (McSeem)
 *
 * Permission to copy, use, modify, sell and distribute this software
 * is granted provided this copyright notice appears in all copies.
 * This software is provided "as is" without express or implied
 * warranty, and with no claim as to its suitability for any purpose.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  Types, GR32, GR32_Polygons, GR32_Transforms;

type
  TPolygonRenderer32AggLite = class(TPolygonRenderer32)
  protected
    procedure Render(CellsPtr: Pointer; MinX, MaxX: Integer);
  public
    procedure PolygonFS(const Points: TArrayOfFloatPoint;
      const ClipRect: TFloatRect); override;
    procedure PolyPolygonFS(const Points: TArrayOfArrayOfFloatPoint;
      const ClipRect: TFloatRect); override;
  end;

procedure PolyPolygonFS_AggLite(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolygonFS_AggLite(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolyPolygonFS_AggLite(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  Filler: TCustomPolygonFiller; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolygonFS_AggLite(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  Filler: TCustomPolygonFiller; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolyPolygonFS_AggLite(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolygonFS_AggLite(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolyPolygonFS_AggLite(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  ClipRect: TRect; Filler: TCustomPolygonFiller; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolygonFS_AggLite(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  ClipRect: TRect; Filler: TCustomPolygonFiller; FillMode: TPolyFillMode = pfAlternate;
  Transformation: TTransformation = nil); overload;
procedure PolyPolylineFS_AggLite(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  Color: TColor32; Closed: Boolean = False; StrokeWidth: TFloat = 1.0;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: TFloat = 4.0; Transformation: TTransformation = nil); overload;
procedure PolyPolylineFS_AggLite(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  Filler: TCustomPolygonFiller; Closed: Boolean = False; StrokeWidth: TFloat = 1.0;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: TFloat = 4.0; Transformation: TTransformation = nil); overload;
procedure PolylineFS_AggLite(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  Color: TColor32; Closed: Boolean = False; StrokeWidth: TFloat = 1.0;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: TFloat = 4.0; Transformation: TTransformation = nil); overload;
procedure PolylineFS_AggLite(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  Filler: TCustomPolygonFiller; Closed: Boolean = False; StrokeWidth: TFloat = 1.0;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: TFloat = 4.0; Transformation: TTransformation = nil); overload;
procedure DashLineFS_AggLite(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  const Dashes: TArrayOfFloat; Color: TColor32;
  Closed: Boolean = False; Width: TFloat = 1.0); overload;
procedure DashLineFS_AggLite(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  const Dashes: TArrayOfFloat; FillColor, StrokeColor: TColor32;
  Closed: Boolean; Width: TFloat; StrokeWidth: TFloat = 2.0); overload;
procedure DashLineFS_AggLite(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  const Dashes: TArrayOfFloat; Filler: TCustomPolygonFiller;
  Closed: Boolean = False; Width: TFloat = 1.0); overload;
procedure DashLineFS_AggLite(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  const Dashes: TArrayOfFloat; Filler: TCustomPolygonFiller; StrokeColor: TColor32;
  Closed: Boolean; Width: TFloat; StrokeWidth: TFloat = 2.0); overload;

implementation

uses
  Math, GR32_Blend, GR32_Gamma, GR32_LowLevel, GR32_System, GR32_Bindings,
  GR32_VectorUtils;

procedure PolyPolygonFS_AggLite(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  Color: TColor32; FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32AggLite;
begin
  Renderer := TPolygonRenderer32AggLite.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.Color := Color;
    Renderer.FillMode := FillMode;
    Renderer.PolyPolygonFS(Points, FloatRect(Bitmap.ClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure PolygonFS_AggLite(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  Color: TColor32; FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32AggLite;
begin
  Renderer := TPolygonRenderer32AggLite.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.Color := Color;
    Renderer.FillMode := FillMode;
    Renderer.PolygonFS(Points, FloatRect(Bitmap.ClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure PolyPolygonFS_AggLite(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  Filler: TCustomPolygonFiller; FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32AggLite;
begin
  if not Assigned(Filler) then Exit;
  Renderer := TPolygonRenderer32AggLite.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.Filler := Filler;
    Renderer.FillMode := FillMode;
    Renderer.PolyPolygonFS(Points, FloatRect(Bitmap.ClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure PolygonFS_AggLite(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  Filler: TCustomPolygonFiller; FillMode: TPolyFillMode; Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32AggLite;
begin
  if not Assigned(Filler) then Exit;
  Renderer := TPolygonRenderer32AggLite.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.Filler := Filler;
    Renderer.FillMode := FillMode;
    Renderer.PolygonFS(Points, FloatRect(Bitmap.ClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure PolyPolygonFS_AggLite(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode;
  Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32AggLite;
  IntersectedClipRect: TRect;
begin
  Renderer := TPolygonRenderer32AggLite.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.Color := Color;
    Renderer.FillMode := FillMode;
    GR32.IntersectRect(IntersectedClipRect, Bitmap.ClipRect, ClipRect);
    Renderer.PolyPolygonFS(Points, FloatRect(IntersectedClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure PolygonFS_AggLite(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode;
  Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32AggLite;
  IntersectedClipRect: TRect;
begin
  Renderer := TPolygonRenderer32AggLite.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.Color := Color;
    Renderer.FillMode := FillMode;
    GR32.IntersectRect(IntersectedClipRect, Bitmap.ClipRect, ClipRect);
    Renderer.PolygonFS(Points, FloatRect(IntersectedClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure PolyPolygonFS_AggLite(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  ClipRect: TRect; Filler: TCustomPolygonFiller; FillMode: TPolyFillMode;
  Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32AggLite;
  IntersectedClipRect: TRect;
begin
  if not Assigned(Filler) then Exit;
  Renderer := TPolygonRenderer32AggLite.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.Filler := Filler;
    Renderer.FillMode := FillMode;
    GR32.IntersectRect(IntersectedClipRect, Bitmap.ClipRect, ClipRect);
    Renderer.PolyPolygonFS(Points, FloatRect(IntersectedClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure PolygonFS_AggLite(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  ClipRect: TRect; Filler: TCustomPolygonFiller; FillMode: TPolyFillMode;
  Transformation: TTransformation);
var
  Renderer: TPolygonRenderer32AggLite;
  IntersectedClipRect: TRect;
begin
  if not Assigned(Filler) then Exit;
  Renderer := TPolygonRenderer32AggLite.Create;
  try
    Renderer.Bitmap := Bitmap;
    Renderer.Filler := Filler;
    Renderer.FillMode := FillMode;
    GR32.IntersectRect(IntersectedClipRect, Bitmap.ClipRect, ClipRect);
    Renderer.PolygonFS(Points, FloatRect(IntersectedClipRect), Transformation);
  finally
    Renderer.Free;
  end;
end;

procedure PolyPolylineFS_AggLite(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  Color: TColor32; Closed: Boolean; StrokeWidth: TFloat;
  JoinStyle: TJoinStyle; EndStyle: TEndStyle;
  MiterLimit: TFloat; Transformation: TTransformation);
var
  Dst: TArrayOfArrayOfFloatPoint;
begin
  Dst := BuildPolyPolyLine(Points, Closed, StrokeWidth, JoinStyle, EndStyle, MiterLimit);
  PolyPolygonFS_AggLite(Bitmap, Dst, Color, pfWinding, Transformation);
end;

procedure PolyPolylineFS_AggLite(Bitmap: TBitmap32; const Points: TArrayOfArrayOfFloatPoint;
  Filler: TCustomPolygonFiller; Closed: Boolean = False; StrokeWidth: TFloat = 1.0;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: TFloat = 4.0; Transformation: TTransformation = nil);
var
  Dst: TArrayOfArrayOfFloatPoint;
begin
  Dst := BuildPolyPolyLine(Points, Closed, StrokeWidth, JoinStyle, EndStyle, MiterLimit);
  PolyPolygonFS(Bitmap, Dst, Filler, pfWinding, Transformation);
end;

procedure PolylineFS_AggLite(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  Color: TColor32; Closed: Boolean; StrokeWidth: TFloat;
  JoinStyle: TJoinStyle; EndStyle: TEndStyle;
  MiterLimit: TFloat; Transformation: TTransformation);
begin
  PolyPolylineFS_AggLite(Bitmap, PolyPolygon(Points), Color, Closed, StrokeWidth,
    JoinStyle, EndStyle, MiterLimit, Transformation);
end;

procedure PolylineFS_AggLite(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  Filler: TCustomPolygonFiller; Closed: Boolean = False; StrokeWidth: TFloat = 1.0;
  JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt;
  MiterLimit: TFloat = 4.0; Transformation: TTransformation = nil);
begin
  PolyPolylineFS_AggLite(Bitmap, PolyPolygon(Points), Filler, Closed, StrokeWidth,
    JoinStyle, EndStyle, MiterLimit, Transformation);
end;

procedure DashLineFS_AggLite(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  const Dashes: TArrayOfFloat; Color: TColor32;
  Closed: Boolean = False; Width: TFloat = 1.0);
var
  MultiPoly: TArrayOfArrayOfFloatPoint;
begin
  MultiPoly := GR32_VectorUtils.BuildDashedLine(Points, Dashes, 0, Closed);
  PolyPolylineFS_AggLite(Bitmap, MultiPoly, Color, False, Width);
end;

procedure DashLineFS_AggLite(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  const Dashes: TArrayOfFloat; FillColor, StrokeColor: TColor32;
  Closed: Boolean; Width: TFloat; StrokeWidth: TFloat = 2.0);
var
  MultiPoly: TArrayOfArrayOfFloatPoint;
begin
  MultiPoly := GR32_VectorUtils.BuildDashedLine(Points, Dashes, 0, Closed);
  MultiPoly := BuildPolyPolyLine(MultiPoly, False, Width);
  PolyPolygonFS_AggLite(Bitmap, MultiPoly, FillColor);
  PolyPolylineFS_AggLite(Bitmap, MultiPoly, StrokeColor, True, StrokeWidth);
end;

procedure DashLineFS_AggLite(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  const Dashes: TArrayOfFloat; Filler: TCustomPolygonFiller;
  Closed: Boolean = False; Width: TFloat = 1.0);
var
  MultiPoly: TArrayOfArrayOfFloatPoint;
begin
  MultiPoly := GR32_VectorUtils.BuildDashedLine(Points, Dashes, 0, Closed);
  PolyPolylineFS_AggLite(Bitmap, MultiPoly, Filler, False, Width);
end;

procedure DashLineFS_AggLite(Bitmap: TBitmap32; const Points: TArrayOfFloatPoint;
  const Dashes: TArrayOfFloat; Filler: TCustomPolygonFiller; StrokeColor: TColor32;
  Closed: Boolean; Width: TFloat; StrokeWidth: TFloat = 2.0);
var
  MultiPoly: TArrayOfArrayOfFloatPoint;
begin
  MultiPoly := GR32_VectorUtils.BuildDashedLine(Points, Dashes, 0, Closed);
  MultiPoly := BuildPolyPolyLine(MultiPoly, False, Width);
  PolyPolygonFS_AggLite(Bitmap, MultiPoly, Filler);
  PolyPolylineFS_AggLite(Bitmap, MultiPoly, StrokeColor, True, StrokeWidth);
end;

const
  CPolyBaseShift = 8;
  CPolyBaseSize = 1 shl CPolyBaseShift;
  CPolyBaseMask = CPolyBaseSize - 1;

  CCellBlockShift = 12;
  CCellBlockSize  = 1 shl CCellBlockShift;
  CCellBlockMask  = CCellBlockSize - 1;
  CCellBlockPool  = 256;
  CCellBlockLimit = 1024;

type
  PPColor32  = ^PColor32;

  TPointWord = record
  case Byte of
    0: (X, Y: SmallInt);
    1: (PackedCoord: Integer);
  end;

  TCell = packed record
    Pnt: TPointWord;
    PackedCoord: Integer;
    Cover: Integer;
    Area: Integer;
  end;
  PCell = ^TCell;
  PPCell = ^PCell;

  TScanLine = class(TObject)
  private
    FCounts: PWord;
    FCovers: PColor32Array;
    FCurCount: PWord;
    FCurStartPtr: PPColor32;
    FLastX: Integer;
    FLastY: Integer;
    FMaxLen: Cardinal;
    FMinX: Integer;
    FNumSpans: Cardinal;
    FStartPtrs: PPColor32;
  public
    constructor Create(MinX, MaxX: Integer);
    destructor Destroy; override;

    procedure AddCell(X, Y: Integer; Cover: Cardinal);
    procedure AddSpan(X, Y: Integer; Len, Cover: Cardinal);
    function IsReady(Y: Integer): Integer;
    procedure ResetSpans;

    property BaseX: Integer read FMinX;
    property Y: Integer read FLastY;
    property NumSpans: Cardinal read FNumSpans;
    property CountsPtr: PWord read FCounts;
    property CoversPtr: PColor32Array read FCovers;
    property StartPtrs: PPColor32 read FStartPtrs;
  end;

  TOutlineFlag = (ofNotClosed, ofSortRequired);
  TOutlineFlags = set of TOutlineFlag;

  TOutline = class(TObject)
  private
    FCells: PPCell;
    FClose: TPoint;
    FCurBlock: Cardinal;
    FCurCell: TCell;
    FCurCellPtr: PCell;
    FCur: TPoint;
    FFlags: TOutlineFlags;
    FMaxBlocks: Cardinal;
    FMax: TPoint;
    FMin: TPoint;
    FNumBlocks: Cardinal;
    FNumCells: Cardinal;
    FSortedCells: PPCell;
    FSortedSize: Cardinal;
    procedure AddCurCell;
    procedure AllocateBlock;
    function GetCells: PPCell;
    procedure RenderLine(X1, Y1, X2, Y2: Integer);
    procedure RenderScanLine(EY, X1, Y1, X2, Y2: Integer);
    procedure SetCurCell(X, Y: Integer);
    procedure SortCells;
    procedure InternalReset;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LineTo(X, Y: Integer);
    procedure MoveTo(X, Y: Integer);
    procedure Reset;

    property Cells: PPCell read GetCells;
    property MaxX: Integer read FMax.X;
    property MaxY: Integer read FMax.Y;
    property MinX: Integer read FMin.X;
    property MinY: Integer read FMin.Y;
    property NumCells: Cardinal read FNumCells;
  end;

function Fixed8(C: TFloat): Integer; {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  Result := Trunc(C * CPolyBaseSize);
end;


{ TCell }

procedure SetCell(var Cell: TCell; CX, CY: Integer); {$IFDEF USEINLINING} inline; {$ENDIF}
begin
  with Cell do
  begin
    Pnt.X := SmallInt(CX);
    Pnt.Y := SmallInt(CY);
    PackedCoord := (CY shl 16) + CX;
    Cover := 0;
    Area := 0;
  end;
end;

procedure PartSort(var A, B: PPCell; const Stop: PCell);
{$IFDEF PUREPASCAL}
  {$IFDEF USEINLINING} inline; {$ENDIF}

  procedure SwapCells(A, B: PPCell); {$IFDEF USEINLINING} inline; {$ENDIF}
  var
    Temp: PCell;
  begin
    Temp := A^;
    A^ := B^;
    B^ := Temp;
  end;

begin
  while True do
  begin
    repeat
      Inc(A)
    until (A^^.PackedCoord >= Stop^.PackedCoord);
    repeat
      Dec(B)
    until (B^^.PackedCoord <= Stop^.PackedCoord);

    {$IFDEF FPC}
    if PtrInt(A) > PtrInt(B) then
      Break;
    {$ELSE}
    {$IFDEF HAS_NATIVEINT}
    if NativeInt(A) > NativeInt(B) then
      Break;
    {$ELSE}
    if Integer(A) > Integer(B) then
      Break;
    {$ENDIF}
    {$ENDIF}

    SwapCells(A, B);
  end;
{$ELSE}
asm
{$IFDEF CPUX86}
        PUSH    EBX
        PUSH    EDI
        PUSH    ESI
        PUSH    EBP

        MOV     ECX, [ECX + 4]
@0:
        MOV     EDI, [EAX]
@1:
        ADD     EDI, $04
        MOV     EBX, [EDI]
        CMP     ECX, [EBX + 4]
        JG      @1
        MOV     [EAX], EDI

        MOV     EDI, [EDX]
@2:
        SUB     EDI, $04
        MOV     EBX, [EDI]
        CMP     ECX, [EBX + 4]
        JL      @2
        MOV     [EDX], EDI

        CMP     EDI, [EAX]
        JLE     @3
        MOV     EBX, [EAX]

        MOV     ESI, [EBX]
        MOV     EBP, [EDI]
        MOV     [EDI], ESI
        MOV     [EBX], EBP

        JMP     @0

@3:
        POP     EBP
        POP     ESI
        POP     EDI
        POP     EBX
{$ENDIF}
{$IFDEF CPUX64}
        MOV     R8D, [R8 + 4]
@0:
        MOV     R9, [RCX]
@1:
        ADD     R9, $08
        MOV     RAX, [R9]
        CMP     R8D, [RAX + 4]
        JG      @1
        MOV     [RCX], R9


        MOV     R9, [RDX]
@2:
        SUB     R9, $08
        MOV     RAX, [R9]
        CMP     R8D, [RAX + 4]
        JL      @2
        MOV     [RDX], R9

        CMP     R9, [RCX]
        JLE     @3
        MOV     RAX, [RCX]

        MOV     R10, [RAX]
        MOV     R11, [R9]
        MOV     [RAX], R11
        MOV     [R9], R10
        JMP     @0
@3:
{$ENDIF}
{$ENDIF}
end;


procedure QSortCells(Start: PPCell; Num: Cardinal);
const
  QSortThreshold = 9;
var
  Stack: array [0 .. 79] of PPCell;
  Top: ^PPCell;
  Limit, Base, I, J, Pivot: PPCell;
  Len: Integer;

  procedure CheckCells(var A, B: PCell); {$IFDEF USEINLINING} inline; {$ENDIF}
  var
    Temp: PCell;
  begin
    if A^.PackedCoord < B^.PackedCoord then
    begin
      Temp := A;
      A := B;
      B := Temp;
    end;
  end;

  procedure SwapCells(A, B: PPCell); {$IFDEF USEINLINING} inline; {$ENDIF}
  var
    Temp: PCell;
  begin
    Temp := A^;
    A^ := B^;
    B^ := Temp;
  end;

  function LessThan(A, B: PPCell): Boolean; {$IFDEF USEINLINING} inline; {$ENDIF}
  begin
    Result := A^^.PackedCoord < B^^.PackedCoord;
  end;

begin
  {$IFDEF FPC}
  Limit := PPCell(PtrInt(Start) + Num * SizeOf(PCell));
  {$ELSE}
  {$IFDEF HAS_NATIVEINT}
  Limit := PPCell(NativeUInt(Start) + Num * SizeOf(PCell));
  {$ELSE}
  Limit := PPCell(Cardinal(Start) + Num * SizeOf(PCell));
  {$ENDIF}
  {$ENDIF}
  Base  := Start;
  Top   := @Stack[0];

  while True do
  begin
    {$IFDEF FPC}
    Len := (PtrInt(Limit) - PtrInt(Base)) div SizeOf(PCell);
    {$ELSE}
    {$IFDEF HAS_NATIVEINT}
    Len := (NativeInt(Limit) - NativeInt(Base)) div SizeOf(PCell);
    {$ELSE}
    Len := (Integer(Limit) - Integer(Base)) div SizeOf(PCell);
    {$ENDIF}
    {$ENDIF}

    if Len > QSortThreshold then
    begin
      // we use Base + (Len div 2) as the pivot
      Pivot := Base;
      Inc(Pivot,  Len div 2);
      SwapCells(Base, Pivot);

      I := Base;
      Inc(I);
      J := Limit;
      Dec(J);

      // now ensure that I^ <= Base^ <= J^
      CheckCells(J^, I^);
      CheckCells(Base^, I^);
      CheckCells(J^, Base^);

      PartSort(I, J, Base^);
      SwapCells(Base, J);

      // now, push the largest sub-array
      {$IFDEF FPC}
      if PtrInt(J) - PtrInt(Base) > PtrInt(Limit) - PtrInt(I) then
      {$ELSE}
      {$IFDEF HAS_NATIVEINT}
      if NativeInt(J) - NativeInt(Base) > NativeInt(Limit) - NativeInt(I) then
      {$ELSE}
      if Integer(J) - Integer(Base) > Integer(Limit) - Integer(I) then
      {$ENDIF}
      {$ENDIF}
      begin
        Top^ := Base;
        Inc(Top);
        Top^ := J;
        Base := I;
      end
      else
      begin
        Top^ := I;
        Inc(Top);
        Top^ := Limit;
        Limit := J;
      end;
      Inc(Top);
    end
    else
    begin
      // the sub-array is small, perform insertion sort
      J := Base;
      I := J;
      Inc(I);

      {$IFDEF FPC}
      while PtrInt(I) < PtrInt(Limit) do
      {$ELSE}
      {$IFDEF HAS_NATIVEINT}
      while NativeInt(I) < NativeInt(Limit) do
      {$ELSE}
      while Integer(I) < Integer(Limit) do
      {$ENDIF}
      {$ENDIF}
      begin
        {$IFDEF FPC}
        while LessThan(PPCell(PtrInt(J) + SizeOf(PCell)), J) do
        begin
          SwapCells(PPCell(PtrInt(J) + SizeOf(PCell)), J);
        {$ELSE}
        {$IFDEF HAS_NATIVEINT}
        while LessThan(PPCell(NativeUInt(J) + SizeOf(PCell)), J) do
        begin
          SwapCells(PPCell(NativeUInt(J) + SizeOf(PCell)), J);
        {$ELSE}
        while LessThan(PPCell(Cardinal(J) + SizeOf(PCell)), J) do
        begin
          SwapCells(PPCell(Cardinal(J) + SizeOf(PCell)), J);
        {$ENDIF}
        {$ENDIF}
          if J = Base then
            Break;
          Dec(J);
        end;
        J := I;
        Inc(I);
      end;

      {$IFDEF FPC}
      if PtrInt(Top) > PtrInt(@Stack[0]) then
      {$ELSE}
      {$IFDEF HAS_NATIVEINT}
      if NativeInt(Top) > NativeInt(@Stack[0]) then
      {$ELSE}
      if Integer(Top) > Integer(@Stack[0]) then
      {$ENDIF}
      {$ENDIF}
      begin
        Dec(Top, 2);
        Base := Top^;
        Limit := PPCell(Pointer(NativeInt(Top) + SizeOf(PPCell))^);
      end
      else
        Break;
    end;
  end;
end;

var
  FillSpan: procedure (Ptr: PColor32Array; Covers: PColor32; Count: Cardinal;
    const C: TColor32);

procedure FillSpan_Pas(Ptr: PColor32Array; Covers: PColor32; Count: Cardinal;
  const C: TColor32);
begin
  repeat
    BlendMemEx(C, PColor32(Ptr)^, Covers^);
    Inc(Covers);
    Inc(Ptr);
    Dec(Count);
  until Count = 0;
end;

procedure FillSpan_ASM(Ptr: PColor32Array; Covers: PColor32; Count: Cardinal;
  const C: TColor32);
asm
{$IFDEF CPUX86}
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        LEA     ESI, EDX + 4 * ECX             // ESI = Covers
        LEA     EDI, EAX + 4 * ECX             // EDI = P
        NEG     ECX

@LoopStart:
        MOVZX   EBX, [ESI + 4 * ECX]
        MOVZX   EAX, [EBP + $0B]               // EAX = C.A
        IMUL    EBX, EAX                       // EBX = Alpha

        MOVZX   EAX, [EDI + 4 * ECX]
        MOVZX   EDX, [EBP + $08]               // EDX = C.R
        SUB     EDX, EAX
        IMUL    EDX, EBX
        SHL     EAX, $10
        ADD     EDX, EAX
        SHR     EDX, $10
        MOV     [EDI + 4 * ECX], DL            // store to pointer

        MOVZX   EAX, [EDI + 4 * ECX + 1]
        MOVZX   EDX, [EBP + $09]               // EDX = C.G
        SUB     EDX, EAX
        IMUL    EDX, EBX
        SHL     EAX, $10
        ADD     EDX, EAX
        SHR     EDX, $10
        MOV     [EDI + 4 * ECX + 1], DL        // store to pointer

        MOVZX   EAX, [EDI + 4 * ECX + 2]
        MOVZX   EDX, [EBP + $0A]               // EDX = C.B
        SUB     EDX, EAX
        IMUL    EDX, EBX
        SHL     EAX, $10
        ADD     EDX, EAX
        SHR     EDX, $10
        MOV     [EDI + 4 * ECX + 2], DL        // store to pointer

        MOVZX   EAX, [EDI + 4 * ECX + 3]
        MOVZX   EDX, [EBP + $0B]               // EDX = C.A
        SUB     EDX, EAX
        IMUL    EDX, EBX
        SHL     EAX, $10
        ADD     EDX, EAX
        SHR     EDX, $10
        MOV     [EDI + 4 * ECX + 3], DL        // store to pointer

        ADD     ECX, 1
        JS      @LoopStart

        POP     EDI
        POP     ESI
        POP     EBX
{$ENDIF}
{$IFDEF CPUX64}
        LEA     R10, RDX + 4 * R8              // R10 = Covers
        LEA     R11, RCX + 4 * R8              // R11 = P
        NEG     R8D

@LoopStart:
        MOVZX   R9D, [R10 + 4 * R8]
        MOVZX   ECX, [EBP + $0B]               // ECX = C.A
        IMUL    R9D, ECX                       // R9D = Alpha

        MOVZX   ECX, [R11 + 4 * R8]
        MOVZX   EDX, [EBP + $08]               // EDX = C.R
        SUB     EDX, ECX
        IMUL    EDX, R9D
        SHL     ECX, $10
        ADD     EDX, ECX
        SHR     EDX, $10
        MOV     [R11 + 4 * R8], DL             // store to pointer

        MOVZX   ECX, [R11 + 4 * R8 + 1]
        MOVZX   EDX, [EBP + $09]               // EDX = C.G
        SUB     EDX, ECX
        IMUL    EDX, R9D
        SHL     ECX, $10
        ADD     EDX, ECX
        SHR     EDX, $10
        MOV     [R11 + 4 * R8 + 1], DL         // store to pointer

        MOVZX   ECX, [R11 + 4 * R8 + 2]
        MOVZX   EDX, [EBP + $0A]               // EDX = C.B
        SUB     EDX, ECX
        IMUL    EDX, R9D
        SHL     ECX, $10
        ADD     EDX, ECX
        SHR     EDX, $10
        MOV     [R11 + 4 * R8 + 2], DL         // store to pointer

        MOVZX   ECX, [R11 + 4 * R8 + 3]
        MOVZX   EDX, [EBP + $0B]               // EDX = C.A
        SUB     EDX, ECX
        IMUL    EDX, R9D
        SHL     ECX, $10
        ADD     EDX, ECX
        SHR     EDX, $10
        MOV     [R11 + 4 * R8 + 3], DL         // store to pointer

        ADD     R8D, 1
        JS      @LoopStart
{$ENDIF}
end;

{$IFNDEF OMIT_MMX}
{$IFDEF TARGET_X86}
procedure FillSpan_MMX(Ptr: PColor32Array; Covers: PColor32; Count: Cardinal;
  const C: TColor32);
asm
        JCXZ      @3

        PUSH      EBX
        PUSH      ESI
        MOV       ESI,EAX
        MOV       EBX,C

        PXOR      MM3,MM3            // MM3 = 0

        MOVD      MM1,EBX            // MM1 = C (Foreground)
        PUNPCKLBW MM1,MM3

        SHR       EBX,24
        JZ        @2
        INC       EBX                // 255:256 range bias

@1:
        MOVD      MM2,[ESI]          // MM2 = Dest (Background)
        PUNPCKLBW MM2,MM3
        MOV       EAX,[EDX]          // EAX = Alpha
        IMUL      EAX,EBX
        SHR       EAX,8
        SHL       EAX,4
        ADD       EAX,alpha_ptr
        MOVQ      MM0,MM1
        PSUBW     MM0,MM2
        PMULLW    MM0,[EAX]
        PSLLW     MM2,8
        MOV       EAX,bias_ptr
        PADDW     MM2,[EAX]
        PADDW     MM0,MM2
        PSRLW     MM0,8
        PACKUSWB  MM0,MM3
        MOVD      [ESI],MM0

        ADD       ESI,4
        ADD       EDX,4

        DEC       ECX
        JNZ       @1

@2:     POP       ESI
        POP       EBX

@3:
end;
{$ENDIF}
{$ENDIF}

{$IFNDEF OMIT_SSE2}
procedure FillSpan_SSE2(Ptr: PColor32Array; Covers: PColor32; Count: Cardinal;
  const C: TColor32);
asm
{$IFDEF TARGET_X86}
        JCXZ      @5

        PUSH      EBX
        MOV       EBX,C

        PXOR      XMM7,XMM7          // XMM7 = 0

        MOVD      XMM1,EBX           // XMM1 = C (Foreground)
        PUNPCKLBW XMM1,XMM7

        SHR       EBX,24
        JZ        @4
        INC       EBX                // 255:256 range bias

        PUSH      ESI
        MOV       ESI,EAX

@1:     MOVQ      XMM0,XMM1
        MOVD      XMM2,[ESI]         // XMM2 = Dest (Background)
        PUNPCKLBW XMM2,XMM7
        MOV       EAX,[EDX]          // EAX = Alpha
        IMUL      EAX,EBX
        SHR       EAX,8
        JZ        @3
        CMP       EAX,$FF
        JZ        @2
        SHL       EAX,4
        ADD       EAX,alpha_ptr
        PSUBW     XMM0,XMM2
        PMULLW    XMM0,[EAX]
        PSLLW     XMM2,8
        MOV       EAX,bias_ptr
        PADDW     XMM2,[EAX]
        PADDW     XMM0,XMM2
        PSRLW     XMM0,8

@2:     PACKUSWB  XMM0,XMM7
        MOVD      [ESI],XMM0

@3:     ADD       ESI,4
        ADD       EDX,4

        DEC       ECX
        JNZ       @1

        POP       ESI
@4:     POP       EBX

@5:
{$ENDIF}

{$IFDEF TARGET_X64}
        TEST      R8D,R8D
        JZ        @4

        PXOR      XMM7,XMM7          // XMM7 = 0

        MOVD      XMM1,R9D           // XMM1 = C (Foreground)
        PUNPCKLBW XMM1,XMM7

        SHR       R9D,24
        JZ        @2
        INC       R9D                // 255:256 range bias

@1:     MOVQ      XMM0,XMM1
        MOVD      XMM2,[RCX]         // XMM2 = Dest (Background)
        PUNPCKLBW XMM2,XMM7
        MOV       EAX,[RDX]          // EAX = Alpha
        IMUL      EAX,R9D
        SHR       EAX,8
        JZ        @3
        CMP       EAX,$FF
        JZ        @2
        SHL       EAX,4
        ADD       RAX,alpha_ptr
        PSUBW     XMM0,XMM2
        PMULLW    XMM0,[RAX]
        PSLLW     XMM2,8
        MOV       RAX,bias_ptr
        PADDW     XMM2,[RAX]
        PADDW     XMM0,XMM2
        PSRLW     XMM0,8

@2:     PACKUSWB  XMM0,XMM7
        MOVD      [RCX],XMM0

@3:     ADD       ECX,4
        ADD       EDX,4

        DEC       R8D
        JNZ       @1

@4:
{$ENDIF}
end;
{$ENDIF}

function CalculateAlpha(FillMode: TPolyFillMode; Area: Integer): Cardinal;
var
  Cover: Integer;
const
  CAAShift = 8;
  CAANum = 1 shl CAAShift;
  CAAMask  = CAANum - 1;
  CAA2Num  = CAANum shl 1;
  CAA2Mask = CAA2Num - 1;
begin
  Cover := SAR_9(Area);
  if Cover < 0 then
    Cover := -Cover;
  if FillMode = pfEvenOdd then
  begin
    Cover := Cover and CAA2Mask;
    if Cover > CAANum then
      Cover := CAA2Num - Cover;
  end;
  if Cover > CAAMask then
    Cover := CAAMask;
  Result := Cover;
end;


{ TScanLine }

constructor TScanLine.Create(MinX, MaxX: Integer);
begin
  inherited Create;

  FMaxLen := MaxX - MinX + 2;
  GetMem(FCovers, FMaxLen * SizeOf(TColor32));
  GetMem(FStartPtrs, FMaxLen * SizeOf(PColor32));
  GetMem(FCounts, FMaxLen * SizeOf(Word));
  FLastX := $7FFF;
  FLastY := $7FFF;
  FMinX := MinX;
  FCurCount := FCounts;
  FCurStartPtr := FStartPtrs;
  FNumSpans := 0;
end;

destructor TScanLine.Destroy;
begin
  FreeMem(FCounts);
  FreeMem(FStartPtrs);
  FreeMem(FCovers);

  inherited Destroy;
end;

procedure TScanLine.AddCell(X, Y: Integer; Cover: Cardinal);
begin
  Dec(X, FMinX);
  FCovers[X] := TColor32(Cover);
  if X = FLastX + 1 then
    Inc(FCurCount^)
  else
  begin
    Inc(FCurCount);
    FCurCount^ := 1;
    Inc(FCurStartPtr);
    FCurStartPtr^ := PColor32(@FCovers[X]);
    Inc(FNumSpans);
  end;
  FLastX := X;
  FLastY := Y;
end;

procedure TScanLine.AddSpan(X, Y: Integer; Len, Cover: Cardinal);
begin
  Dec(X, FMinX);
  FillLongWord(FCovers[X], Len, Cover);

  if X = FLastX + 1 then
    Inc(FCurCount^, Word(Len))
  else
  begin
    Inc(FCurCount);
    FCurCount^ := Word(Len);
    Inc(FCurStartPtr);
    FCurStartPtr^ := PColor32(@FCovers[X]);
    Inc(FNumSpans);
  end;
  FLastX := X + Integer(Len) - 1;
  FLastY := Y;
end;

function TScanLine.IsReady(Y: Integer): Integer;
begin
  Result := Ord((FNumSpans <> 0) and ((Y xor FLastY) <> 0));
end;

procedure TScanLine.ResetSpans;
begin
  FLastX := $7FFF;
  FLastY := $7FFF;
  FCurCount := FCounts;
  FCurStartPtr := FStartPtrs;
  FNumSpans := 0;
end;


{ TOutline }

constructor TOutline.Create;
begin
  inherited Create;

  FCurCellPtr := nil;

  FMin.X := $7FFFFFFF;
  FMin.Y := $7FFFFFFF;
  FMax.X := -$7FFFFFFF;
  FMax.Y := -$7FFFFFFF;
  FFlags := [ofSortRequired];
  SetCell(FCurCell, $7FFF, $7FFF);
end;

destructor TOutline.Destroy;
var
  Ptr: PPCell;
begin
  FreeMem(FSortedCells);
  if FNumBlocks <> 0 then
  begin
    Ptr := PPCell(Cardinal(FCells) + (FNumBlocks - 1) * SizeOf(PCell));
    while FNumBlocks <> 0 do
    begin
      FreeMem(Ptr^);
      Dec(Ptr);

      Dec(FNumBlocks);
    end;
    FreeMem(FCells);
  end;

  inherited Destroy;
end;

procedure TOutline.Reset;
begin
  FNumCells := 0;
  FCurBlock := 0;
  InternalReset;
end;

procedure TOutline.InternalReset;
begin
  FMin.X := $7FFFFFFF;
  FMin.Y := $7FFFFFFF;
  FMax.X := -$7FFFFFFF;
  FMax.Y := -$7FFFFFFF;
  FFlags := [ofSortRequired];
  SetCell(FCurCell, $7FFF, $7FFF);
end;

procedure TOutline.AddCurCell;
begin
  if FCurCell.Area or FCurCell.Cover <> 0 then
  begin
    if FNumCells and CCellBlockMask = 0 then
    begin
      if FNumBlocks >= CCellBlockLimit then
        Exit;
      AllocateBlock;
    end;
    FCurCellPtr^ := FCurCell;
    Inc(FCurCellPtr);
    Inc(FNumCells);
  end;
end;

procedure TOutline.AllocateBlock;
var
  NewCells: PPCell;
begin
  if FCurBlock >= FNumBlocks then
  begin
    if FNumBlocks >= FMaxBlocks then
    begin
      GetMem(NewCells, (FMaxBlocks + CCellBlockPool) * SizeOf(PCell));
      if Assigned(FCells) then
      begin
        Move(FCells^, NewCells^, FMaxBlocks * SizeOf(PCell));
        FreeMem(FCells);
      end;
      FCells := NewCells;
      Inc(FMaxBlocks, CCellBlockPool);
    end;
    GetMem(PPCell(Cardinal(FCells) + FNumBlocks * SizeOf(PCell))^,
      Cardinal(CCellBlockSize) * SizeOf(TCell));
    Inc(FNumBlocks);
  end;
  FCurCellPtr := PPCell(Cardinal(FCells) + FCurBlock * SizeOf(PCell))^;
  Inc(FCurBlock);
end;

function TOutline.GetCells: PPCell;
begin
  if ofNotClosed in FFlags then
  begin
    LineTo(FClose.X, FClose.Y);
    FFlags := FFlags - [ofNotClosed];
  end;

  // Perform sort only the first time.
  if ofSortRequired in FFlags then
  begin
    AddCurCell;
    if FNumCells = 0 then
    begin
      Result := nil;
      Exit;
    end;
    SortCells;
    FFlags := FFlags - [ofSortRequired];
  end;
  Result := FSortedCells;
end;

procedure TOutline.LineTo(X, Y: Integer);
var
  C: Integer;
begin
  if (ofSortRequired in FFlags) and (((FCur.X xor X) or (FCur.Y xor Y)) <> 0) then
  begin
    C := SAR_8(FCur.X);
    if C < FMin.X then FMin.X := C;
    Inc(C);
    if C > FMax.X then FMax.X := C;

    C := SAR_8(X);
    if C < FMin.X then FMin.X := C;
    Inc(C);
    if C > FMax.X then FMax.X := C;

    RenderLine(FCur.X, FCur.Y, X, Y);
    FCur.X := X;
    FCur.Y := Y;
    FFlags := FFlags + [ofNotClosed];
  end;
end;

procedure TOutline.MoveTo(X, Y: Integer);
begin
  if not (ofSortRequired in FFlags) then  //-7468, -6124, -6124, -4836
    Reset;
  if ofNotClosed in FFlags then
    LineTo(FClose.X, FClose.Y);

  SetCurCell(SAR_8(X), SAR_8(Y));

  FCur.X   := X;
  FClose.X := X;
  FCur.Y   := Y;
  FClose.Y := Y;
end;

procedure TOutline.RenderLine(X1, Y1, X2, Y2: Integer);
var
  EY1, EY2, FY1, FY2, Dx, Dy, XFrom, XTo, P, Rem, AMod, Lift: Integer;
  Delta, First, Incr, EX, TwoFx, Area: Integer;
begin
  EY1 := SAR_8(Y1);
  EY2 := SAR_8(Y2);
  FY1 := Y1 and CPolyBaseMask;
  FY2 := Y2 and CPolyBaseMask;

  if EY1 <  FMin.Y then FMin.Y := EY1;
  if EY1 >= FMax.Y then FMax.Y := EY1 + 1;
  if EY2 < FMin.Y then FMin.Y := EY2;
  if EY2 >= FMax.Y then FMax.Y := EY2 + 1;

  Dx := X2 - X1;
  Dy := Y2 - Y1;

  // everything is on a single scanline
  if EY1 = EY2 then
  begin
    RenderScanLine(EY1, X1, FY1, X2, FY2);
    Exit;
  end;

  // Vertical line - we have to calculate start and end cells, and then -
  // the common values of the area and coverage for all cells of the line.
  // We know exactly there's only one cell, so, we don't have to call
  // RenderScanline().
  Incr := 1;
  if Dx = 0 then
  begin
    EX := SAR_8(X1);
    TwoFx := (X1 - (EX shl CPolyBaseShift)) shl 1;

    First := CPolyBaseSize;
    if Dy < 0 then
    begin
      First := 0;
      Incr  := -1;
    end;

    Delta := First - FY1;
    Inc(FCurCell.Cover, Delta);
    Inc(FCurCell.Area, TwoFx * Delta);

    Inc(EY1, Incr);
    SetCurCell(EX, EY1);

    Delta := First + First - CPolyBaseSize;
    Area := TwoFx * Delta;
    while EY1 <> EY2 do
    begin
      FCurCell.Cover := Delta;
      FCurCell.Area := Area;
      Inc(EY1, Incr);
      SetCurCell(EX, EY1);
    end;
    Delta := FY2 - CPolyBaseSize + First;
    Inc(FCurCell.Cover, Delta);
    Inc(FCurCell.Area, TwoFx * Delta);
    Exit;
  end;

  // ok, we have to render several scanlines
  P := (CPolyBaseSize - FY1) * Dx;
  First := CPolyBaseSize;

  if Dy < 0 then
  begin
    P     := FY1 * Dx;
    First := 0;
    Incr  := -1;
    Dy    := -Dy;
  end;

  Delta := P div Dy;
  AMod  := P mod Dy;

  if AMod < 0 then
  begin
    Dec(Delta);
    Inc(AMod, Dy);
  end;

  XFrom := X1 + Delta;
  RenderScanLine(EY1, X1, FY1, XFrom, First);

  Inc(EY1, Incr);
  SetCurCell(SAR_8(XFrom), EY1);

  if EY1 <> EY2 then
  begin
    P     := CPolyBaseSize * Dx;
    Lift  := P div Dy;
    Rem   := P mod Dy;

    if Rem < 0 then
    begin
      Dec(Lift);
      Inc(Rem, Dy);
    end;
    Dec(AMod, Dy);

    while EY1 <> EY2 do
    begin
      Delta := Lift;
      Inc(AMod, Rem);
      if AMod >= 0 then
      begin
        Dec(AMod, Dy);
        Inc(Delta);
      end;

      XTo := XFrom + Delta;
      RenderScanLine(EY1, XFrom, CPolyBaseSize - First, XTo, First);
      XFrom := XTo;

      Inc(EY1, Incr);
      SetCurCell(SAR_8(XFrom), EY1);
    end;
  end;

  RenderScanLine(EY1, XFrom, CPolyBaseSize - First, X2, FY2);
end;

procedure TOutline.RenderScanLine(EY, X1, Y1, X2, Y2: Integer);
var
  EX1, EX2, FX1, FX2, Delta, P, First, Dx, Incr, Lift, AMod, Rem: Integer;
begin
  EX1 := SAR_8(X1);
  EX2 := SAR_8(X2);
  FX1 := X1 and CPolyBaseMask;
  FX2 := X2 and CPolyBaseMask;

  // trivial case. Happens often
  if Y1 = Y2 then
  begin
    SetCurCell(EX2, EY);
    Exit;
  end;

  // everything is located in a single cell.  That is easy!
  if EX1 = EX2 then
  begin
    Delta := Y2 - Y1;
    Inc(FCurCell.Cover, Delta);
    Inc(FCurCell.Area, (FX1 + FX2) * Delta);
    Exit;
  end;

  // ok, we'll have to render a run of adjacent cells on the same scanline...
  P := (CPolyBaseSize - FX1) * (Y2 - Y1);
  First := CPolyBaseSize;
  Incr := 1;

  Dx := X2 - X1;

  if Dx < 0 then
  begin
    P := FX1 * (Y2 - Y1);
    First := 0;
    Incr  := -1;
    Dx := -Dx;
  end;

  Delta := P div Dx;
  AMod  := P mod Dx;

  if AMod < 0 then
  begin
    Dec(Delta);
    Inc(AMod, Dx);
  end;

  Inc(FCurCell.Cover, Delta);
  Inc(FCurCell.Area, (FX1 + First) * Delta);
  Inc(EX1, Incr);
  SetCurCell(EX1, EY);
  Inc(Y1, Delta);

  if EX1 <> EX2 then
  begin
    P := CPolyBaseSize * (Y2 - Y1 + Delta);
    Lift := P div Dx;
    Rem := P mod Dx;

    if Rem < 0 then
    begin
      Dec(Lift);
      Inc(Rem, Dx);
    end;

    Dec(AMod, Dx);

    while EX1 <> EX2 do
    begin
      Delta := Lift;
      Inc(AMod, Rem);
      if AMod >= 0 then
      begin
        Dec(AMod, Dx);
        Inc(Delta);
      end;

      Inc(FCurCell.Cover, Delta);
      Inc(FCurCell.Area, CPolyBaseSize * Delta);
      Inc(Y1, Delta);
      Inc(EX1, Incr);
      SetCurCell(EX1, EY);
    end;
  end;

  Delta := Y2 - Y1;
  Inc(FCurCell.Cover, Delta);
  Inc(FCurCell.Area, (FX2 + CPolyBaseSize - First) * Delta);
end;

procedure TOutline.SetCurCell(X, Y: Integer);
begin
  if FCurCell.PackedCoord <> (Y shl 16) + X then
  begin
    AddCurCell;
    SetCell(FCurCell, X, Y);
  end;
end;

procedure TOutline.SortCells;
var
  SortedPtr, BlockPtr: PPCell;
  CellPtr: PCell;
  NB, I: Cardinal;
begin
  if FNumCells = 0 then
    Exit;

  if FNumCells > FSortedSize then
  begin
    FreeMem(FSortedCells);
    FSortedSize := FNumCells;
    GetMem(FSortedCells, (FNumCells + 1) * SizeOf(PCell));
  end;

  SortedPtr := FSortedCells;
  BlockPtr := FCells;

  NB := FNumCells shr CCellBlockShift;

  while NB <> 0 do
  begin
    Dec(NB);

    CellPtr := BlockPtr^;
    Inc(BlockPtr);
    I := CCellBlockSize;
    while I <> 0 do
    begin
      Dec(I);

      SortedPtr^ := CellPtr;
      Inc(SortedPtr);
      Inc(CellPtr);
    end;
  end;

  CellPtr := BlockPtr^;
  I := FNumCells and CCellBlockMask;
  while I <> 0 do
  begin
    Dec(I);

    SortedPtr^ := CellPtr;
    Inc(SortedPtr);
    Inc(CellPtr);
  end;
  PPCell(Cardinal(FSortedCells) + FNumCells * SizeOf(PCell))^ := nil;

  QSortCells(FSortedCells, FNumCells);
end;


{ TPolygonRenderer32AggLite }

procedure TPolygonRenderer32AggLite.Render(CellsPtr: Pointer; MinX, MaxX: Integer);
var
  X, Y, Cover, Alpha, Area, Coord: Integer;
  Cells: PPCell absolute CellsPtr;
  CurCell, StartCell: PCell;
  ScanLine: TScanLine;

  procedure RenderSpan;
  var
    NumSpans: Cardinal;
    BaseX: Integer;
    Row: PColor32Array;
    CurX: Integer;
    Covers: PColor32;
    NumPix: Integer;
    BaseCovers: Pointer;
    CurCount: PWord;
    CurStartPtr: PPColor32;

  begin
    NumSpans := ScanLine.NumSpans;
    BaseX := ScanLine.BaseX;
    Row := Bitmap.ScanLine[ScanLine.Y];

    BaseCovers := ScanLine.CoversPtr;
    CurCount := ScanLine.CountsPtr;
    CurStartPtr := ScanLine.StartPtrs;

    if Assigned(Filler) then
    repeat
      Dec(NumSpans);
      Inc(CurCount);
      Inc(CurStartPtr);
      {$IFDEF FPC}
      CurX := (PtrInt(CurStartPtr^) - PtrInt(BaseCovers)) div SizeOf(TColor32) + BaseX;
      {$ELSE}
      {$IFDEF HAS_NATIVEINT}
      CurX := (NativeInt(CurStartPtr^) - NativeInt(BaseCovers)) div SizeOf(TColor32) + BaseX;
      {$ELSE}
      CurX := (Integer(CurStartPtr^) - Integer(BaseCovers)) div SizeOf(TColor32) + BaseX;
      {$ENDIF}
      {$ENDIF}
      Covers := CurStartPtr^;
      NumPix := CurCount^;

      if CurX < 0 then
      begin
        Inc(NumPix, CurX);
        if NumPix <= 0 then
          Continue;
        Dec(Covers, CurX);
        CurX := 0;
      end;
      if CurX + NumPix >= Bitmap.Width then
      begin
        NumPix := Bitmap.Width - CurX;
        if NumPix <= 0 then
          Continue;
      end;

      Filler.FillLine(@Row^[CurX], CurX, ScanLine.Y, NumPix, Covers, Bitmap.CombineMode);
    until NumSpans = 0
    else
    repeat
      Dec(NumSpans);
      Inc(CurCount);
      Inc(CurStartPtr);
      {$IFDEF FPC}
      CurX := (PtrInt(CurStartPtr^) - PtrInt(BaseCovers)) div SizeOf(TColor32) + BaseX;
      {$ELSE}
      {$IFDEF HAS_NATIVEINT}
      CurX := (NativeInt(CurStartPtr^) - NativeInt(BaseCovers)) div SizeOf(TColor32) + BaseX;
      {$ELSE}
      CurX := (Integer(CurStartPtr^) - Integer(BaseCovers)) div SizeOf(TColor32) + BaseX;
      {$ENDIF}
      {$ENDIF}

      Covers := CurStartPtr^;
      NumPix := CurCount^;

      if CurX < 0 then
      begin
        Inc(NumPix, CurX);
        if NumPix <= 0 then
          Continue;
        Dec(Covers, CurX);
        CurX := 0;
      end;
      if CurX + NumPix >= Bitmap.Width then
      begin
        NumPix := Bitmap.Width - CurX;
        if NumPix <= 0 then
          Continue;
      end;

      FillSpan(@Row^[CurX], PColor32(Covers), NumPix, Color);
    until NumSpans = 0;
    EMMS;
  end;

begin
  ScanLine := TScanLine.Create(MinX, MaxX);  // -32, 64
  try
    Cover := 0;
    CurCell := Cells^;
    Inc(Cells);
    while True do
    begin
      StartCell := CurCell;

      Coord := CurCell^.Pnt.PackedCoord;
      X := CurCell^.Pnt.X;
      Y := CurCell^.Pnt.Y;

      Area := StartCell^.Area;
      Inc(Cover, StartCell^.Cover);

      CurCell := Cells^;
      Inc(Cells);
      while Assigned(CurCell) do
      begin
        if CurCell^.Pnt.PackedCoord <> Coord then
          Break;
        Inc(Area, CurCell^.Area);
        Inc(Cover, CurCell^.Cover);

        CurCell := Cells^;
        Inc(Cells);
      end;

      if Area <> 0 then
      begin
        Alpha := CalculateAlpha(Fillmode, (Cover shl (CPolyBaseShift + 1)) - Area);
        if Alpha <> 0 then
        begin
          if ScanLine.IsReady(Y) <> 0 then
          begin
            if (ScanLine.Y >= 0) and (ScanLine.Y < Bitmap.Height) then
              RenderSpan;
            ScanLine.ResetSpans;
          end;
          ScanLine.AddCell(X, Y, GAMMA_ENCODING_TABLE[Alpha]);
        end;
        Inc(X);
      end;

      if not Assigned(CurCell) then
        Break;

      if CurCell^.Pnt.X > X then
      begin
        Alpha := CalculateAlpha(Fillmode, Cover shl (CPolyBaseShift + 1));
        if Alpha <> 0 then
        begin
          if ScanLine.IsReady(Y) <> 0 then
          begin
            if (ScanLine.Y >= 0) and (ScanLine.Y < Bitmap.Height) then
              RenderSpan;
            ScanLine.ResetSpans;
          end;
          ScanLine.AddSpan(X, Y, CurCell^.Pnt.X - X, GAMMA_ENCODING_TABLE[Alpha]);
        end;
      end;
    end;

    with ScanLine do
      if (NumSpans <> 0) and (Y >= 0) and (Y < Bitmap.Height) then
        RenderSpan;
  finally
    ScanLine.Free;
  end;
end;

type
  TBitmap32Access = class(TBitmap32);

procedure TPolygonRenderer32AggLite.PolygonFS(
  const Points: TArrayOfFloatPoint; const ClipRect: TFloatRect);
var
  I: Integer;
  Cells: PPCell;
  OutLine: TOutline;
  APoints: TArrayOfFloatPoint;
  R: TFloatRect;
begin
  R := ClipRect;
  InflateRect(R, 0.05, 0.05);
  APoints := ClipPolygon (Points, R);

  OutLine := TOutline.Create;
  try
    OutLine.Reset;
    OutLine.MoveTo(Fixed8(APoints[0].X), Fixed8(APoints[0].Y));
    for I := 1 to High(APoints) do
      OutLine.LineTo(Fixed8(APoints[I].X), Fixed8(APoints[I].Y));

    // get cells and check count
    Cells := OutLine.Cells;
    if OutLine.NumCells = 0 then
      Exit;

    if Assigned(Filler) then
    begin
      // call begin rendering of assigned filler
      Filler.BeginRendering;

      Render(Cells, OutLine.MinX, OutLine.MaxX);

      // rendering done, call end rendering of assigned filler
      Filler.EndRendering;
    end
    else
      Render(Cells, OutLine.MinX, OutLine.MaxX);

  {$IFDEF CHANGENOTIFICATIONS}
    if TBitmap32Access(Bitmap).UpdateCount = 0 then
      if Length(APoints) > 0 then
        Bitmap.Changed(MakeRect(OutLine.MinX, OutLine.MinY, OutLine.MaxX,
          OutLine.MaxY));
  {$ENDIF}
  finally
    SetLength(APoints, 0);
    OutLine.Free;
  end;
end;

procedure TPolygonRenderer32AggLite.PolyPolygonFS(
  const Points: TArrayOfArrayOfFloatPoint; const ClipRect: TFloatRect);
var
  I, J: Integer;
  Cells: PPCell;
  OutLine: TOutline;
  Bounds: TRect;
  APoints: TArrayOfArrayOfFloatPoint;
  R: TFloatRect;
  FirstValid: integer;
begin
  if Length(Points) = 0 then
    Exit;

  APoints := Points;
  // temporary fix for floating point rounding errors - corr. - to + by pws
  R := ClipRect;
  InflateRect(R, 0.05, 0.05);
  FirstValid := -1;
  for i := 0 to High(APoints) do
  begin
    APoints[i] := ClipPolygon(Points[I], R);
    if (FirstValid = -1) and (Length(APoints[i]) > 0) then
      FirstValid := i;
  end;

  if (FirstValid = -1) then
    exit; // All were clipped

  OutLine := TOutline.Create;
  try
    OutLine.Reset;
    OutLine.MoveTo(Fixed8(APoints[FirstValid, 0].X), Fixed8(APoints[FirstValid, 0].Y));
    for I := 1 to High(APoints[FirstValid]) do
      OutLine.LineTo(Fixed8(APoints[FirstValid, I].X), Fixed8(APoints[FirstValid, I].Y));

    Bounds := MakeRect(OutLine.MinX, OutLine.MinY, OutLine.MaxX, OutLine.MaxY);

    for J := FirstValid+1 to High(APoints) do
    begin
      if (Length(APoints[J]) = 0) then
        continue;
      OutLine.MoveTo(Fixed8(APoints[J, 0].X), Fixed8(APoints[J, 0].Y));
      for I := 1 to High(APoints[J]) do
        OutLine.LineTo(Fixed8(APoints[J, I].X), Fixed8(APoints[J, I].Y));

      Bounds.Left := Min(Bounds.Left, OutLine.MinX);
      Bounds.Right := Max(Bounds.Right, OutLine.MaxX);
      Bounds.Top := Min(Bounds.Top, OutLine.MinY);
      Bounds.Bottom := Max(Bounds.Bottom, OutLine.MaxY);
    end;

    // get cells and check count
    Cells := OutLine.Cells;
    if OutLine.NumCells = 0 then
      Exit;

    if Assigned(Filler) then
    begin
      // call begin rendering of assigned filler
      Filler.BeginRendering;

      Render(Cells, Bounds.Left, Bounds.Right);

      // rendering done, call end rendering of assigned filler
      Filler.EndRendering;
    end
    else
      Render(Cells, Bounds.Left, Bounds.Right);

{$IFDEF CHANGENOTIFICATIONS}
    if TBitmap32Access(Bitmap).UpdateCount = 0 then
      for I := 0 to High(APoints) do
        if Length(APoints[I]) > 0 then
          Bitmap.Changed(Bounds);
{$ENDIF}
  finally
    OutLine.Free;
    SetLength(APoints, 0);
  end;
end;

const
  FID_FILLSPAN = 0;

procedure RegisterBindings;
begin
  BlendRegistry := NewRegistry('GR32_PolygonsAggLite bindings');
  BlendRegistry.RegisterBinding(FID_FILLSPAN, @@FILLSPAN);

  // pure pascal
  BlendRegistry.Add(FID_FILLSPAN, @FILLSPAN_Pas);

{$IFNDEF PUREPASCAL}
  BlendRegistry.Add(FID_FILLSPAN, @FILLSPAN_ASM, []);
{$IFNDEF OMIT_MMX}
  BlendRegistry.Add(FID_FILLSPAN, @FILLSPAN_MMX, [ciMMX]);
{$ENDIF}

{$IFNDEF OMIT_SSE2}
  BlendRegistry.Add(FID_FILLSPAN, @FILLSPAN_SSE2, [ciSSE2]);
{$ENDIF}
{$ENDIF}

  BlendRegistry.RebindAll;
end;

initialization
  RegisterPolygonRenderer(TPolygonRenderer32AggLite);
  RegisterBindings;

finalization

end.
