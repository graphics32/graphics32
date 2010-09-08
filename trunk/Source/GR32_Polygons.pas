unit GR32_Polygons;

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
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2009
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Andre Beckedorf <Andre@metaException.de>
 *   Mattias Andersson <mattias@centaurix.com>
 *   Peter Larson <peter@larson.net>
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
{$IFDEF FPC}
{$ELSE}
  Windows,
{$ENDIF}
  Classes, SysUtils, GR32, GR32_LowLevel, GR32_Blend, GR32_Transforms,
  GR32_Resamplers, GR32_Math;

{ Polylines }

procedure PolylineTS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  Color: TColor32; Closed: Boolean = False; Transformation: TTransformation = nil);
procedure PolylineAS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  Color: TColor32; Closed: Boolean = False; Transformation: TTransformation = nil);
procedure PolylineXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  Color: TColor32; Closed: Boolean = False; Transformation: TTransformation = nil);
procedure PolylineXSP(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  Closed: Boolean = False; Transformation: TTransformation = nil);

procedure PolyPolylineTS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Color: TColor32; Closed: Boolean = False; Transformation: TTransformation = nil);
procedure PolyPolylineAS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Color: TColor32; Closed: Boolean = False; Transformation: TTransformation = nil);
procedure PolyPolylineXS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Color: TColor32; Closed: Boolean = False; Transformation: TTransformation = nil);
procedure PolyPolylineXSP(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Closed: Boolean = False; Transformation: TTransformation = nil);

{ Polygons }

type
  TPolyFillMode = (pfAlternate, pfWinding);
  TAntialiasMode = (am32times, am16times, am8times, am4times, am2times, amNone);

  TFillLineEvent = procedure(Dst: PColor32; DstX, DstY, Length: Integer; AlphaValues: PColor32) of object;

  TCustomPolygonFiller = class
  protected
    function GetFillLine: TFillLineEvent; virtual; abstract;
  public
    property FillLine: TFillLineEvent read GetFillLine;
  end;

const
  DefaultAAMode = am8times; // Use 54 levels of transparency for antialiasing.

procedure PolygonTS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  Color: TColor32; Mode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
procedure PolygonTS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  FillLineCallback: TFillLineEvent; Mode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
procedure PolygonTS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  Filler: TCustomPolygonFiller; Mode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;

procedure PolygonXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  Color: TColor32; Mode: TPolyFillMode = pfAlternate;
  const AAMode: TAntialiasMode = DefaultAAMode; Transformation: TTransformation = nil); overload;
procedure PolygonXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  FillLineCallback: TFillLineEvent; Mode: TPolyFillMode = pfAlternate;
  const AAMode: TAntialiasMode = DefaultAAMode; Transformation: TTransformation = nil); overload;
procedure PolygonXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  Filler: TCustomPolygonFiller; Mode: TPolyFillMode = pfAlternate;
  const AAMode: TAntialiasMode = DefaultAAMode; Transformation: TTransformation = nil); overload;

procedure PolyPolygonTS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Color: TColor32; Mode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
procedure PolyPolygonTS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  FillLineCallback: TFillLineEvent; Mode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
procedure PolyPolygonTS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Filler: TCustomPolygonFiller; Mode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;

procedure PolyPolygonXS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  FillLineCallback: TFillLineEvent; Mode: TPolyFillMode = pfAlternate;
  const AAMode: TAntialiasMode = DefaultAAMode; Transformation: TTransformation = nil); overload;
procedure PolyPolygonXS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Color: TColor32; Mode: TPolyFillMode = pfAlternate;
  const AAMode: TAntialiasMode = DefaultAAMode; Transformation: TTransformation = nil); overload;
procedure PolyPolygonXS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint;
  Filler: TCustomPolygonFiller; Mode: TPolyFillMode = pfAlternate;
  const AAMode: TAntialiasMode = DefaultAAMode; Transformation: TTransformation = nil); overload;

function PolygonBounds(const Points: TArrayOfFixedPoint; Transformation: TTransformation = nil): TFixedRect;
function PolyPolygonBounds(const Points: TArrayOfArrayOfFixedPoint; Transformation: TTransformation = nil): TFixedRect;

function PtInPolygon(const Pt: TFixedPoint; const Points: TArrayOfFixedPoint): Boolean;

{ TPolygon32 }
{ TODO : Bezier Curves, and QSpline curves for TrueType font rendering }
{ TODO : Check if QSpline is compatible with Type1 fonts }
type
  TPolygon32 = class(TThreadPersistent)
  private
    FAntialiased: Boolean;
    FClosed: Boolean;
    FFillMode: TPolyFillMode;
    FNormals: TArrayOfArrayOfFixedPoint;
    FPoints: TArrayOfArrayOfFixedPoint;
    FAntialiasMode: TAntialiasMode;
  protected
    procedure BuildNormals;
    procedure CopyPropertiesTo(Dst: TPolygon32); virtual;
    procedure AssignTo(Dst: TPersistent); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Add(const P: TFixedPoint);
    procedure AddPoints(var First: TFixedPoint; Count: Integer);
    function  ContainsPoint(const P: TFixedPoint): Boolean;
    procedure Clear;
    function  Grow(const Delta: TFixed; EdgeSharpness: Single = 0): TPolygon32;

    procedure Draw(Bitmap: TCustomBitmap32; OutlineColor, FillColor: TColor32; Transformation: TTransformation = nil); overload;
    procedure Draw(Bitmap: TCustomBitmap32; OutlineColor: TColor32; FillCallback: TFillLineEvent; Transformation: TTransformation = nil); overload;
    procedure Draw(Bitmap: TCustomBitmap32; OutlineColor: TColor32; Filler: TCustomPolygonFiller; Transformation: TTransformation = nil); overload;

    procedure DrawEdge(Bitmap: TCustomBitmap32; Color: TColor32; Transformation: TTransformation = nil);

    procedure DrawFill(Bitmap: TCustomBitmap32; Color: TColor32; Transformation: TTransformation = nil); overload;
    procedure DrawFill(Bitmap: TCustomBitmap32; FillCallback: TFillLineEvent; Transformation: TTransformation = nil); overload;
    procedure DrawFill(Bitmap: TCustomBitmap32; Filler: TCustomPolygonFiller; Transformation: TTransformation = nil); overload;

    procedure NewLine;
    procedure Offset(const Dx, Dy: TFixed);
    function  Outline: TPolygon32;
    procedure Transform(Transformation: TTransformation);
    function GetBoundingRect: TFixedRect;

    property Antialiased: Boolean read FAntialiased write FAntialiased;
    property AntialiasMode: TAntialiasMode read FAntialiasMode write FAntialiasMode;
    property Closed: Boolean read FClosed write FClosed;
    property FillMode: TPolyFillMode read FFillMode write FFillMode;

    property Normals: TArrayOfArrayOfFixedPoint read FNormals write FNormals;
    property Points: TArrayOfArrayOfFixedPoint read FPoints write FPoints;
  end;

  { TBitmapPolygonFiller }
  TBitmapPolygonFiller = class(TCustomPolygonFiller)
  private
    FPattern: TCustomBitmap32;
    FOffsetY: Integer;
    FOffsetX: Integer;
  protected
    function GetFillLine: TFillLineEvent; override;
    procedure FillLineOpaque(Dst: PColor32; DstX, DstY, Length: Integer; AlphaValues: PColor32);
    procedure FillLineBlend(Dst: PColor32; DstX, DstY, Length: Integer; AlphaValues: PColor32);
    procedure FillLineBlendMasterAlpha(Dst: PColor32; DstX, DstY, Length: Integer; AlphaValues: PColor32);
    procedure FillLineCustomCombine(Dst: PColor32; DstX, DstY, Length: Integer; AlphaValues: PColor32);
  public
    property Pattern: TCustomBitmap32 read FPattern write FPattern;
    property OffsetX: Integer read FOffsetX write FOffsetX;
    property OffsetY: Integer read FOffsetY write FOffsetY;
  end;

  { TSamplerFiller }
  TSamplerFiller = class(TCustomPolygonFiller)
  private
    FSampler: TCustomSampler;
    FGetSample: TGetSampleInt;
    procedure SetSampler(const Value: TCustomSampler);
  protected
    function GetFillLine: TFillLineEvent; override;
    procedure SampleLineOpaque(Dst: PColor32; DstX, DstY, Length: Integer; AlphaValues: PColor32);

    property Sampler: TCustomSampler read FSampler write SetSampler;
  end;
    
implementation

uses Math;

type
  TCustomBitmap32Access = class(TCustomBitmap32);
  TShiftFunc = function(Value: Integer): Integer;  // needed for antialiasing to speed things up
// These are for edge scan info. Note, that the most significant bit of the
// edge in a scan line is used for winding (edge direction) info.

  TEdgePoint = Integer;

  PEdgePoints = ^TEdgePoints;
  TEdgePoints = array [0..MaxListSize-1] of TEdgePoint;

  PScanLine = ^TScanLine;
  TScanLine = record
    Count: Integer;
    EdgePoints: PEdgePoints;
    EdgePointsLength: Integer;
  end;

  TScanLines = array of TScanLine;

const
  AA_LINES: Array[TAntialiasMode] of Integer = (32, 16, 8, 4, 2, 1);
  AA_SHIFT: Array[TAntialiasMode] of Integer = (5, 4, 3, 2, 1, 0);
  AA_MULTI: Array[TAntialiasMode] of Integer = (65, 273, 1167, 5460, 32662, 0);
  AA_SAR:   Array[TAntialiasMode] of TShiftFunc = (SAR_11, SAR_12, SAR_13, SAR_14, SAR_15, nil);
  AA_BitShift: Array[TAntialiasMode] of Integer = (11, 12, 13, 14, 15, 16);

{ POLYLINES }

procedure PolylineTS(
  Bitmap: TCustomBitmap32;
  const Points: TArrayOfFixedPoint;
  Color: TColor32;
  Closed: Boolean;
  Transformation: TTransformation);
var
  I, Count: Integer;
  DoAlpha: Boolean;
begin
  Count := Length(Points);

  if (Count = 1) and Closed then
    if Assigned(Transformation) then
      with Transformation.Transform(Points[0]) do
        Bitmap.SetPixelTS(FixedRound(X), FixedRound(Y), Color)
    else
      with Points[0] do
        Bitmap.SetPixelTS(FixedRound(X), FixedRound(Y), Color);

  if Count < 2 then Exit;
  DoAlpha := Color and $FF000000 <> $FF000000;
  Bitmap.BeginUpdate;
  Bitmap.PenColor := Color;

  if Assigned(Transformation) then
  begin
    with Transformation.Transform(Points[0]) do Bitmap.MoveTo(FixedRound(X), FixedRound(Y));
    if DoAlpha then
      for I := 1 to Count - 1 do
        with Transformation.Transform(Points[I]) do
          Bitmap.LineToTS(FixedRound(X), FixedRound(Y))
    else
      for I := 1 to Count - 1 do
        with Transformation.Transform(Points[I]) do
          Bitmap.LineToS(FixedRound(X), FixedRound(Y));

    if Closed then with Transformation.Transform(Points[0]) do
      if DoAlpha then
        Bitmap.LineToTS(FixedRound(X), FixedRound(Y))
      else
        Bitmap.LineToS(FixedRound(X), FixedRound(Y));
  end
  else
  begin
    with Points[0] do Bitmap.MoveTo(FixedRound(X), FixedRound(Y));
    if DoAlpha then
      for I := 1 to Count - 1 do
        with Points[I] do
          Bitmap.LineToTS(FixedRound(X), FixedRound(Y))
    else
      for I := 1 to Count - 1 do
        with Points[I] do
          Bitmap.LineToS(FixedRound(X), FixedRound(Y));

    if Closed then with Points[0] do
      if DoAlpha then
        Bitmap.LineToTS(FixedRound(X), FixedRound(Y))
      else
        Bitmap.LineToS(FixedRound(X), FixedRound(Y));
  end;

  Bitmap.EndUpdate;
  Bitmap.Changed;
end;

procedure PolylineAS(
  Bitmap: TCustomBitmap32;
  const Points: TArrayOfFixedPoint;
  Color: TColor32;
  Closed: Boolean;
  Transformation: TTransformation);
var
  I, Count: Integer;
begin
  Count := Length(Points);
  if (Count = 1) and Closed then
    if Assigned(Transformation) then
      with Transformation.Transform(Points[0]) do
        Bitmap.SetPixelTS(FixedRound(X), FixedRound(Y), Color)
    else
      with Points[0] do
        Bitmap.SetPixelTS(FixedRound(X), FixedRound(Y), Color);

  if Count < 2 then Exit;
  Bitmap.BeginUpdate;
  Bitmap.PenColor := Color;

  if Assigned(Transformation) then
  begin
    with Transformation.Transform(Points[0]) do Bitmap.MoveTo(FixedRound(X), FixedRound(Y));
    for I := 1 to Count - 1 do
      with Transformation.Transform(Points[I]) do
        Bitmap.LineToAS(FixedRound(X), FixedRound(Y));
    if Closed then with Transformation.Transform(Points[0]) do Bitmap.LineToAS(FixedRound(X), FixedRound(Y));
  end
  else
  begin
    with Points[0] do Bitmap.MoveTo(FixedRound(X), FixedRound(Y));
    for I := 1 to Count - 1 do
      with Points[I] do
        Bitmap.LineToAS(FixedRound(X), FixedRound(Y));
    if Closed then with Points[0] do Bitmap.LineToAS(FixedRound(X), FixedRound(Y));
  end;

  Bitmap.EndUpdate;
  Bitmap.Changed;
end;

procedure PolylineXS(
  Bitmap: TCustomBitmap32;
  const Points: TArrayOfFixedPoint;
  Color: TColor32;
  Closed: Boolean;
  Transformation: TTransformation);
var
  I, Count: Integer;
begin
  Count := Length(Points);
  if (Count = 1) and Closed then
    if Assigned(Transformation) then
      with Transformation.Transform(Points[0]) do Bitmap.PixelXS[X, Y] := Color
    else
      with Points[0] do Bitmap.PixelXS[X, Y] := Color;

  if Count < 2 then Exit;
  Bitmap.BeginUpdate;
  Bitmap.PenColor := Color;

  if Assigned(Transformation) then
  begin
    with Transformation.Transform(Points[0]) do Bitmap.MoveToX(X, Y);
    for I := 1 to Count - 1 do with Transformation.Transform(Points[I]) do Bitmap.LineToXS(X, Y);
    if Closed then with Transformation.Transform(Points[0]) do Bitmap.LineToXS(X, Y);
  end
  else
  begin
    with Points[0] do Bitmap.MoveToX(X, Y);
    for I := 1 to Count - 1 do with Points[I] do Bitmap.LineToXS(X, Y);
    if Closed then with Points[0] do Bitmap.LineToXS(X, Y);
  end;

  Bitmap.EndUpdate;
  Bitmap.Changed;
end;

procedure PolylineXSP(
  Bitmap: TCustomBitmap32;
  const Points: TArrayOfFixedPoint;
  Closed: Boolean;
  Transformation: TTransformation);
var
  I, Count: Integer;
begin
  Count := Length(Points);
  if Count < 2 then Exit;
  Bitmap.BeginUpdate;
  if Assigned(Transformation) then
  begin
    with Transformation.Transform(Points[0]) do Bitmap.MoveToX(X, Y);
    for I := 1 to Count - 1 do with Transformation.Transform(Points[I]) do Bitmap.LineToXSP(X, Y);
    if Closed then with Transformation.Transform(Points[0]) do Bitmap.LineToXSP(X, Y);
  end
  else
  begin
    with Points[0] do Bitmap.MoveToX(X, Y);
    for I := 1 to Count - 1 do with Points[I] do Bitmap.LineToXSP(X, Y);
    if Closed then with Points[0] do Bitmap.LineToXSP(X, Y);
  end;

  Bitmap.EndUpdate;
  Bitmap.Changed;
end;

procedure PolyPolylineTS(
  Bitmap: TCustomBitmap32;
  const Points: TArrayOfArrayOfFixedPoint;
  Color: TColor32;
  Closed: Boolean;
  Transformation: TTransformation);
var
  I: Integer;
begin
  for I := 0 to High(Points) do PolylineTS(Bitmap, Points[I], Color, Closed, Transformation);
end;

procedure PolyPolylineAS(
  Bitmap: TCustomBitmap32;
  const Points: TArrayOfArrayOfFixedPoint;
  Color: TColor32;
  Closed: Boolean;
  Transformation: TTransformation);
var
  I: Integer;
begin
  for I := 0 to High(Points) do PolylineAS(Bitmap, Points[I], Color, Closed, Transformation);
end;

procedure PolyPolylineXS(
  Bitmap: TCustomBitmap32;
  const Points: TArrayOfArrayOfFixedPoint;
  Color: TColor32;
  Closed: Boolean;
  Transformation: TTransformation);
var
  I: Integer;
begin
  for I := 0 to High(Points) do PolylineXS(Bitmap, Points[I], Color, Closed, Transformation);
end;

procedure PolyPolylineXSP(
  Bitmap: TCustomBitmap32;
  const Points: TArrayOfArrayOfFixedPoint;
  Closed: Boolean;
  Transformation: TTransformation);
var
  I: Integer;
begin
  for I := 0 to High(Points) do PolylineXSP(Bitmap, Points[I], Closed, Transformation);
end;


{ General routines for drawing polygons }

procedure ScanLinesCreate(var ScanLines: TScanLines; Length: Integer);
begin
  SetLength(ScanLines, Length);
end;

procedure ScanLinesDestroy(var ScanLines: TScanLines);
var
  I: Integer;
begin
  for I := 0 to High(ScanLines) do
    FreeMem(ScanLines[I].EdgePoints);

  SetLength(ScanLines, 0);
end;


{ Routines for sorting edge points in scanlines }

const
  SortThreshold = 10;
  ReallocationThreshold = 64;

procedure InsertionSort(LPtr, RPtr: PInteger);
var
  IPtr, JPtr: PInteger;
  P, C, T: Integer;
begin
  IPtr := LPtr;
  Inc(IPtr);
  repeat
    C := IPtr^;
    P := C and $7FFFFFFF;
    JPtr := IPtr;

    if Integer(JPtr) > Integer(LPtr) then
    repeat
      T := PInteger(Integer(JPtr) - SizeOf(Integer))^;
      if T and $7FFFFFFF > P then
      begin
        JPtr^ := T;
        Dec(JPtr);
      end
      else
        Break;
    until Integer(JPtr) <= Integer(LPtr);

    JPtr^ := C;
    Inc(IPtr);
  until Integer(IPtr) > Integer(RPtr);
end;

procedure QuickSort(LPtr, RPtr: PInteger);
var
  P: Integer;
  IPtr, JPtr: PInteger;
  Temp: Integer;
const
  OddMask = SizeOf(Integer) and not(SizeOf(Integer) - 1);
begin
  if Integer(RPtr) - Integer(LPtr) > SortThreshold shl 2 then
  repeat
//    P := PInteger(Integer(LPtr) + (((Integer(RPtr) - Integer(LPtr)) shr 2) shr 1) shl 2)^ and $7FFFFFFF;

    P := Integer(RPtr) - Integer(LPtr);
    if (P and OddMask > 0) then Dec(P, SizeOf(Integer));
    P := PInteger(Integer(LPtr) + P shr 1)^ and $7FFFFFFF;

    IPtr := LPtr;
    JPtr := RPtr;
    repeat
      while (IPtr^ and $7FFFFFFF) < P do Inc(IPtr);
      while (JPtr^ and $7FFFFFFF) > P do Dec(JPtr);
      if Integer(IPtr) <= Integer(JPtr) then
      begin
        Temp := IPtr^;
        IPtr^ := JPtr^;
        JPtr^ := Temp;
//        Swap(IPtr^, JPtr^);
        Inc(IPtr);
        Dec(JPtr);
      end;
    until Integer(IPtr) > Integer(JPtr);
    if Integer(LPtr) < Integer(JPtr) then QuickSort(LPtr, JPtr);
    LPtr := IPtr;
  until Integer(IPtr) >= Integer(RPtr)
  else
    InsertionSort(LPtr, RPtr);
end;

procedure SortLine(const ALine: TScanLine);
var
  L, T: Integer;
begin
  L := ALine.Count;
  Assert(not Odd(L));
  if L = 2 then
  begin
    if (ALine.EdgePoints[0] and $7FFFFFFF) > (ALine.EdgePoints[1] and $7FFFFFFF) then
    begin
      T := ALine.EdgePoints[0];
      ALine.EdgePoints[0] := ALine.EdgePoints[1];
      ALine.EdgePoints[1] := T;
    end;
  end
  else if L > SortThreshold then
    QuickSort(@ALine.EdgePoints[0], @ALine.EdgePoints[L - 1])
  else if L > 2 then
    InsertionSort(@ALine.EdgePoints[0], @ALine.EdgePoints[L - 1]);
end;

procedure SortLines(const ScanLines: TScanLines);
var
  I: Integer;
begin
  for I := 0 to High(ScanLines) do SortLine(ScanLines[I]);
end;


{ Routines for rendering polygon edges to scanlines }

procedure AddEdgePoint(X: Integer; const Y: Integer; const ClipRect: TFixedRect; const ScanLines: TScanLines; const Direction: Integer);
var
  L: Integer;
  ScanLine: PScanLine;
begin
  if (Y < ClipRect.Top) or (Y > ClipRect.Bottom) then Exit;

  if X < ClipRect.Left then
    X := ClipRect.Left
  else if X > ClipRect.Right then
    X := ClipRect.Right;

  // positive direction (+1) is down
  if Direction < 0 then
    X := Integer(Longword(X) or $80000000); // set the highest bit if the winding is up

  ScanLine := @ScanLines[Y - ClipRect.Top];

  L := ScanLine.Count;
  Inc(ScanLine.Count);
  if ScanLine.Count > ScanLine.EdgePointsLength then
  begin
    ScanLine.EdgePointsLength := L + ReallocationThreshold;
    ReallocMem(ScanLine.EdgePoints, ScanLine.EdgePointsLength * SizeOf(TEdgePoint));
  end;
  ScanLine.EdgePoints[L] := X;  
end;

function DrawEdge(const P1, P2: TFixedPoint; const ClipRect: TFixedRect; const ScanLines: TScanLines): Integer;
var
  X, Y: Integer;
  I, K: Integer;
  Dx, Dy, Sx, Sy: Integer;
  Delta: Integer;
begin
  // this function 'renders' a line into the edge point (ScanLines) buffer
  // and returns the line direction (1 - down, -1 - up, 0 - horizontal)
  Result := 0;
  if P2.Y = P1.Y then Exit;
  Dx := P2.X - P1.X;
  Dy := P2.Y - P1.Y;

  if Dy > 0 then Sy := 1
  else
  begin
    Sy := -1;
    Dy := -Dy;
  end;

  Result := Sy;

  if Dx > 0 then Sx := 1
  else
  begin
    Sx := -1;
    Dx := -Dx;
  end;

  Delta := (Dx mod Dy) shr 1;
  X := P1.X; Y := P1.Y;

  for I := 0 to Dy - 1 do
  begin
    AddEdgePoint(X, Y, ClipRect, ScanLines, Result);
    Inc(Y, Sy);
    Inc(Delta, Dx);

    // try it two times and if anything else left, use div and mod
    if Delta > Dy then
    begin
      Inc(X, Sx);
      Dec(Delta, Dy);

      if Delta > Dy then  // segment is tilted more than 45 degrees?
      begin
        Inc(X, Sx);
        Dec(Delta, Dy);

        if Delta > Dy then // are we still here?
        begin
          K := (Delta + Dy - 1) div Dy;
          Inc(X, Sx * K);
          Dec(Delta, Dy * K);
        end;
      end;
    end;
  end;
end;


procedure RoundShift1(var DstPoint: TFixedPoint; const SrcPoint: TFixedPoint; const T: TTransformation); forward; {$IFNDEF TARGET_x86}{$IFDEF INLININGSUPPORTED} inline; {$ENDIF}{$ENDIF}
procedure RoundShift2(var DstPoint: TFixedPoint; const SrcPoint: TFixedPoint; const T: TTransformation); forward; {$IFNDEF TARGET_x86}{$IFDEF INLININGSUPPORTED} inline; {$ENDIF}{$ENDIF}
procedure RoundShift4(var DstPoint: TFixedPoint; const SrcPoint: TFixedPoint; const T: TTransformation); forward; {$IFNDEF TARGET_x86}{$IFDEF INLININGSUPPORTED} inline; {$ENDIF}{$ENDIF}
procedure RoundShift8(var DstPoint: TFixedPoint; const SrcPoint: TFixedPoint; const T: TTransformation); forward; {$IFNDEF TARGET_x86}{$IFDEF INLININGSUPPORTED} inline; {$ENDIF}{$ENDIF}
procedure RoundShift16(var DstPoint: TFixedPoint; const SrcPoint: TFixedPoint; const T: TTransformation); forward; {$IFNDEF TARGET_x86}{$IFDEF INLININGSUPPORTED} inline; {$ENDIF}{$ENDIF}
procedure RoundShift32(var DstPoint: TFixedPoint; const SrcPoint: TFixedPoint; const T: TTransformation); forward; {$IFNDEF TARGET_x86}{$IFDEF INLININGSUPPORTED} inline; {$ENDIF}{$ENDIF}

type
  TTransformProc = procedure(var DstPoint: TFixedPoint; const SrcPoint: TFixedPoint; const T: TTransformation);
  TTransformationAccess = class(TTransformation);

procedure Transform1(var DstPoint: TFixedPoint; const SrcPoint: TFixedPoint; const T: TTransformation);
begin
  TTransformationAccess(T).TransformFixed(SrcPoint.X, SrcPoint.Y, DstPoint.X, DstPoint.Y);
  RoundShift1(DstPoint, DstPoint, nil);
end;

procedure RoundShift1(var DstPoint: TFixedPoint; const SrcPoint: TFixedPoint; const T: TTransformation); {$IFNDEF TARGET_x86}{$IFDEF INLININGSUPPORTED} inline; {$ENDIF}{$ENDIF}
{$IFDEF PUREPASCAL}
begin
  DstPoint.X := (SrcPoint.X + $7F) div 256;
  DstPoint.Y := (SrcPoint.Y + $7FFF) div 65536;
{$ELSE}
{$IFDEF TARGET_x86}
asm
    MOV ECX, [SrcPoint.X]
    ADD ECX, $0000007F
    SAR ECX, 8 // sub-sampled
    MOV [DstPoint.X], ECX
    MOV EDX, [SrcPoint.Y]
    ADD EDX, $00007FFF
    SAR EDX, 16
    MOV [DstPoint.Y], EDX
{$ENDIF}
{$ENDIF}
end;

procedure Transform2(var DstPoint: TFixedPoint; const SrcPoint: TFixedPoint; const T: TTransformation);
begin
  TTransformationAccess(T).TransformFixed(SrcPoint.X, SrcPoint.Y, DstPoint.X, DstPoint.Y);
  RoundShift2(DstPoint, DstPoint, nil);
end;

procedure RoundShift2(var DstPoint: TFixedPoint; const SrcPoint: TFixedPoint; const T: TTransformation); {$IFNDEF TARGET_x86}{$IFDEF INLININGSUPPORTED} inline; {$ENDIF}{$ENDIF}
{$IFDEF PUREPASCAL}
begin
  DstPoint.X := (SrcPoint.X + $3FFF) div 32768;
  DstPoint.Y := (SrcPoint.Y + $3FFF) div 32768;
{$ELSE}
{$IFDEF TARGET_x86}
asm
    MOV ECX, [SrcPoint.X]
    ADD ECX, $00003FFF
    SAR ECX, 15
    MOV [DstPoint.X], ECX
    MOV EDX, [SrcPoint.Y]
    ADD EDX, $00003FFF
    SAR EDX, 15
    MOV [DstPoint.Y], EDX
{$ENDIF}
{$ENDIF}
end;

procedure Transform4(var DstPoint: TFixedPoint; const SrcPoint: TFixedPoint; const T: TTransformation);
begin
  TTransformationAccess(T).TransformFixed(SrcPoint.X, SrcPoint.Y, DstPoint.X, DstPoint.Y);
  RoundShift4(DstPoint, DstPoint, nil);
end;

procedure RoundShift4(var DstPoint: TFixedPoint; const SrcPoint: TFixedPoint; const T: TTransformation); {$IFNDEF TARGET_x86}{$IFDEF INLININGSUPPORTED} inline; {$ENDIF}{$ENDIF}
{$IFDEF PUREPASCAL}
begin
  DstPoint.X := (SrcPoint.X + $1FFF) div 16384;
  DstPoint.Y := (SrcPoint.Y + $1FFF) div 16384;
{$ELSE}
{$IFDEF TARGET_x86}
asm
    MOV ECX, [SrcPoint.X]
    ADD ECX, $00001FFF
    SAR ECX, 14
    MOV [DstPoint.X], ECX
    MOV EDX, [SrcPoint.Y]
    ADD EDX, $00001FFF
    SAR EDX, 14
    MOV [DstPoint.Y], EDX
{$ENDIF}
{$ENDIF}
end;

procedure Transform8(var DstPoint: TFixedPoint; const SrcPoint: TFixedPoint; const T: TTransformation);
begin
  TTransformationAccess(T).TransformFixed(SrcPoint.X, SrcPoint.Y, DstPoint.X, DstPoint.Y);
  RoundShift8(DstPoint, DstPoint, nil);
end;

procedure RoundShift8(var DstPoint: TFixedPoint; const SrcPoint: TFixedPoint; const T: TTransformation); {$IFNDEF TARGET_x86}{$IFDEF INLININGSUPPORTED} inline; {$ENDIF}{$ENDIF}
{$IFDEF PUREPASCAL}
begin
  DstPoint.X := (SrcPoint.X + $FFF) div 8192;
  DstPoint.Y := (SrcPoint.Y + $FFF) div 8192;
{$ELSE}
{$IFDEF TARGET_x86}
asm
    MOV ECX, [SrcPoint.X]
    ADD ECX, $00000FFF
    SAR ECX, 13
    MOV [DstPoint.X], ECX
    MOV EDX, [SrcPoint.Y]
    ADD EDX, $00000FFF
    SAR EDX, 13
    MOV [DstPoint.Y], EDX
{$ENDIF}
{$ENDIF}
end;

procedure Transform16(var DstPoint: TFixedPoint; const SrcPoint: TFixedPoint; const T: TTransformation);
begin
  TTransformationAccess(T).TransformFixed(SrcPoint.X, SrcPoint.Y, DstPoint.X, DstPoint.Y);
  RoundShift16(DstPoint, DstPoint, nil);
end;

procedure RoundShift16(var DstPoint: TFixedPoint; const SrcPoint: TFixedPoint; const T: TTransformation); {$IFNDEF TARGET_x86}{$IFDEF INLININGSUPPORTED} inline; {$ENDIF}{$ENDIF}
{$IFDEF PUREPASCAL}
begin
  DstPoint.X := (SrcPoint.X + $7FF) div 4096;
  DstPoint.Y := (SrcPoint.Y + $7FF) div 4096;
{$ELSE}
{$IFDEF TARGET_x86}
asm
    MOV ECX, [SrcPoint.X]
    ADD ECX, $000007FF
    SAR ECX, 12
    MOV [DstPoint.X], ECX
    MOV EDX, [SrcPoint.Y]
    ADD EDX, $000007FF
    SAR EDX, 12
    MOV [DstPoint.Y], EDX
{$ENDIF}
{$ENDIF}
end;

procedure Transform32(var DstPoint: TFixedPoint; const SrcPoint: TFixedPoint; const T: TTransformation);
begin
  TTransformationAccess(T).TransformFixed(SrcPoint.X, SrcPoint.Y, DstPoint.X, DstPoint.Y);
  RoundShift32(DstPoint, DstPoint, nil);
end;

procedure RoundShift32(var DstPoint: TFixedPoint; const SrcPoint: TFixedPoint; const T: TTransformation); {$IFNDEF TARGET_x86}{$IFDEF INLININGSUPPORTED} inline; {$ENDIF}{$ENDIF}
{$IFDEF PUREPASCAL}
begin
  DstPoint.X := (SrcPoint.X + $3FF) div 2048;
  DstPoint.Y := (SrcPoint.Y + $3FF) div 2048;
{$ELSE}
{$IFDEF TARGET_x86}
asm
    MOV ECX, [SrcPoint.X]
    ADD ECX, $000003FF
    SAR ECX, 11
    MOV [DstPoint.X], ECX
    MOV EDX, [SrcPoint.Y]
    ADD EDX, $000003FF
    SAR EDX, 11
    MOV [DstPoint.Y], EDX
{$ENDIF}
{$ENDIF}
end;

const
  RoundShiftProcs: array[TAntialiasMode] of TTransformProc = (RoundShift32, RoundShift16, RoundShift8, RoundShift4, RoundShift2, RoundShift1);
  TransformProcs:  array[TAntialiasMode] of TTransformProc = (Transform32, Transform16, Transform8, Transform4, Transform2, Transform1);

procedure AddPolygon(const Points: TArrayOfFixedPoint; const ClipRect: TFixedRect;
  var ScanLines: TScanLines; AAMode: TAntialiasMode; Transformation: TTransformation);
var
  P1, P2: TFixedPoint;
  I: Integer;
  PPtr: PFixedPoint;
  Transform: TTransformProc;
  Direction, PrevDirection: Integer; // up = 1 or down = -1
begin
  if Length(Points) < 3 then Exit;

  if Assigned(Transformation) then
    Transform := TransformProcs[AAMode]
  else
    Transform := RoundShiftProcs[AAMode];

  Transform(P1, Points[0], Transformation);

  // find the last Y different from Y1 and get direction
  PrevDirection := 0;
  I := High(Points);
  PPtr := @Points[I];

  while (I > 0) and (PrevDirection = 0) do
  begin
    Dec(I);
    Transform(P2, PPtr^, Transformation); { TODO : optimize minor inefficiency... }
    PrevDirection := P1.Y - P2.Y;
    Dec(PPtr);
  end;

  if PrevDirection > 0 then
    PrevDirection := 1
  else if PrevDirection < 0 then
    PrevDirection := -1
  else
    PrevDirection := 0;

  PPtr := @Points[1];
  for I := 1 to High(Points) do
  begin
    Transform(P2, PPtr^, Transformation);

    if P1.Y <> P2.Y then
    begin
      Direction := DrawEdge(P1, P2, ClipRect, ScanLines);
      if Direction <> PrevDirection then
      begin
        AddEdgePoint(P1.X, P1.Y, ClipRect, ScanLines, -Direction);
        PrevDirection := Direction;
      end;
    end;

    P1 := P2;
    Inc(PPtr);
  end;

  Transform(P2, Points[0], Transformation);

  if P1.Y <> P2.Y then
  begin
    Direction := DrawEdge(P1, P2, ClipRect, ScanLines);
    if Direction <> PrevDirection then AddEdgePoint(P1.X, P1.Y, ClipRect, ScanLines, -Direction);
  end;
end;


{ FillLines routines }
{ These routines rasterize the sorted edge points in the scanlines to
  the bitmap buffer }

procedure ColorFillLines(Bitmap: TCustomBitmap32; BaseY: Integer;
  const ScanLines: TScanLines; Color: TColor32; Mode: TPolyFillMode);
var
  I, J, L: Integer;
  Top, Left, Right, OldRight, LP, RP, Cx: Integer;
  Winding, NextWinding: Integer;
  HorzLine: procedure(X1, Y, X2: Integer; Value: TColor32) of Object;
begin
  if Color and $FF000000 <> $FF000000 then
    HorzLine := Bitmap.HorzLineT
  else
    HorzLine := Bitmap.HorzLine;

  Cx := Bitmap.ClipRect.Right - 1;
  Top := BaseY - 1;

  if Mode = pfAlternate then
    for J := 0 to High(ScanLines) do
    begin
      Inc(Top);
      L := ScanLines[J].Count; // assuming length is even
      if L = 0 then Continue;
      I := 0;
      OldRight := -1;

      while I < L do
      begin
        Left := ScanLines[J].EdgePoints[I] and $7FFFFFFF;
        Inc(I);
        Right := ScanLines[J].EdgePoints[I] and $7FFFFFFF - 1;
        if Right > Left then
        begin
          if (Left and $FF) < $80 then Left := Left shr 8
          else Left := Left shr 8 + 1;

          if (Right and $FF) < $80 then Right := Right shr 8
          else Right := Right shr 8 + 1;

          if Right >= Cx then Right := Cx;

          if Left <= OldRight then Left := OldRight + 1;
          OldRight := Right;
          if Right >= Left then HorzLine(Left, Top, Right, Color);
        end;
        Inc(I);
      end
    end
  else // Mode = pfWinding
    for J := 0 to High(ScanLines) do
    begin
      Inc(Top);
      L := ScanLines[J].Count; // assuming length is even
      if L = 0 then Continue;
      I := 0;

      Winding := 0;
      Left := ScanLines[J].EdgePoints[0];
      if (Left and $80000000) <> 0 then Inc(Winding) else Dec(Winding);
      Left := Left and $7FFFFFFF;
      Inc(I);

      while I < L do
      begin
        Right := ScanLines[J].EdgePoints[I];
        if (Right and $80000000) <> 0 then NextWinding := 1 else NextWinding := -1;
        Right := Right and $7FFFFFFF;
        Inc(I);

        if Winding <> 0 then
        begin
          if (Left and $FF) < $80 then LP := Left shr 8
          else LP := Left shr 8 + 1;
          if (Right and $FF) < $80 then RP := Right shr 8
          else RP := Right shr 8 + 1;

          if RP >= Cx then RP := Cx;

          if RP >= LP then HorzLine(LP, Top, RP, Color);
        end;

        Inc(Winding, NextWinding);
        Left := Right;
      end;
    end;
end;

procedure ColorFillLines2(Bitmap: TCustomBitmap32; BaseY: Integer;
  const ScanLines: TScanLines; Color: TColor32; Mode: TPolyFillMode;
  const AAMode: TAntialiasMode = DefaultAAMode);
var
  I, J, L, N: Integer;
  MinY, MaxY, Y, Top, Bottom: Integer;
  MinX, MaxX, X, Dx: Integer;
  Left, Right: Integer;
  Buffer: array of Integer;
  ColorBuffer: array of TColor32;
  BufferSize: Integer;
  C, A: TColor32;
  ScanLine: PIntegerArray;
  Winding, NextWinding: Integer;
  AAShift, AALines, AAMultiplier: Integer;
  BlendLineEx: TBlendLineEx;
begin
  A := Color shr 24;

  AAShift := AA_SHIFT[AAMode];
  AALines := AA_LINES[AAMode] - 1; // we do the -1 here for optimization.
  AAMultiplier := AA_MULTI[AAMode];

  BlendLineEx := BLEND_LINE_EX[Bitmap.CombineMode]^;

  // find the range of Y screen coordinates
  MinY := BaseY shr AAShift;
  MaxY := (BaseY + Length(ScanLines) + AALines) shr AAShift;

  Y := MinY;
  while Y < MaxY do
  begin
    Top := Y shl AAShift - BaseY;
    Bottom := Top + AALines;
    if Top < 0 then Top := 0;
    if Bottom >= Length(ScanLines) then Bottom := High(ScanLines);

    // find left and right edges of the screen scanline
    MinX := $7F000000; MaxX := -$7F000000;
    for J := Top to Bottom do
    begin
      L := ScanLines[J].Count - 1;
      if L > 0 then
      begin
        Left := (ScanLines[J].EdgePoints[0] and $7FFFFFFF);
        Right := (ScanLines[J].EdgePoints[L] and $7FFFFFFF + AALines);
        if Left < MinX then MinX := Left;
        if Right > MaxX then MaxX := Right;
      end
    end;

    if MaxX >= MinX then
    begin
      MinX := MinX shr AAShift;
      MaxX := MaxX shr AAShift;
      // allocate buffer for a single scanline
      BufferSize := MaxX - MinX + 2;
      if Length(Buffer) < BufferSize then
      begin
        SetLength(Buffer, BufferSize + 64);
        SetLength(ColorBuffer, BufferSize + 64);
      end;
      FillLongword(Buffer[0], BufferSize, 0);

      // ...and fill it
      if Mode = pfAlternate then
        for J := Top to Bottom do
        begin
          I := 0;
          L := ScanLines[J].Count;
          ScanLine := @ScanLines[J].EdgePoints[0];
          while I < L do
          begin
            // Left edge
            X := ScanLine[I] and $7FFFFFFF;
            Dx := X and AALines;
            X := X shr AAShift - MinX;
            Inc(Buffer[X], Dx xor AALines);
            Inc(Buffer[X + 1], Dx);
            Inc(I);

            // Right edge
            X := ScanLine[I] and $7FFFFFFF;
            Dx := X and AALines;
            X := X shr AAShift - MinX;
            Dec(Buffer[X], Dx xor AALines);
            Dec(Buffer[X + 1], Dx);
            Inc(I);
          end
        end
      else // mode = pfWinding
        for J := Top to Bottom do
        begin
          I := 0;
          L := ScanLines[J].Count;
          ScanLine := @ScanLines[J].EdgePoints[0];
          Winding := 0;
          while I < L do
          begin
            X := ScanLine[I];
            Inc(I);
            if (X and $80000000) <> 0 then NextWinding := 1 else NextWinding := -1;
            X := X and $7FFFFFFF;
            if Winding = 0 then
            begin
              Dx := X and AALines;
              X := X shr AAShift - MinX;
              Inc(Buffer[X], Dx xor AALines);
              Inc(Buffer[X + 1], Dx);
            end;
            Inc(Winding, NextWinding);
            if Winding = 0 then
            begin
              Dx := X and AALines;
              X := X shr AAShift - MinX;
              Dec(Buffer[X], Dx xor AALines);
              Dec(Buffer[X + 1], Dx);
            end;
          end;
        end;

      // integrate the buffer
      N := 0;
      C := Color and $00FFFFFF;
      for I := 0 to BufferSize - 1 do
      begin
        Inc(N, Buffer[I]);
        ColorBuffer[I] := TColor32(N * AAMultiplier and $FF00) shl 16 or C;
      end;

      // draw it to the screen
      BlendLineEx(@ColorBuffer[0], Pointer(Bitmap.PixelPtr[MinX, Y]), BufferSize, A);
      EMMS;
    end;

    Inc(Y);
  end;
end;

procedure CustomFillLines(Bitmap: TCustomBitmap32; BaseY: Integer;
  const ScanLines: TScanLines; FillLineCallback: TFillLineEvent; Mode: TPolyFillMode);
var
  I, J, L: Integer;
  Top, Left, Right, OldRight, LP, RP, Cx: Integer;
  Winding, NextWinding: Integer;
begin
  Top := BaseY - 1;
  Cx := Bitmap.ClipRect.Right - 1;

  if Mode = pfAlternate then
    for J := 0 to High(ScanLines) do
    begin
      Inc(Top);
      L := ScanLines[J].Count; // assuming length is even
      if L = 0 then Continue;
      I := 0;
      OldRight := -1;

      while I < L do
      begin
        Left := ScanLines[J].EdgePoints[I] and $7FFFFFFF;
        Inc(I);
        Right := ScanLines[J].EdgePoints[I] and $7FFFFFFF - 1;
        if Right > Left then
        begin
          if (Left and $FF) < $80 then Left := Left shr 8
          else Left := Left shr 8 + 1;
          if (Right and $FF) < $80 then Right := Right shr 8
          else Right := Right shr 8 + 1;

          if Right >= Cx then Right := Cx;

          if Left <= OldRight then Left := OldRight + 1;
          OldRight := Right;
          if Right >= Left then
            FillLineCallback(Bitmap.PixelPtr[Left, Top], Left, Top, Right - Left, nil);
        end;
        Inc(I);
      end
    end
  else // Mode = pfWinding
    for J := 0 to High(ScanLines) do
    begin
      Inc(Top);
      L := ScanLines[J].Count; // assuming length is even
      if L = 0 then Continue;
      I := 0;

      Winding := 0;
      Left := ScanLines[J].EdgePoints[0];
      if (Left and $80000000) <> 0 then Inc(Winding) else Dec(Winding);
      Left := Left and $7FFFFFFF;
      Inc(I);
      while I < L do
      begin
        Right := ScanLines[J].EdgePoints[I];
        if (Right and $80000000) <> 0 then NextWinding := 1 else NextWinding := -1;
        Right := Right and $7FFFFFFF;
        Inc(I);

        if Winding <> 0 then
        begin
          if (Left and $FF) < $80 then LP := Left shr 8
          else LP := Left shr 8 + 1;
          if (Right and $FF) < $80 then RP := Right shr 8
          else RP := Right shr 8 + 1;

          if RP >= Cx then RP := Cx;

          if RP >= LP then
            FillLineCallback(Bitmap.PixelPtr[LP, Top], LP, Top, RP - LP, nil);
        end;

        Inc(Winding, NextWinding);
        Left := Right;
      end;
    end;
  EMMS;
end;

procedure CustomFillLines2(Bitmap: TCustomBitmap32; BaseY: Integer;
  const ScanLines: TScanLines; FillLineCallback: TFillLineEvent; Mode: TPolyFillMode;
  const AAMode: TAntialiasMode = DefaultAAMode);
var
  I, J, L, N: Integer;
  MinY, MaxY, Y, Top, Bottom: Integer;
  MinX, MaxX, X, Dx: Integer;
  Left, Right: Integer;
  Buffer: array of Integer;
  AlphaBuffer: array of TColor32;
  BufferSize: Integer;
  ScanLine: PIntegerArray;
  Winding, NextWinding: Integer;
  AAShift, AALines, AAMultiplier: Integer;
begin
  AAShift := AA_SHIFT[AAMode];
  AALines := AA_LINES[AAMode] - 1; // we do the -1 here for optimization.
  AAMultiplier := AA_MULTI[AAMode];

  // find the range of Y screen coordinates
  MinY := BaseY shr AAShift;
  MaxY := (BaseY + Length(ScanLines) + AALines) shr AAShift;

  Y := MinY;
  while Y < MaxY do
  begin
    Top := Y shl AAShift - BaseY;
    Bottom := Top + AALines;
    if Top < 0 then Top := 0;
    if Bottom >= Length(ScanLines) then Bottom := High(ScanLines);

    // find left and right edges of the screen scanline
    MinX := $7F000000; MaxX := -$7F000000;
    for J := Top to Bottom do
    begin
      L := ScanLines[J].Count - 1;
      if L > 0 then
      begin
        Left := (ScanLines[J].EdgePoints[0] and $7FFFFFFF);
        Right := (ScanLines[J].EdgePoints[L] and $7FFFFFFF + AALines);
        if Left < MinX then MinX := Left;
        if Right > MaxX then MaxX := Right;
      end
    end;

    if MaxX >= MinX then
    begin
      MinX := MinX shr AAShift;
      MaxX := MaxX shr AAShift;
      // allocate buffer for a single scanline
      BufferSize := MaxX - MinX + 2;
      if Length(Buffer) < BufferSize then
      begin
        SetLength(Buffer, BufferSize + 64);
        SetLength(AlphaBuffer, BufferSize + 64);
      end;
      FillLongword(Buffer[0], BufferSize, 0);

      // ...and fill it
      if Mode = pfAlternate then
        for J := Top to Bottom do
        begin
          I := 0;
          L := ScanLines[J].Count;
          ScanLine := @ScanLines[J].EdgePoints[0];
          while I < L do
          begin
            // Left edge
            X := ScanLine[I] and $7FFFFFFF;
            Dx := X and AALines;
            X := X shr AAShift - MinX;
            Inc(Buffer[X], Dx xor AALines);
            Inc(Buffer[X + 1], Dx);
            Inc(I);

            // Right edge
            X := ScanLine[I] and $7FFFFFFF;
            Dx := X and AALines;
            X := X shr AAShift - MinX;
            Dec(Buffer[X], Dx xor AALines);
            Dec(Buffer[X + 1], Dx);
            Inc(I);
          end
        end
      else // mode = pfWinding
        for J := Top to Bottom do
        begin
          I := 0;
          L := ScanLines[J].Count;
          ScanLine := @ScanLines[J].EdgePoints[0];
          Winding := 0;
          while I < L do
          begin
            X := ScanLine[I];
            Inc(I);
            if (X and $80000000) <> 0 then NextWinding := 1 else NextWinding := -1;
            X := X and $7FFFFFFF;
            if Winding = 0 then
            begin
              Dx := X and AALines;
              X := X shr AAShift - MinX;
              Inc(Buffer[X], Dx xor AALines);
              Inc(Buffer[X + 1], Dx);
            end;
            Inc(Winding, NextWinding);
            if Winding = 0 then
            begin
              Dx := X and AALines;
              X := X shr AAShift - MinX;
              Dec(Buffer[X], Dx xor AALines);
              Dec(Buffer[X + 1], Dx);
            end;
          end;
        end;

      // integrate the buffer
      N := 0;
      for I := 0 to BufferSize - 1 do
      begin
        Inc(N, Buffer[I]);
        AlphaBuffer[I] := (N * AAMultiplier) shr 8;
      end;

      // draw it to the screen
      FillLineCallback(Pointer(Bitmap.PixelPtr[MinX, Y]), MinX, Y, BufferSize, @AlphaBuffer[0]);
      EMMS;
    end;

    Inc(Y);
  end;
end;


{ Helper routines for drawing Polygons and PolyPolygons }

procedure RenderPolyPolygon(Bitmap: TCustomBitmap32;
  const Points: TArrayOfArrayOfFixedPoint; Color: TColor32;
  FillLineCallback: TFillLineEvent; Mode: TPolyFillMode;
  const AAMode: TAntialiasMode; Transformation: TTransformation);
var
  ChangedRect, DstRect: TFixedRect;
  P: TFixedPoint;
  AAShift: Integer;
  I: Integer;
  ScanLines: TScanLines;
begin
  if not Bitmap.MeasuringMode then
  begin
    ChangedRect := PolyPolygonBounds(Points, Transformation);

    with DstRect do
    if AAMode <> amNone then
    begin
      AAShift := AA_SHIFT[AAMode];
      Left := Bitmap.ClipRect.Left shl AAShift;
      Right := Bitmap.ClipRect.Right shl AAShift - 1;
      Top := Bitmap.ClipRect.Top shl AAShift;
      Bottom := Bitmap.ClipRect.Bottom shl AAShift - 1;

      P.X := ChangedRect.Top;
      P.Y := ChangedRect.Bottom;
      RoundShiftProcs[AAMode](P, P, nil);
      Top := Constrain(P.X, Top, Bottom);
      Bottom := Constrain(P.Y, Top, Bottom);
    end
    else
    begin
      Left := Bitmap.ClipRect.Left shl 8;
      Right := Bitmap.ClipRect.Right shl 8 - 1;
      Top := Constrain(SAR_16(ChangedRect.Top + $00007FFF),
        Bitmap.ClipRect.Top, Bitmap.ClipRect.Bottom - 1);
      Bottom := Constrain(SAR_16(ChangedRect.Bottom + $00007FFF),
        Bitmap.ClipRect.Top, Bitmap.ClipRect.Bottom - 1);
    end;

    if DstRect.Top >= DstRect.Bottom then Exit;

    ScanLinesCreate(ScanLines, DstRect.Bottom - DstRect.Top + 1);
    for I := 0 to High(Points) do
      AddPolygon(Points[I], DstRect, ScanLines, AAMode, Transformation);

    SortLines(ScanLines);
    Bitmap.BeginUpdate;
    try
      if AAMode <> amNone then
        if Assigned(FillLineCallback) then
          CustomFillLines2(Bitmap, DstRect.Top, ScanLines, FillLineCallback, Mode, AAMode)
        else
          ColorFillLines2(Bitmap, DstRect.Top, ScanLines, Color, Mode, AAMode)
      else
        if Assigned(FillLineCallback) then
          CustomFillLines(Bitmap, DstRect.Top, ScanLines, FillLineCallback, Mode)
        else
          ColorFillLines(Bitmap, DstRect.Top, ScanLines, Color, Mode);
    finally
      Bitmap.EndUpdate;
      ScanLinesDestroy(ScanLines);
    end;
    Bitmap.Changed(MakeRect(ChangedRect, rrOutside));
  end
  else
    Bitmap.Changed(MakeRect(PolyPolygonBounds(Points, Transformation), rrOutside));
end;

procedure RenderPolygon(Bitmap: TCustomBitmap32;
  const Points: TArrayOfFixedPoint; Color: TColor32;
  FillLineCallback: TFillLineEvent; Mode: TPolyFillMode;
  const AAMode: TAntialiasMode; Transformation: TTransformation);
var
  H: TArrayOfArrayOfFixedPoint;
begin
  SetLength(H, 1);
  H[0] := Points;
  RenderPolyPolygon(Bitmap, H, Color, FillLineCallback, Mode, AAMode, Transformation);
  H[0] := nil;
end;


{ Polygons }

procedure PolygonTS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  Color: TColor32; Mode: TPolyFillMode; Transformation: TTransformation);
begin
  RenderPolygon(Bitmap, Points, Color, nil, Mode, amNone, Transformation);
end;

procedure PolygonTS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  FillLineCallback: TFillLineEvent; Mode: TPolyFillMode;
  Transformation: TTransformation);
begin
  RenderPolygon(Bitmap, Points, 0, FillLineCallback, Mode, amNone, Transformation);
end;

procedure PolygonTS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  Filler: TCustomPolygonFiller; Mode: TPolyFillMode;
  Transformation: TTransformation);
begin
  RenderPolygon(Bitmap, Points, 0, Filler.FillLine, Mode, amNone, Transformation);
end;

procedure PolygonXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  Color: TColor32; Mode: TPolyFillMode;
  const AAMode: TAntialiasMode; Transformation: TTransformation);
begin
  RenderPolygon(Bitmap, Points, Color, nil, Mode, AAMode, Transformation);
end;

procedure PolygonXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  FillLineCallback: TFillLineEvent; Mode: TPolyFillMode;
  const AAMode: TAntialiasMode; Transformation: TTransformation);
begin
  RenderPolygon(Bitmap, Points, 0, FillLineCallback, Mode, AAMode, Transformation);
end;

procedure PolygonXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint;
  Filler: TCustomPolygonFiller; Mode: TPolyFillMode;
  const AAMode: TAntialiasMode; Transformation: TTransformation);
begin
  RenderPolygon(Bitmap, Points, 0, Filler.FillLine, Mode, AAMode, Transformation);
end;


{ PolyPolygons }

procedure PolyPolygonTS(Bitmap: TCustomBitmap32;
  const Points: TArrayOfArrayOfFixedPoint; Color: TColor32; Mode: TPolyFillMode;
  Transformation: TTransformation);
begin
  RenderPolyPolygon(Bitmap, Points, Color, nil, Mode, amNone, Transformation);
end;

procedure PolyPolygonTS(Bitmap: TCustomBitmap32;
  const Points: TArrayOfArrayOfFixedPoint; FillLineCallback: TFillLineEvent;
  Mode: TPolyFillMode; Transformation: TTransformation);
begin
  RenderPolyPolygon(Bitmap, Points, 0, FillLineCallback, Mode, amNone, Transformation);
end;

procedure PolyPolygonTS(Bitmap: TCustomBitmap32;
  const Points: TArrayOfArrayOfFixedPoint; Filler: TCustomPolygonFiller;
  Mode: TPolyFillMode; Transformation: TTransformation);
begin
  RenderPolyPolygon(Bitmap, Points, 0, Filler.FillLine, Mode, amNone, Transformation);
end;

procedure PolyPolygonXS(Bitmap: TCustomBitmap32;
  const Points: TArrayOfArrayOfFixedPoint; Color: TColor32; Mode: TPolyFillMode;
  const AAMode: TAntialiasMode; Transformation: TTransformation);
begin
  RenderPolyPolygon(Bitmap, Points, Color, nil, Mode, AAMode, Transformation);
end;

procedure PolyPolygonXS(Bitmap: TCustomBitmap32;
  const Points: TArrayOfArrayOfFixedPoint; FillLineCallback: TFillLineEvent;
  Mode: TPolyFillMode; const AAMode: TAntialiasMode;
  Transformation: TTransformation);
begin
  RenderPolyPolygon(Bitmap, Points, 0, FillLineCallback, Mode, AAMode, Transformation);
end;

procedure PolyPolygonXS(Bitmap: TCustomBitmap32;
  const Points: TArrayOfArrayOfFixedPoint; Filler: TCustomPolygonFiller;
  Mode: TPolyFillMode; const AAMode: TAntialiasMode;
  Transformation: TTransformation);
begin
  RenderPolyPolygon(Bitmap, Points, 0, Filler.FillLine, Mode, AAMode, Transformation);
end;


{ Helper routines }

function PolygonBounds(const Points: TArrayOfFixedPoint;
  Transformation: TTransformation): TFixedRect;
var
  I: Integer;
begin
  with Result do
  begin
    Left := $7FFFFFFF;
    Right := -$7FFFFFFF;
    Top := $7FFFFFFF;
    Bottom := -$7FFFFFFF;

    if Assigned(Transformation) then
    begin
      for I := 0 to High(Points) do
      with Transformation.Transform(Points[I]) do
      begin
        if X < Left   then Left := X;
        if X > Right  then Right := X;
        if Y < Top    then Top := Y;
        if Y > Bottom then Bottom := Y;
      end
    end
    else
      for I := 0 to High(Points) do
      with Points[I] do
      begin
        if X < Left   then Left := X;
        if X > Right  then Right := X;
        if Y < Top    then Top := Y;
        if Y > Bottom then Bottom := Y;
      end;
  end;
end;

function PolyPolygonBounds(const Points: TArrayOfArrayOfFixedPoint;
  Transformation: TTransformation): TFixedRect;
var
  I, J: Integer;
begin
  with Result do
  begin
    Left := $7FFFFFFF;
    Right := -$7FFFFFFF;
    Top := $7FFFFFFF;
    Bottom := -$7FFFFFFF;

    if Assigned(Transformation) then
      for I := 0 to High(Points) do
        for J := 0 to High(Points[I]) do
        with Transformation.Transform(Points[I, J]) do
        begin
          if X < Left   then Left := X;
          if X > Right  then Right := X;
          if Y < Top    then Top := Y;
          if Y > Bottom then Bottom := Y;
        end
    else
      for I := 0 to High(Points) do
        for J := 0 to High(Points[I]) do
        with Points[I, J] do
        begin
          if X < Left   then Left := X;
          if X > Right  then Right := X;
          if Y < Top    then Top := Y;
          if Y > Bottom then Bottom := Y;
        end;
  end;
end;

function PtInPolygon(const Pt: TFixedPoint; const Points: TArrayOfFixedPoint): Boolean;
var
  I: Integer;
  iPt, jPt: PFixedPoint;
begin
  Result := False;
  iPt := @Points[0];
  jPt := @Points[High(Points)];
  for I := 0 to High(Points) do
  begin
    Result := Result xor (((Pt.Y >= iPt.Y) xor (Pt.Y >= jPt.Y)) and
      (Pt.X - iPt.X < MulDiv(jPt.X - iPt.X, Pt.Y - iPt.Y, jPt.Y - iPt.Y)));
    jPt := iPt;
    Inc(iPt);
  end;
end;

{ TPolygon32 }

procedure TPolygon32.Add(const P: TFixedPoint);
var
  H, L: Integer;
begin
  H := High(Points);
  L := Length(Points[H]);
  SetLength(Points[H], L + 1);
  Points[H][L] := P;
  Normals := nil;
end;

procedure TPolygon32.AddPoints(var First: TFixedPoint; Count: Integer);
var
  H, L, I: Integer;
begin
  H := High(Points);
  L := Length(Points[H]);
  SetLength(Points[H], L + Count);
  for I := 0 to Count - 1 do
    Points[H, L + I] := PFixedPointArray(@First)[I];
  Normals := nil;
end;

procedure TPolygon32.CopyPropertiesTo(Dst: TPolygon32);
begin
  Dst.Antialiased := Antialiased;
  Dst.AntialiasMode := AntialiasMode;
  Dst.Closed := Closed;
  Dst.FillMode := FillMode;
end;

procedure TPolygon32.AssignTo(Dst: TPersistent);
var
  DstPolygon: TPolygon32;
begin
  if Dst is TPolygon32 then
  begin
    DstPolygon := TPolygon32(Dst);
    CopyPropertiesTo(DstPolygon);
    DstPolygon.Normals := Copy(Normals);
    DstPolygon.Points := Copy(Points);
  end
  else
    inherited;
end;

function TPolygon32.GetBoundingRect: TFixedRect;
begin
  Result := PolyPolygonBounds(Points);
end;

procedure TPolygon32.BuildNormals;
var
  I, J, Count, NextI: Integer;
  dx, dy, f: Single;
begin
  if Length(Normals) <> 0 then Exit;
  SetLength(FNormals, Length(Points));

  for J := 0 to High(Points) do
  begin
    Count := Length(Points[J]);
    SetLength(Normals[J], Count);

    if Count = 0 then Continue;
    if Count = 1 then
    begin
      FillChar(Normals[J][0], SizeOf(TFixedPoint), 0);
      Continue;
    end;

    I := 0;
    NextI := 1;
    dx := 0;
    dy := 0;

    while I < Count do
    begin
      if Closed and (NextI >= Count) then NextI := 0;
      if NextI < Count then
      begin
        dx := (Points[J][NextI].X - Points[J][I].X) / $10000;
        dy := (Points[J][NextI].Y - Points[J][I].Y) / $10000;
      end;
      if (dx <> 0) or (dy <> 0) then
      begin
        f := 1 / GR32_Math.Hypot(dx, dy);
        dx := dx * f;
        dy := dy * f;
      end;
      with Normals[J][I] do
      begin
        X := Fixed(dy);
        Y := Fixed(-dx);
      end;
      Inc(I);
      Inc(NextI);
    end;
  end;
end;

procedure TPolygon32.Clear;
begin
  Points := nil;
  Normals := nil;
  NewLine;
end;

function TPolygon32.ContainsPoint(const P: TFixedPoint): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to High(FPoints) do
    if PtInPolygon(P, FPoints[I]) then
    begin
      Result := True;
      Exit;
    end;
end;

constructor TPolygon32.Create;
begin
  inherited;
  FClosed := True;
  FAntialiasMode := DefaultAAMode;
  NewLine; // initiate a new contour
end;

destructor TPolygon32.Destroy;
begin
  Clear;
  inherited;
end;

procedure TPolygon32.Draw(Bitmap: TCustomBitmap32; OutlineColor, FillColor: TColor32; Transformation: TTransformation);
begin
  Bitmap.BeginUpdate;

  if Antialiased then
  begin
    if (FillColor and $FF000000) <> 0 then
      PolyPolygonXS(Bitmap, Points, FillColor, FillMode, AntialiasMode, Transformation);
    if (OutlineColor and $FF000000) <> 0 then
      PolyPolylineXS(Bitmap, Points, OutlineColor, Closed, Transformation);
  end
  else
  begin
    if (FillColor and $FF000000) <> 0 then
      PolyPolygonTS(Bitmap, Points, FillColor, FillMode, Transformation);
    if (OutlineColor and $FF000000) <> 0 then
      PolyPolylineTS(Bitmap, Points, OutlineColor, Closed, Transformation);
  end;

  Bitmap.EndUpdate;
  Bitmap.Changed;
end;

procedure TPolygon32.Draw(Bitmap: TCustomBitmap32; OutlineColor: TColor32;
  FillCallback: TFillLineEvent; Transformation: TTransformation);
begin
  Bitmap.BeginUpdate;

  if Antialiased then
  begin
{$IFDEF FPC}
    RenderPolyPolygon(Bitmap, Points, 0, FillCallback, FillMode, AntialiasMode, Transformation);
{$ELSE}
    PolyPolygonXS(Bitmap, Points, FillCallback, FillMode, AntialiasMode, Transformation);
{$ENDIF}
    if (OutlineColor and $FF000000) <> 0 then
      PolyPolylineXS(Bitmap, Points, OutlineColor, Closed, Transformation);
  end
  else
  begin
{$IFDEF FPC}
    RenderPolyPolygon(Bitmap, Points, 0, FillCallback, FillMode, amNone, Transformation);
{$ELSE}
    PolyPolygonTS(Bitmap, Points, FillCallback, FillMode, Transformation);
{$ENDIF}
    if (OutlineColor and $FF000000) <> 0 then
      PolyPolylineTS(Bitmap, Points, OutlineColor, Closed, Transformation);
  end;

  Bitmap.EndUpdate;
  Bitmap.Changed;
end;

procedure TPolygon32.Draw(Bitmap: TCustomBitmap32; OutlineColor: TColor32;
  Filler: TCustomPolygonFiller; Transformation: TTransformation);
begin
{$IFDEF FPC}
  Bitmap.BeginUpdate;

  if Antialiased then
  begin
    RenderPolyPolygon(Bitmap, Points, 0, Filler.FillLine, FillMode, AntialiasMode, Transformation);
    if (OutlineColor and $FF000000) <> 0 then
      PolyPolylineXS(Bitmap, Points, OutlineColor, Closed, Transformation);
  end
  else
  begin
    RenderPolyPolygon(Bitmap, Points, 0, Filler.FillLine, FillMode, amNone, Transformation);
    if (OutlineColor and $FF000000) <> 0 then
      PolyPolylineTS(Bitmap, Points, OutlineColor, Closed, Transformation);
  end;

  Bitmap.EndUpdate;
  Bitmap.Changed;

{$ELSE}
  Draw(Bitmap, OutlineColor, Filler.FillLine, Transformation);
{$ENDIF}
end;

procedure TPolygon32.DrawEdge(Bitmap: TCustomBitmap32; Color: TColor32; Transformation: TTransformation);
begin
  Bitmap.BeginUpdate;

  if Antialiased then
    PolyPolylineXS(Bitmap, Points, Color, Closed, Transformation)
  else
    PolyPolylineTS(Bitmap, Points, Color, Closed, Transformation);

  Bitmap.EndUpdate;
  Bitmap.Changed;
end;

procedure TPolygon32.DrawFill(Bitmap: TCustomBitmap32; Color: TColor32; Transformation: TTransformation);
begin
  Bitmap.BeginUpdate;

  if Antialiased then
    PolyPolygonXS(Bitmap, Points, Color, FillMode, AntialiasMode, Transformation)
  else
    PolyPolygonTS(Bitmap, Points, Color, FillMode, Transformation);

  Bitmap.EndUpdate;
  Bitmap.Changed;
end;

procedure TPolygon32.DrawFill(Bitmap: TCustomBitmap32; FillCallback: TFillLineEvent;
  Transformation: TTransformation);
begin
  Bitmap.BeginUpdate;

{$IFDEF FPC}
  if Antialiased then
    RenderPolyPolygon(Bitmap, Points, 0, FillCallback, FillMode, AntialiasMode, Transformation)
  else
    RenderPolyPolygon(Bitmap, Points, 0, FillCallback, FillMode, amNone, Transformation);
{$ELSE}
  if Antialiased then
    PolyPolygonXS(Bitmap, Points, FillCallback, FillMode, AntialiasMode, Transformation)
  else
    PolyPolygonTS(Bitmap, Points, FillCallback, FillMode, Transformation);
{$ENDIF}

  Bitmap.EndUpdate;
  Bitmap.Changed;
end;

procedure TPolygon32.DrawFill(Bitmap: TCustomBitmap32; Filler: TCustomPolygonFiller;
  Transformation: TTransformation);
begin
{$IFDEF FPC}
  Bitmap.BeginUpdate;
  if Antialiased then
    RenderPolyPolygon(Bitmap, Points, 0, Filler.FillLine, FillMode, AntialiasMode, Transformation)
  else
    RenderPolyPolygon(Bitmap, Points, 0, Filler.FillLine, FillMode, amNone, Transformation);

  Bitmap.EndUpdate;
  Bitmap.Changed;
{$ELSE}
  DrawFill(Bitmap, Filler.FillLine, Transformation);
{$ENDIF}
end;

function TPolygon32.Grow(const Delta: TFixed; EdgeSharpness: Single = 0): TPolygon32;
var
  J, I, PrevI: Integer;
  PX, PY, AX, AY, BX, BY, CX, CY, R, D, E: Integer;

  procedure AddPoint(LongDeltaX, LongDeltaY: Integer);
  var
    N, L: Integer;
  begin
    with Result do
    begin
      N := High(Points);
      L := Length(Points[N]);
      SetLength(Points[N], L + 1);
    end;
    with Result.Points[N][L] do
    begin
      X := PX + LongDeltaX;
      Y := PY + LongDeltaY;
    end;
  end;

begin
  BuildNormals;

  if EdgeSharpness > 0.99 then
    EdgeSharpness := 0.99
  else if EdgeSharpness < 0 then
    EdgeSharpness := 0;

  D := Delta;
  E := Round(D * (1 - EdgeSharpness));

  Result := TPolygon32.Create;
  CopyPropertiesTo(Result);

  if Delta = 0 then
  begin
    // simply copy the data
    SetLength(Result.FPoints, Length(Points));
    for J := 0 to High(Points) do
      Result.Points[J] := Copy(Points[J], 0, Length(Points[J]));
    Exit;
  end;

  Result.Points := nil;

  for J := 0 to High(Points) do
  begin
    if Length(Points[J]) < 2 then Continue;

    Result.NewLine;

    for I := 0 to High(Points[J]) do
    begin
      with Points[J][I] do
      begin
        PX := X;
        PY := Y;
      end;

      with Normals[J][I] do
      begin
        BX := MulDiv(X, D, $10000);
        BY := MulDiv(Y, D, $10000);
      end;

      if (I > 0) or Closed then
      begin
        PrevI := I - 1;
        if PrevI < 0 then PrevI := High(Points[J]);
        with Normals[J][PrevI] do
        begin
          AX := MulDiv(X, D, $10000);
          AY := MulDiv(Y, D, $10000);
        end;

        if (I = High(Points[J])) and (not Closed) then AddPoint(AX, AY)
        else
        begin
          CX := AX + BX;
          CY := AY + BY;
          R := MulDiv(AX, CX, D) + MulDiv(AY, CY, D);
          if R > E then AddPoint(MulDiv(CX, D, R), MulDiv(CY, D, R))
          else
          begin
            AddPoint(AX, AY);
            AddPoint(BX, BY);
          end;
        end;
      end
      else AddPoint(BX, BY);
    end;
  end;
end;

procedure TPolygon32.NewLine;
begin
  SetLength(FPoints, Length(Points) + 1);
  Normals := nil;
end;

procedure TPolygon32.Offset(const Dx, Dy: TFixed);
var
  J, I: Integer;
begin
  for J := 0 to High(Points) do
    for I := 0 to High(Points[J]) do
      with Points[J][I] do
      begin
        Inc(X, Dx);
        Inc(Y, Dy);
      end;
end;

function TPolygon32.Outline: TPolygon32;
var
  J, I: Integer;
begin
  BuildNormals;

  Result := TPolygon32.Create;
  CopyPropertiesTo(Result);

  Result.Points := nil;

  for J := 0 to High(Points) do
  begin
    if Length(Points[J]) < 2 then Continue;

    if Closed then
    begin
      Result.NewLine;
      for I := 0 to High(Points[J]) do Result.Add(Points[J][I]);
      Result.NewLine;
      for I := High(Points[J]) downto 0 do Result.Add(Points[J][I]);
    end
    else // not closed
    begin
      Result.NewLine;
      for I := 0 to High(Points[J]) do Result.Add(Points[J][I]);
      for I := High(Points[J]) downto 0 do Result.Add(Points[J][I]);
    end;
  end;
end;

procedure TPolygon32.Transform(Transformation: TTransformation);
begin
  Points := TransformPoints(Points, Transformation);
end;

{ TBitmapPolygonFiller }

procedure TBitmapPolygonFiller.FillLineOpaque(Dst: PColor32; DstX, DstY,
  Length: Integer; AlphaValues: PColor32);
var
  PatternX, PatternY, X: Integer;
  OpaqueAlpha: TColor32;
  Src: PColor32;
  BlendMemEx: TBlendMemEx;
begin
  PatternX := (DstX - OffsetX) mod FPattern.Width;
  if PatternX < 0 then PatternX := (FPattern.Width + PatternX) mod FPattern.Width;
  PatternY := (DstY - OffsetY) mod FPattern.Height;
  if PatternY < 0 then PatternY := (FPattern.Height + PatternY) mod FPattern.Height;

  Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];

  if Assigned(AlphaValues) then
  begin
    OpaqueAlpha := TColor32($FF shl 24);
    BlendMemEx := BLEND_MEM_EX[FPattern.CombineMode]^;
    for X := DstX to DstX + Length - 1 do
    begin
      BlendMemEx(Src^ and $00FFFFFF or OpaqueAlpha, Dst^, AlphaValues^);
      Inc(Dst);  Inc(Src);  Inc(PatternX);
      if PatternX >= FPattern.Width then
      begin
        PatternX := 0;
        Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];
      end;
      Inc(AlphaValues);
    end
  end
  else
    for X := DstX to DstX + Length - 1 do
    begin
      Dst^ := Src^;
      Inc(Dst);  Inc(Src);  Inc(PatternX);
      if PatternX >= FPattern.Width then
      begin
        PatternX := 0;
        Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];
      end;
    end;
end;

procedure TBitmapPolygonFiller.FillLineBlend(Dst: PColor32; DstX, DstY, Length: Integer; AlphaValues: PColor32);
var
  PatternX, PatternY, X: Integer;
  Src: PColor32;
  BlendMemEx: TBlendMemEx;
  BlendMem: TBlendMem;
begin
  PatternX := (DstX - OffsetX) mod FPattern.Width;
  if PatternX < 0 then PatternX := (FPattern.Width + PatternX) mod FPattern.Width;
  PatternY := (DstY - OffsetY) mod FPattern.Height;
  if PatternY < 0 then PatternY := (FPattern.Height + PatternY) mod FPattern.Height;

  Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];

  if Assigned(AlphaValues) then
  begin
    BlendMemEx := BLEND_MEM_EX[FPattern.CombineMode]^;
    for X := DstX to DstX + Length - 1 do
    begin
      BlendMemEx(Src^, Dst^, AlphaValues^);
      Inc(Dst);  Inc(Src);  Inc(PatternX);
      if PatternX >= FPattern.Width then
      begin
        PatternX := 0;
        Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];
      end;
      Inc(AlphaValues);
    end
  end
  else
  begin
    BlendMem := BLEND_MEM[FPattern.CombineMode]^;
    for X := DstX to DstX + Length - 1 do
    begin
      BlendMem(Src^, Dst^);
      Inc(Dst);  Inc(Src);  Inc(PatternX);
      if PatternX >= FPattern.Width then
      begin
        PatternX := 0;
        Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];
      end;
    end;
  end;
end;

procedure TBitmapPolygonFiller.FillLineBlendMasterAlpha(Dst: PColor32; DstX, DstY,
  Length: Integer; AlphaValues: PColor32);
var
  PatternX, PatternY, X: Integer;
  Src: PColor32;
  BlendMemEx: TBlendMemEx;
begin
  PatternX := (DstX - OffsetX) mod FPattern.Width;
  if PatternX < 0 then PatternX := (FPattern.Width + PatternX) mod FPattern.Width;
  PatternY := (DstY - OffsetY) mod FPattern.Height;
  if PatternY < 0 then PatternY := (FPattern.Height + PatternY) mod FPattern.Height;

  Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];

  BlendMemEx := BLEND_MEM_EX[FPattern.CombineMode]^;

  if Assigned(AlphaValues) then
    for X := DstX to DstX + Length - 1 do
    begin
      BlendMemEx(Src^, Dst^, (AlphaValues^ * FPattern.MasterAlpha) div 255);
      Inc(Dst);  Inc(Src);  Inc(PatternX);
      if PatternX >= FPattern.Width then
      begin
        PatternX := 0;
        Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];
      end;
      Inc(AlphaValues);
    end
  else
    for X := DstX to DstX + Length - 1 do
    begin
      BlendMemEx(Src^, Dst^, FPattern.MasterAlpha);
      Inc(Dst);  Inc(Src);  Inc(PatternX);
      if PatternX >= FPattern.Width then
      begin
        PatternX := 0;
        Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];
      end;
    end;
end;

procedure TBitmapPolygonFiller.FillLineCustomCombine(Dst: PColor32; DstX, DstY,
  Length: Integer; AlphaValues: PColor32);
var
  PatternX, PatternY, X: Integer;
  Src: PColor32;
begin
  PatternX := (DstX - OffsetX) mod FPattern.Width;
  if PatternX < 0 then PatternX := (FPattern.Width + PatternX) mod FPattern.Width;
  PatternY := (DstY - OffsetY) mod FPattern.Height;
  if PatternY < 0 then PatternY := (FPattern.Height + PatternY) mod FPattern.Height;

  Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];

  if Assigned(AlphaValues) then
    for X := DstX to DstX + Length - 1 do
    begin
      FPattern.OnPixelCombine(Src^, Dst^, (AlphaValues^ * FPattern.MasterAlpha) div 255);
      Inc(Dst);  Inc(Src);  Inc(PatternX);
      if PatternX >= FPattern.Width then
      begin
        PatternX := 0;
        Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];
      end;
      Inc(AlphaValues);
    end
  else
    for X := DstX to DstX + Length - 1 do
    begin
      FPattern.OnPixelCombine(Src^, Dst^, FPattern.MasterAlpha);
      Inc(Dst);  Inc(Src);  Inc(PatternX);
      if PatternX >= FPattern.Width then
      begin
        PatternX := 0;
        Src := @FPattern.Bits[PatternX + PatternY * FPattern.Width];
      end;
    end;
end;

function TBitmapPolygonFiller.GetFillLine: TFillLineEvent;
begin
  if not Assigned(FPattern) then
  begin
    Result := nil;
  end
  else if FPattern.DrawMode = dmOpaque then
    Result := FillLineOpaque
  else if FPattern.DrawMode = dmBlend then
  begin
    if FPattern.MasterAlpha = 255 then
      Result := FillLineBlend
    else
      Result := FillLineBlendMasterAlpha;
  end
  else if (FPattern.DrawMode = dmCustom) and Assigned(FPattern.OnPixelCombine) then
  begin
    Result := FillLineCustomCombine;
  end
  else
    Result := nil;
end;

{ TSamplerFiller }

procedure TSamplerFiller.SampleLineOpaque(Dst: PColor32; DstX, DstY,
  Length: Integer; AlphaValues: PColor32);
var
  X: Integer;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[cmBlend]^;
  for X := DstX to DstX + Length - 1 do
  begin
    BlendMemEx(FGetSample(X, DstY) and $00FFFFFF or $FF000000, Dst^, AlphaValues^);
    EMMS;
    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

function TSamplerFiller.GetFillLine: TFillLineEvent;
begin
  Result := SampleLineOpaque;
end;

procedure TSamplerFiller.SetSampler(const Value: TCustomSampler);
begin
  FSampler := Value;
  FGetSample := FSampler.GetSampleInt;
end;

end.
