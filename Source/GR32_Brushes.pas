unit GR32_Brushes;

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
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

{-$define GR32_DEBUG_BRUSH}

uses
  Classes, GR32, GR32_Polygons, GR32_Transforms;

type
  TBooleanArray = array of boolean;

type
  TCustomBrush = class;
  TBrushClass = class of TCustomBrush;

// TODO: devise a common base class for TBrushCollection/TLayerCollection

  { TBrushCollection }
  TBrushCollection = class(TNotifiablePersistent)
  strict private
    FItems: TList;
    FOwner: TPersistent;
    function GetCount: Integer;
    function GetItem(Index: Integer): TCustomBrush;
    procedure SetItem(Index: Integer; const Value: TCustomBrush);
  protected
    procedure InsertItem(Item: TCustomBrush);
    procedure RemoveItem(Item: TCustomBrush);
    procedure MoveItem(OldIndex, NewIndex: integer);
  public
    constructor Create(AOwner: TPersistent);
    destructor Destroy; override;

    function Add(ItemClass: TBrushClass): TCustomBrush;
    procedure Clear;
    procedure Delete(Index: Integer);
    function Insert(Index: Integer; ItemClass: TBrushClass): TCustomBrush;
    function IndexOf(Item: TCustomBrush): integer;

    property Owner: TPersistent read FOwner;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TCustomBrush read GetItem write SetItem; default;
  end;

  { TCustomBrush }
  TCustomBrush = class(TNotifiablePersistent)
  strict private
    FBrushCollection: TBrushCollection;
    FVisible: Boolean;
    FBatchCount: integer;
    function GetIndex: Integer;
    procedure SetBrushCollection(const Value: TBrushCollection);
    procedure SetVisible(const Value: Boolean);
  protected
    procedure SetIndex(Value: Integer); virtual;
    procedure UpdateRenderer(Renderer: TCustomPolygonRenderer); virtual;

    function ProcessPolyPolygon(Renderer: TCustomPolygonRenderer; const Points: TArrayOfArrayOfFloatPoint;
      const ClipRect: TFloatRect; Transformation: TTransformation; Closed: Boolean): TArrayOfArrayOfFloatPoint; virtual;

    procedure RenderPolyPolygon(Renderer: TCustomPolygonRenderer; const Points: TArrayOfArrayOfFloatPoint;
      const ClipRect: TFloatRect; Transformation: TTransformation); virtual;

    procedure BeginPolygon; virtual;
    procedure EndPolygon(Renderer: TCustomPolygonRenderer; const Points: TArrayOfArrayOfFloatPoint;
      const ClipRect: TFloatRect; Transformation: TTransformation); virtual;
  public
    constructor Create(ABrushCollection: TBrushCollection); virtual;
    destructor Destroy; override;

    procedure Changed; override;

    // Single polygon, either open or closed
    procedure PolygonFS(Renderer: TCustomPolygonRenderer; const Points: TArrayOfFloatPoint;
      const ClipRect: TFloatRect; Transformation: TTransformation; Closed: Boolean); virtual;
    // Polypolygons, either all open or all closed
    procedure PolyPolygonFS(Renderer: TCustomPolygonRenderer; const Points: TArrayOfArrayOfFloatPoint;
      const ClipRect: TFloatRect; Transformation: TTransformation; Closed: Boolean); virtual;
    // Polypolygons, individually open or closed
    procedure PolyPolygonMixedFS(Renderer: TCustomPolygonRenderer; const Points: TArrayOfArrayOfFloatPoint;
      const ClipRect: TFloatRect; Transformation: TTransformation; Closed: TBooleanArray); virtual;

    property Index: Integer read GetIndex write SetIndex;
    property BrushCollection: TBrushCollection read FBrushCollection write SetBrushCollection;
    property Visible: Boolean read FVisible write SetVisible;
  end;


  { TSolidBrush }
  TSolidBrush = class(TCustomBrush)
  strict private
    FFillColor: TColor32;
    FFillMode: TPolyFillMode;
    FFiller: TCustomPolygonFiller;
    procedure SetFillColor(const Value: TColor32);
    procedure SetFillMode(const Value: TPolyFillMode);
    procedure SetFiller(const Value: TCustomPolygonFiller);
  protected
    procedure UpdateRenderer(Renderer: TCustomPolygonRenderer); override;
  public
    constructor Create(ABrushCollection: TBrushCollection); override;

    property FillColor: TColor32 read FFillColor write SetFillColor;
    property FillMode: TPolyFillMode read FFillMode write SetFillMode;
    property Filler: TCustomPolygonFiller read FFiller write SetFiller;
  end;

  { TNestedBrush }
  TNestedBrush = class(TSolidBrush)
  strict private
    FBrushes: TBrushCollection;
  public
    constructor Create(ABrushCollection: TBrushCollection); override;
    destructor Destroy; override;

    procedure PolygonFS(Renderer: TCustomPolygonRenderer; const Points: TArrayOfFloatPoint;
      const ClipRect: TFloatRect; Transformation: TTransformation; Closed: Boolean); override;
    procedure PolyPolygonFS(Renderer: TCustomPolygonRenderer; const Points: TArrayOfArrayOfFloatPoint;
      const ClipRect: TFloatRect; Transformation: TTransformation; Closed: Boolean); override;
    procedure PolyPolygonMixedFS(Renderer: TCustomPolygonRenderer; const Points: TArrayOfArrayOfFloatPoint;
      const ClipRect: TFloatRect; Transformation: TTransformation; Closed: TBooleanArray); override;

    property Brushes: TBrushCollection read FBrushes;
  end;

  { TStrokeBrush }
  TStrokeBrush = class(TSolidBrush)
  strict private
    FStrokeWidth: TFloat;
    FJoinStyle: TJoinStyle;
    FMiterLimit: TFloat;
    FEndStyle: TEndStyle;
    procedure SetStrokeWidth(const Value: TFloat);
    procedure SetEndStyle(const Value: TEndStyle);
    procedure SetJoinStyle(const Value: TJoinStyle);
    procedure SetMiterLimit(const Value: TFloat);
  protected
    function ProcessPolyPolygon(Renderer: TCustomPolygonRenderer; const Points: TArrayOfArrayOfFloatPoint;
      const ClipRect: TFloatRect; Transformation: TTransformation; Closed: Boolean): TArrayOfArrayOfFloatPoint; override;
  public
    constructor Create(BrushCollection: TBrushCollection); override;

    property StrokeWidth: TFloat read FStrokeWidth write SetStrokeWidth;
    property JoinStyle: TJoinStyle read FJoinStyle write SetJoinStyle;
    property EndStyle: TEndStyle read FEndStyle write SetEndStyle;
    property MiterLimit: TFloat read FMiterLimit write SetMiterLimit;
  end;

  { TGrowBrush }
  TGrowBrush = class(TNestedBrush)
  strict private
    FGrowAmount: TFloat;
    FJoinStyle: TJoinStyle;
    FMiterLimit: TFloat;
    procedure SetGrowAmount(const Value: TFloat);
    procedure SetJoinStyle(const Value: TJoinStyle);
    procedure SetMiterLimit(const Value: TFloat);
  protected
    function ProcessPolyPolygon(Renderer: TCustomPolygonRenderer; const Points: TArrayOfArrayOfFloatPoint;
      const ClipRect: TFloatRect; Transformation: TTransformation; Closed: Boolean): TArrayOfArrayOfFloatPoint; override;
  public
    constructor Create(BrushCollection: TBrushCollection); override;

    property GrowAmount: TFloat read FGrowAmount write SetGrowAmount;
    property JoinStyle: TJoinStyle read FJoinStyle write SetJoinStyle;
    property MiterLimit: TFloat read FMiterLimit write SetMiterLimit;
  end;

  { TDashedBrush }
  TDashedBrush = class(TStrokeBrush)
  strict private
    FDashOffset: TFloat;
    FDashArray: TArrayOfFloat;
    procedure SetDashOffset(const Value: TFloat);
    procedure DoSetDashArray(const ADashArray: TArrayOfFloat); // TODO :Rename once SetDashArray is removed
  protected
    function ProcessPolyPolygon(Renderer: TCustomPolygonRenderer; const Points: TArrayOfArrayOfFloatPoint;
      const ClipRect: TFloatRect; Transformation: TTransformation; Closed: Boolean): TArrayOfArrayOfFloatPoint; override;
  public
    procedure SetDashArray(const ADashArray: TArrayOfFloat); deprecated 'Use DashArray property';

    property DashArray: TArrayOfFloat read FDashArray write DoSetDashArray;
    property DashOffset: TFloat read FDashOffset write SetDashOffset;
  end;

implementation

uses
  GR32_VectorUtils;

{ TBrushCollection }

function TBrushCollection.Add(ItemClass: TBrushClass): TCustomBrush;
begin
  Result := ItemClass.Create(Self);
  Result.Index := FItems.Count - 1;
  //Notify(lnLayerAdded, Result, Result.Index);
end;

procedure TBrushCollection.Clear;
begin
  BeginUpdate;
  try
    while FItems.Count > 0 do TCustomBrush(FItems.Last).Free;
    //Notify(lnCleared, nil, 0);
  finally
    EndUpdate;
  end;
end;

constructor TBrushCollection.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
  FItems := TList.Create;
end;

procedure TBrushCollection.Delete(Index: Integer);
begin
  TCustomBrush(FItems[Index]).Free;
end;

destructor TBrushCollection.Destroy;
begin
  if Assigned(FItems) then Clear;
  FItems.Free;
  inherited;
end;

function TBrushCollection.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TBrushCollection.GetItem(Index: Integer): TCustomBrush;
begin
  Result := FItems[Index];
end;

function TBrushCollection.IndexOf(Item: TCustomBrush): integer;
begin
  Result := FItems.IndexOf(Item);
end;

function TBrushCollection.Insert(Index: Integer;
  ItemClass: TBrushClass): TCustomBrush;
begin
  BeginUpdate;
  try
    Result := Add(ItemClass);
    Result.Index := Index;
  finally
    EndUpdate;
  end;
end;

procedure TBrushCollection.InsertItem(Item: TCustomBrush);
begin
  BeginUpdate;
  try
    FItems.Add(Item);
    Changed;
  finally
    EndUpdate;
  end;
end;

procedure TBrushCollection.MoveItem(OldIndex, NewIndex: integer);
begin
  FItems.Move(OldIndex, NewIndex);
end;

procedure TBrushCollection.RemoveItem(Item: TCustomBrush);
var
  Index: Integer;
begin
  Index := FItems.IndexOf(Item);
  if (Index < 0) then
    exit;

  BeginUpdate;
  try
    FItems.Delete(Index);
    Changed;
  finally
    EndUpdate;
  end;
end;

procedure TBrushCollection.SetItem(Index: Integer; const Value: TCustomBrush);
begin
  TCollectionItem(FItems[Index]).Assign(Value);
end;


{ TCustomBrush }

constructor TCustomBrush.Create(ABrushCollection: TBrushCollection);
begin
  inherited Create;
  BrushCollection := ABrushCollection;
  FVisible := True;
end;

destructor TCustomBrush.Destroy;
begin
  SetBrushCollection(nil);
  inherited;
end;

procedure TCustomBrush.BeginPolygon;
begin
  Assert(FBatchCount = 0);
  Inc(FBatchCount);
end;

procedure TCustomBrush.EndPolygon(Renderer: TCustomPolygonRenderer; const Points: TArrayOfArrayOfFloatPoint;
  const ClipRect: TFloatRect; Transformation: TTransformation);
begin
  Assert(FBatchCount = 1);
  Dec(FBatchCount);
  RenderPolyPolygon(Renderer, Points, ClipRect, Transformation);
end;

procedure TCustomBrush.Changed;
begin
  inherited;

  if (LockUpdateCount = 0) and (FBrushCollection <> nil) then
    FBrushCollection.Changed;
end;

function TCustomBrush.GetIndex: Integer;
begin
  if (FBrushCollection <> nil) then
    Result := FBrushCollection.IndexOf(Self)
  else
    Result := -1;
end;

function TCustomBrush.ProcessPolyPolygon(Renderer: TCustomPolygonRenderer; const Points: TArrayOfArrayOfFloatPoint;
  const ClipRect: TFloatRect; Transformation: TTransformation; Closed: Boolean): TArrayOfArrayOfFloatPoint;
begin
  Result := Points;
end;

procedure TCustomBrush.RenderPolyPolygon(Renderer: TCustomPolygonRenderer; const Points: TArrayOfArrayOfFloatPoint;
  const ClipRect: TFloatRect; Transformation: TTransformation);
begin
  UpdateRenderer(Renderer);
  Renderer.PolyPolygonFS(Points, ClipRect, Transformation);
{$ifdef GR32_DEBUG_BRUSH}
  PolyPolylineFS(TPolygonRenderer32(Renderer).Bitmap, Points, clBlue32, True);
{$endif GR32_DEBUG_BRUSH}
end;

procedure TCustomBrush.PolygonFS(Renderer: TCustomPolygonRenderer; const Points: TArrayOfFloatPoint; const ClipRect: TFloatRect;
  Transformation: TTransformation; Closed: Boolean);
begin
  PolyPolygonFS(Renderer, [Points], ClipRect, Transformation, Closed);
end;

procedure TCustomBrush.PolyPolygonFS(Renderer: TCustomPolygonRenderer; const Points: TArrayOfArrayOfFloatPoint;
  const ClipRect: TFloatRect; Transformation: TTransformation; Closed: Boolean);
var
  Buffer: TArrayOfArrayOfFloatPoint;
begin
  BeginPolygon;
  Buffer := ProcessPolyPolygon(Renderer, Points, ClipRect, Transformation, Closed);
  EndPolygon(Renderer, Buffer, ClipRect, Transformation);
end;

procedure TCustomBrush.PolyPolygonMixedFS(Renderer: TCustomPolygonRenderer; const Points: TArrayOfArrayOfFloatPoint;
  const ClipRect: TFloatRect; Transformation: TTransformation; Closed: TBooleanArray);
var
  Start, Next: Integer;
  i: integer;
  Buffer: TArrayOfArrayOfFloatPoint;
  RunBuffer: TArrayOfArrayOfFloatPoint;
  RunClosed: boolean;
begin
  if (Length(Points) = 0) then
    exit;

  // Assume some paths are closed, some are open
  BeginPolygon;
  begin
    Buffer := nil;
    Start := 0;
    // Find contiguous chunks of path with same "closedness"
    while (Start < Length(Points)) do
    begin
      RunClosed := Closed[Start];
      // Find a run of same "closedness"
      Next := Start+1;
      while (Next < Length(Points)) and (Closed[Next] = RunClosed) do
        Inc(Next);

      // Run goes from Start to Next-1
      SetLength(RunBuffer, Next-Start);
      i := 0;
      while (Start < Next) do
      begin
        RunBuffer[i] := Points[Start];
        Inc(Start);
        Inc(i);
      end;

      // Process this run
      Buffer := Buffer + ProcessPolyPolygon(Renderer, RunBuffer, ClipRect, Transformation, RunClosed);
    end;
  end;
  EndPolygon(Renderer, Buffer, ClipRect, Transformation);
end;

procedure TCustomBrush.SetBrushCollection(const Value: TBrushCollection);
var
  OldBrushCollection: TBrushCollection;
begin
  if FBrushCollection <> Value then
  begin
    OldBrushCollection := FBrushCollection;
    FBrushCollection := nil;

    if (OldBrushCollection <> nil) then
      OldBrushCollection.RemoveItem(Self);

    FBrushCollection := Value;

    if (FBrushCollection <> nil) then
      FBrushCollection.InsertItem(Self);
  end;
end;

procedure TCustomBrush.SetIndex(Value: Integer);
var
  CurIndex: Integer;
begin
  CurIndex := GetIndex;
  if (CurIndex < 0) or (CurIndex = Value) then
    exit;

  if (Value < 0) then
    Value := 0;
  if (Value >= BrushCollection.Count) then
    Value := BrushCollection.Count - 1;

  if (Value = CurIndex) then
    exit;

  if Visible then
    BrushCollection.BeginUpdate;
  try
    BrushCollection.MoveItem(CurIndex, Value);
  finally
    if Visible then
      BrushCollection.EndUpdate;
  end;
end;

procedure TCustomBrush.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

procedure TCustomBrush.UpdateRenderer(Renderer: TCustomPolygonRenderer);
begin
end;


{ TNestedBrush }

constructor TNestedBrush.Create(ABrushCollection: TBrushCollection);
begin
  inherited;
  FBrushes := TBrushCollection.Create(Self);
end;

destructor TNestedBrush.Destroy;
begin
  FBrushes.Free;
  inherited;
end;

procedure TNestedBrush.PolygonFS(Renderer: TCustomPolygonRenderer; const Points: TArrayOfFloatPoint;
  const ClipRect: TFloatRect; Transformation: TTransformation; Closed: Boolean);
var
  I: Integer;
begin
  for I := 0 to FBrushes.Count - 1 do
    if FBrushes[I].Visible then
      FBrushes[I].PolygonFS(Renderer, Points, ClipRect, Transformation, Closed);
end;

procedure TNestedBrush.PolyPolygonFS(Renderer: TCustomPolygonRenderer; const Points: TArrayOfArrayOfFloatPoint;
  const ClipRect: TFloatRect; Transformation: TTransformation; Closed: Boolean);
var
  I: Integer;
begin
  for I := 0 to FBrushes.Count - 1 do
    if FBrushes[I].Visible then
      FBrushes[I].PolyPolygonFS(Renderer, Points, ClipRect, Transformation, Closed);
end;

procedure TNestedBrush.PolyPolygonMixedFS(Renderer: TCustomPolygonRenderer; const Points: TArrayOfArrayOfFloatPoint;
  const ClipRect: TFloatRect; Transformation: TTransformation; Closed: TBooleanArray);
var
  I: Integer;
begin
  for I := 0 to FBrushes.Count - 1 do
    if FBrushes[I].Visible then
      FBrushes[I].PolyPolygonMixedFS(Renderer, Points, ClipRect, Transformation, Closed);
end;




{ TSolidBrush }

constructor TSolidBrush.Create(ABrushCollection: TBrushCollection);
begin
  inherited;
  FFillColor := clBlack32;
end;

procedure TSolidBrush.SetFillColor(const Value: TColor32);
begin
  if FFillColor <> Value then
  begin
    FFillColor := Value;
    Changed;
  end;
end;

procedure TSolidBrush.SetFiller(const Value: TCustomPolygonFiller);
begin
  if FFiller <> Value then
  begin
    FFiller := Value;
    Changed;
  end;
end;

procedure TSolidBrush.SetFillMode(const Value: TPolyFillMode);
begin
  if FFillMode <> Value then
  begin
    FFillMode := Value;
    Changed;
  end;
end;

procedure TSolidBrush.UpdateRenderer(Renderer: TCustomPolygonRenderer);
var
  R: TPolygonRenderer32;
begin
  R := Renderer as TPolygonRenderer32;
  R.Color := FillColor;
  R.FillMode := FillMode;
  R.Filler := Filler;
end;


{ TStrokeBrush }

constructor TStrokeBrush.Create(BrushCollection: TBrushCollection);
begin
  inherited;

  BeginLockUpdate;
  FillMode := pfWinding;
  EndLockUpdate;

  FStrokeWidth := 1;
  FMiterLimit := DEFAULT_MITER_LIMIT;
end;

function TStrokeBrush.ProcessPolyPolygon(Renderer: TCustomPolygonRenderer; const Points: TArrayOfArrayOfFloatPoint;
  const ClipRect: TFloatRect; Transformation: TTransformation; Closed: Boolean): TArrayOfArrayOfFloatPoint;
begin
  Result := BuildPolyPolyLine(Points, Closed, StrokeWidth, JoinStyle, EndStyle, MiterLimit);
  Result := inherited ProcessPolyPolygon(Renderer, Result, ClipRect, Transformation, True);

{$ifdef GR32_DEBUG_BRUSH}
  PolyPolylineFS(TPolygonRenderer32(Renderer).Bitmap, Points, clRed32, Closed);
{$endif GR32_DEBUG_BRUSH}
end;

procedure TStrokeBrush.SetEndStyle(const Value: TEndStyle);
begin
  if FEndStyle <> Value then
  begin
    FEndStyle := Value;
    Changed;
  end;
end;

procedure TStrokeBrush.SetJoinStyle(const Value: TJoinStyle);
begin
  if FJoinStyle <> Value then
  begin
    FJoinStyle := Value;
    Changed;
  end;
end;

procedure TStrokeBrush.SetMiterLimit(const Value: TFloat);
begin
  if FMiterLimit <> Value then
  begin
    FMiterLimit := Value;
    Changed;
  end;
end;

procedure TStrokeBrush.SetStrokeWidth(const Value: TFloat);
begin
  if FStrokeWidth <> Value then
  begin
    FStrokeWidth := Value;
    Changed;
  end;
end;


{ TDashedBrush }

procedure TDashedBrush.DoSetDashArray(const ADashArray: TArrayOfFloat);
begin
  FDashArray := Copy(ADashArray);
  Changed;
end;

function TDashedBrush.ProcessPolyPolygon(Renderer: TCustomPolygonRenderer; const Points: TArrayOfArrayOfFloatPoint;
  const ClipRect: TFloatRect; Transformation: TTransformation; Closed: Boolean): TArrayOfArrayOfFloatPoint;
var
  I: Integer;
begin
  if (Length(FDashArray) > 0) then
  begin
    Result := nil;
    for I := 0 to High(Points) do
      Result := Result + BuildDashedLine(Points[I], FDashArray, FDashOffset, Closed);

    Result := inherited ProcessPolyPolygon(Renderer, Result, ClipRect, Transformation, False);
  end else
    Result := inherited ProcessPolyPolygon(Renderer, Points, ClipRect, Transformation, Closed);
end;

procedure TDashedBrush.SetDashArray(const ADashArray: TArrayOfFloat);
begin
  DoSetDashArray(ADashArray);
end;

procedure TDashedBrush.SetDashOffset(const Value: TFloat);
begin
  if FDashOffset <> Value then
  begin
    FDashOffset := Value;
    Changed;
  end;
end;


{ TGrowBrush }

constructor TGrowBrush.Create(BrushCollection: TBrushCollection);
begin
  inherited;
  FMiterLimit := DEFAULT_MITER_LIMIT;
end;

function TGrowBrush.ProcessPolyPolygon(Renderer: TCustomPolygonRenderer; const Points: TArrayOfArrayOfFloatPoint;
  const ClipRect: TFloatRect; Transformation: TTransformation; Closed: Boolean): TArrayOfArrayOfFloatPoint;
var
  I: Integer;
begin
  SetLength(Result, Length(Points));
  for I := 0 to High(Points) do
    Result[I] := Grow(Points[I], GrowAmount, JoinStyle, Closed, MiterLimit);

  Result := inherited ProcessPolyPolygon(Renderer, Result, ClipRect, Transformation, Closed);
end;

procedure TGrowBrush.SetGrowAmount(const Value: TFloat);
begin
  if FGrowAmount <> Value then
  begin
    FGrowAmount := Value;
    Changed;
  end;
end;

procedure TGrowBrush.SetJoinStyle(const Value: TJoinStyle);
begin
  if FJoinStyle <> Value then
  begin
    FJoinStyle := Value;
    Changed;
  end;
end;

procedure TGrowBrush.SetMiterLimit(const Value: TFloat);
begin
  if FMiterLimit <> Value then
  begin
    FMiterLimit := Value;
    Changed;
  end;
end;

end.
