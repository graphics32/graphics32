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
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
  Classes, GR32, GR32_Polygons, GR32_Transforms;

type
  TCustomBrush = class;
  TBrushClass = class of TCustomBrush;

// TODO: devise a common base class for TBrushCollection/TLayerCollection

  { TBrushCollection }
  TBrushCollection = class(TNotifiablePersistent)
  private
    FItems: TList;
    FOwner: TPersistent;
    procedure InsertItem(Item: TCustomBrush);
    procedure RemoveItem(Item: TCustomBrush);
    function GetCount: Integer;
    function GetItem(Index: Integer): TCustomBrush;
    procedure SetItem(Index: Integer; const Value: TCustomBrush);
  public
    constructor Create(AOwner: TPersistent);
    destructor Destroy; override;
    function Add(ItemClass: TBrushClass): TCustomBrush;
    procedure Clear;
    procedure Delete(Index: Integer);
    function Insert(Index: Integer; ItemClass: TBrushClass): TCustomBrush;
    property Owner: TPersistent read FOwner;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TCustomBrush read GetItem write SetItem; default;
  end;

  { TCustomBrush }
  TCustomBrush = class(TNotifiablePersistent)
  private
    FBrushCollection: TBrushCollection;
    FVisible: Boolean;
    function GetIndex: Integer;
    procedure SetBrushCollection(const Value: TBrushCollection);
    procedure SetVisible(const Value: Boolean);
  protected
    procedure SetIndex(Value: Integer); virtual;
    procedure UpdateRenderer(Renderer: TCustomPolygonRenderer); virtual;
  public
    constructor Create(ABrushCollection: TBrushCollection); virtual;
    destructor Destroy; override;
    procedure Changed; override;
    procedure PolyPolygonFS(Renderer: TCustomPolygonRenderer;
      const Points: TArrayOfArrayOfFloatPoint;
      const ClipRect: TFloatRect; Transformation: TTransformation;
      Closed: Boolean); virtual;
    procedure PolygonFS(Renderer: TCustomPolygonRenderer;
      const Points: TArrayOfFloatPoint;
      const ClipRect: TFloatRect; Transformation: TTransformation;
      Closed: Boolean); virtual;
    property Index: Integer read GetIndex write SetIndex;
    property BrushCollection: TBrushCollection read FBrushCollection write SetBrushCollection;
    property Visible: Boolean read FVisible write SetVisible;
  end;


  { TSolidBrush }
  TSolidBrush = class(TCustomBrush)
  private
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
  private
    FBrushes: TBrushCollection;
  public
    constructor Create(ABrushCollection: TBrushCollection); override;
    destructor Destroy; override;
    procedure PolyPolygonFS(Renderer: TCustomPolygonRenderer;
      const Points: TArrayOfArrayOfFloatPoint;
      const ClipRect: TFloatRect; Transformation: TTransformation;
      Closed: Boolean); override;
    procedure PolygonFS(Renderer: TCustomPolygonRenderer;
      const Points: TArrayOfFloatPoint;
      const ClipRect: TFloatRect; Transformation: TTransformation;
      Closed: Boolean); override;
    property Brushes: TBrushCollection read FBrushes;
  end;

  { TStrokeBrush }
  TStrokeBrush = class(TSolidBrush)
  private
    FStrokeWidth: TFloat;
    FJoinStyle: TJoinStyle;
    FMiterLimit: TFloat;
    FEndStyle: TEndStyle;
    procedure SetStrokeWidth(const Value: TFloat);
    procedure SetEndStyle(const Value: TEndStyle);
    procedure SetJoinStyle(const Value: TJoinStyle);
    procedure SetMiterLimit(const Value: TFloat);
  public
    constructor Create(BrushCollection: TBrushCollection); override;
    procedure PolyPolygonFS(Renderer: TCustomPolygonRenderer;
      const Points: TArrayOfArrayOfFloatPoint;
      const ClipRect: TFloatRect; Transformation: TTransformation;
      Closed: Boolean); override;
    property StrokeWidth: TFloat read FStrokeWidth write SetStrokeWidth;
    property JoinStyle: TJoinStyle read FJoinStyle write SetJoinStyle;
    property EndStyle: TEndStyle read FEndStyle write SetEndStyle;
    property MiterLimit: TFloat read FMiterLimit write SetMiterLimit;
  end;

  { TGrowBrush }
  TGrowBrush = class(TNestedBrush)
  private
    FGrowAmount: TFloat;
    FJoinStyle: TJoinStyle;
    FMiterLimit: TFloat;
    procedure SetGrowAmount(const Value: TFloat);
    procedure SetJoinStyle(const Value: TJoinStyle);
    procedure SetMiterLimit(const Value: TFloat);
  public
    constructor Create(BrushCollection: TBrushCollection); override;
    procedure PolyPolygonFS(Renderer: TCustomPolygonRenderer;
      const Points: TArrayOfArrayOfFloatPoint;
      const ClipRect: TFloatRect; Transformation: TTransformation;
      Closed: Boolean); override;
    property GrowAmount: TFloat read FGrowAmount write SetGrowAmount;
    property JoinStyle: TJoinStyle read FJoinStyle write SetJoinStyle;
    property MiterLimit: TFloat read FMiterLimit write SetMiterLimit;
  end;

  { TDashedBrush }
  TDashedBrush = class(TStrokeBrush)
  private
    FDashOffset: TFloat;
    FDashArray: TArrayOfFloat;
    procedure SetDashOffset(const Value: TFloat);
  public
    procedure PolyPolygonFS(Renderer: TCustomPolygonRenderer;
      const Points: TArrayOfArrayOfFloatPoint;
      const ClipRect: TFloatRect; Transformation: TTransformation;
      Closed: Boolean); override;
    procedure SetDashArray(const ADashArray: array of TFloat);
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

function TBrushCollection.Insert(Index: Integer;
  ItemClass: TBrushClass): TCustomBrush;
begin
  BeginUpdate;
  try
    Result := Add(ItemClass);
    Result.Index := Index;
    //Notify(lnLayerInserted, Result, Index);
  finally
    EndUpdate;
  end;
end;

procedure TBrushCollection.InsertItem(Item: TCustomBrush);
(*
var
  Index: Integer;
*)
begin
  BeginUpdate;
  try
    {Index := } FItems.Add(Item);
    Item.FBrushCollection := Self;
    //Notify(lnLayerAdded, Item, Index);
  finally
    EndUpdate;
  end;
end;

procedure TBrushCollection.RemoveItem(Item: TCustomBrush);
var
  Index: Integer;
begin
  BeginUpdate;
  try
    Index := FItems.IndexOf(Item);
    if Index >= 0 then
    begin
      FItems.Delete(Index);
      Item.FBrushCollection := nil;
      //Notify(lnLayerDeleted, Item, Index);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TBrushCollection.SetItem(Index: Integer; const Value: TCustomBrush);
begin
  TCollectionItem(FItems[Index]).Assign(Value);
end;


{ TCustomBrush }

procedure TCustomBrush.Changed;
begin
  inherited;
  if Assigned(FBrushCollection) then
    FBrushCollection.Changed;
end;

constructor TCustomBrush.Create(ABrushCollection: TBrushCollection);
begin
  BrushCollection := ABrushCollection;
  FVisible := True;
end;

destructor TCustomBrush.Destroy;
begin
  SetBrushCollection(nil);
  inherited;
end;

function TCustomBrush.GetIndex: Integer;
begin
  if Assigned(FBrushCollection) then
    Result := FBrushCollection.FItems.IndexOf(Self)
  else
    Result := -1;
end;

procedure TCustomBrush.PolygonFS(Renderer: TCustomPolygonRenderer;
  const Points: TArrayOfFloatPoint;
  const ClipRect: TFloatRect; Transformation: TTransformation; Closed: Boolean);
begin
  //PolyPolygonFS(Renderer, PolyPolygon(Points), ClipRect, Transformation, Closed);
  //Renderer.PolygonFS(Points, ClipRect, Transformation);
  PolyPolygonFS(Renderer, PolyPolygon(Points), ClipRect, Transformation, Closed);
end;

procedure TCustomBrush.PolyPolygonFS(Renderer: TCustomPolygonRenderer;
  const Points: TArrayOfArrayOfFloatPoint;
  const ClipRect: TFloatRect; Transformation: TTransformation; Closed: Boolean);
begin
  UpdateRenderer(Renderer);
  Renderer.PolyPolygonFS(Points, ClipRect, Transformation);
end;

procedure TCustomBrush.SetBrushCollection(const Value: TBrushCollection);
begin
  if FBrushCollection <> Value then
  begin
    if Assigned(FBrushCollection) then
      FBrushCollection.RemoveItem(Self);
    if Assigned(Value) then
      Value.InsertItem(Self);
  end;
end;

procedure TCustomBrush.SetIndex(Value: Integer);
var
  CurIndex: Integer;
begin
  CurIndex := GetIndex;
  if (CurIndex >= 0) and (CurIndex <> Value) then
    with FBrushCollection do
    begin
      if Value < 0 then Value := 0;
      if Value >= Count then Value := Count - 1;
      if Value <> CurIndex then
      begin
        if Visible then BeginUpdate;
        try
          FBrushCollection.FItems.Move(CurIndex, Value);
        finally
          if Visible then EndUpdate;
        end;
      end;
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

procedure TNestedBrush.PolygonFS(Renderer: TCustomPolygonRenderer;
  const Points: TArrayOfFloatPoint; const ClipRect: TFloatRect;
  Transformation: TTransformation; Closed: Boolean);
var
  I: Integer;
begin
  for I := 0 to FBrushes.Count - 1 do
    if FBrushes[I].Visible then
      FBrushes[I].PolygonFS(Renderer, Points, ClipRect, Transformation, Closed);
end;

procedure TNestedBrush.PolyPolygonFS(Renderer: TCustomPolygonRenderer;
  const Points: TArrayOfArrayOfFloatPoint; const ClipRect: TFloatRect;
  Transformation: TTransformation; Closed: Boolean);
var
  I: Integer;
begin
  for I := 0 to FBrushes.Count - 1 do
    if FBrushes[I].Visible then
      FBrushes[I].PolyPolygonFS(Renderer, Points, ClipRect, Transformation, Closed);
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
  FStrokeWidth := 1;
  FFillMode := pfWinding;
  FMiterLimit := DEFAULT_MITER_LIMIT;
end;

procedure TStrokeBrush.PolyPolygonFS(Renderer: TCustomPolygonRenderer;
  const Points: TArrayOfArrayOfFloatPoint; const ClipRect: TFloatRect;
  Transformation: TTransformation; Closed: Boolean);
var
  APoints: TArrayOfArrayOfFloatPoint;
begin
  APoints := BuildPolyPolyLine(Points, Closed, StrokeWidth, JoinStyle,
    EndStyle, MiterLimit);
  inherited PolyPolygonFS(Renderer, APoints, ClipRect, Transformation, Closed);
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

procedure TDashedBrush.PolyPolygonFS(Renderer: TCustomPolygonRenderer;
  const Points: TArrayOfArrayOfFloatPoint; const ClipRect: TFloatRect;
  Transformation: TTransformation; Closed: Boolean);
var
  I: Integer;
begin
  for I := 0 to High(Points) do
    inherited PolyPolygonFS(
      Renderer, BuildDashedLine(Points[I], FDashArray, FDashOffset, Closed),
      ClipRect, Transformation, False);
end;

procedure TDashedBrush.SetDashArray(const ADashArray: array of TFloat);
var
  L: Integer;
begin
  L := Length(ADashArray);
  SetLength(FDashArray, L);
  Move(ADashArray[0], FDashArray[0], L * SizeOf(TFloat));
  Changed;
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

procedure TGrowBrush.PolyPolygonFS(Renderer: TCustomPolygonRenderer;
  const Points: TArrayOfArrayOfFloatPoint; const ClipRect: TFloatRect;
  Transformation: TTransformation; Closed: Boolean);
var
  I: Integer;
  APoints: TArrayOfArrayOfFloatPoint;
begin
  SetLength(APoints, Length(Points));
  for I := 0 to High(Points) do
    APoints[I] := Grow(Points[I], GrowAmount, JoinStyle, Closed, MiterLimit);
  inherited PolyPolygonFS(Renderer, APoints, ClipRect, Transformation, True);
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
