unit GR32_Layers;

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
 * The Original Code is Graphics32
 *
 * The Initial Developer of the Original Code is
 * Alex A. Denisov
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2004
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$INCLUDE GR32.inc}

uses
{$IFDEF CLX}
  Qt, Types, QControls, QGraphics, QForms,
  {$IFDEF LINUX}Libc, {$ENDIF}
  {$IFDEF MSWINDOWS}Windows, {$ENDIF}
{$ELSE}
  Windows, Controls, Graphics, Forms,
{$ENDIF}
  Classes, SysUtils, GR32;

const
  { Layer Options Bits }
  LOB_VISIBLE           = $80000000; // 31-st bit
  LOB_GDI_OVERLAY       = $40000000; // 30-th bit
  LOB_MOUSE_EVENTS      = $20000000; // 29-th bit
  LOB_NO_UPDATE         = $10000000; // 28-th bit
  LOB_NO_CAPTURE        = $08000000; // 27-th bit
  LOB_RESERVED_26       = $04000000; // 26-th bit
  LOB_RESERVED_25       = $02000000; // 25-th bit
  LOB_RESERVED_24       = $01000000; // 24-th bit
  LOB_RESERVED_MASK     = $FF000000;

type
  TCustomLayer = class;
  TPositionedLayer = class;
  TLayerClass = class of TCustomLayer;

  { TCoordXForm - transformations from bitmap image to buffer origin }
  TCoordXForm = record
    ScaleX: TFixed;       // bitmap image to buf
    ScaleY: TFixed;       
    ShiftX: Integer;
    ShiftY: Integer;
    RevScaleX: TFixed;
    RevScaleY: TFixed;
  end;
  PCoordXForm = ^TCoordXForm;

  TLayerCollection = class(TPersistent)
  private
    FCoordXForm: PCoordXForm;
    FItems: TList;
    FMouseEvents: Boolean;
    FMouseListener: TCustomLayer;
    FUpdateCount: Integer;
    FOwner: TComponent;
    FOnChanging: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FOnGDIUpdate: TNotifyEvent;
    function GetCount: Integer;
    procedure InsertItem(Item: TCustomLayer);
    procedure RemoveItem(Item: TCustomLayer);
    procedure SetMouseEvents(Value: Boolean);
    procedure SetMouseListener(Value: TCustomLayer);
  protected
    procedure BeginUpdate;
    procedure Changed;
    procedure Changing;
    procedure EndUpdate;
    function  FindLayerAtPos(X, Y: Integer; OptionsMask: Cardinal): TCustomLayer;
    function  GetItem(Index: Integer): TCustomLayer;
    function  GetOwner: TPersistent; override;
    procedure GDIUpdate;
    procedure SetItem(Index: Integer; Value: TCustomLayer);
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): TCustomLayer;
    function MouseMove(Shift: TShiftState; X, Y: Integer): TCustomLayer;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): TCustomLayer;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnGDIUpdate: TNotifyEvent read FOnGDIUpdate write FOnGDIUpdate;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    function  Add(ItemClass: TLayerClass): TCustomLayer;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    procedure Delete(Index: Integer);
    function  Insert(Index: Integer; ItemClass: TLayerClass): TCustomLayer;
    property Count: Integer read GetCount;
    property CoordXForm: PCoordXForm read FCoordXForm write FCoordXForm;
    property Owner: TComponent read FOwner;
    property Items[Index: Integer]: TCustomLayer read GetItem write SetItem; default;
    property MouseListener: TCustomLayer read FMouseListener write SetMouseListener;
    property MouseEvents: Boolean read FMouseEvents write SetMouseEvents;
  end;

  TLayerState = (lsMouseLeft, lsMouseRight, lsMouseMiddle);
  TLayerStates = set of TLayerState;

  TPaintLayerEvent = procedure(Sender: TObject; Buffer: TBitmap32) of object;
  THitTestEvent = procedure(Sender: TObject; X, Y: Integer; var Passed: Boolean) of object;

  TCustomLayer = class(TPersistent)
  private
    FCursor: TCursor;
    FFreeNotifies: TList;
    FLayerCollection: TLayerCollection;
    FLayerStates: TLayerStates;
    FLayerOptions: Cardinal;
    FOnHitTest: THitTestEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseUp: TMouseEvent;
    FOnPaint: TPaintLayerEvent;
    FTag: Integer;
    FOnDestroy: TNotifyEvent;
    function  GetIndex: Integer;
    function  GetMouseEvents: Boolean;
    function  GetVisible: Boolean;
    procedure SetCursor(Value: TCursor);
    procedure SetLayerCollection(Value: TLayerCollection);
    procedure SetLayerOptions(Value: Cardinal);
    procedure SetMouseEvents(Value: Boolean);
    procedure SetVisible(Value: Boolean);
  protected
    procedure AddNotification(ALayer: TCustomLayer);
    procedure Changed;
    procedure Changing;
    function  DoHitTest(X, Y: Integer): Boolean; virtual;
    procedure DoPaint(Buffer: TBitmap32);
    function  GetOwner: TPersistent; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure Notification(ALayer: TCustomLayer); virtual;
    procedure Paint(Buffer: TBitmap32); virtual;
    procedure PaintGDI(Canvas: TCanvas); virtual;
    procedure RemoveNotification(ALayer: TCustomLayer);
    procedure SetIndex(Value: Integer); virtual;
  public
    constructor Create(ALayerCollection: TLayerCollection); virtual;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    procedure BringToFront;
    function  HitTest(X, Y: Integer): Boolean;
    procedure SendToBack;
    procedure SetAsMouseListener;
    property Cursor: TCursor read FCursor write SetCursor;
    property Index: Integer read GetIndex write SetIndex;
    property LayerCollection: TLayerCollection read FLayerCollection write SetLayerCollection;
    property LayerOptions: Cardinal read FLayerOptions write SetLayerOptions;
    property LayerStates: TLayerStates read FLayerStates;
    property MouseEvents: Boolean read GetMouseEvents write SetMouseEvents;
    property Tag: Integer read FTag write FTag;
    property Visible: Boolean read GetVisible write SetVisible;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnHitTest: THitTestEvent read FOnHitTest write FOnHitTest;
    property OnPaint: TPaintLayerEvent read FOnPaint write FOnPaint;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
  end;

  TPositionedLayer = class(TCustomLayer)
  private
    FLocation: TFloatRect;
    FScaled: Boolean;
    procedure SetLocation(const Value: TFloatRect);
    procedure SetScaled(Value: Boolean);
  protected
    function DoHitTest(X, Y: Integer): Boolean; override;
    procedure DoSetLocation(const NewLocation: TFloatRect); virtual;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    function GetAdjustedLocation: TFloatRect;
    property Location: TFloatRect read FLocation write SetLocation;
    property Scaled: Boolean read FScaled write SetScaled;
  end;

  TBitmapLayer = class(TPositionedLayer)
  private
    FBitmap: TBitmap32;
    FAlphaHit: Boolean;
    FCropped: Boolean;
    procedure BitmapChanged(Sender: TObject);
    procedure SetBitmap(Value: TBitmap32);
    procedure SetCropped(Value: Boolean);
  protected
    function DoHitTest(X, Y: Integer): Boolean; override;
    procedure Paint(Buffer: TBitmap32); override;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;
    property AlphaHit: Boolean read FAlphaHit write FAlphaHit;
    property Bitmap: TBitmap32 read FBitmap write SetBitmap;
    property Cropped: Boolean read FCropped write SetCropped;
  end;

  TDragState = (dsNone, dsMove, dsSizeL, dsSizeT, dsSizeR, dsSizeB,
    dsSizeTL, dsSizeTR, dsSizeBL, dsSizeBR);
  TRBHandles = set of (rhCenter, rhSides, rhCorners, rhFrame,
    rhNotLeftSide, rhNotRightSide, rhNotTopSide, rhNotBottomSide,
    rhNotTLCorner, rhNotTRCorner, rhNotBLCorner, rhNotBRCorner);
  TRBResizingEvent = procedure(
    Sender: TObject;
    const OldLocation: TFloatRect;
    var NewLocation: TFloatRect;
    DragState: TDragState;
    Shift: TShiftState) of object;

  TRubberbandLayer = class(TPositionedLayer)
  private
    FChildLayer: TPositionedLayer;
    FFrameStipplePattern: TArrayOfColor32;
    FFrameStippleStep: Single;
    FHandleFrame: TColor32;
    FHandleFill: TColor32;
    FHandles: TRBHandles;
    FHandleSize: Integer;
    FMinWidth: Single;
    FMaxHeight: Single;
    FMinHeight: Single;
    FMaxWidth: Single;
    FOnUserChange: TNotifyEvent;
    FOnResizing: TRBResizingEvent;
    procedure SetFrameStippleStep(const Value: Single);
    procedure SetChildLayer(Value: TPositionedLayer);
    procedure SetHandleFill(Value: TColor32);
    procedure SetHandleFrame(Value: TColor32);
    procedure SetHandles(Value: TRBHandles);
    procedure SetHandleSize(Value: Integer);
  protected
    IsDragging: Boolean;
    DragState: TDragState;
    OldLocation: TFloatRect;
    MouseShift: TFloatPoint;
    function  DoHitTest(X, Y: Integer): Boolean; override;
    procedure DoResizing(var OldLocation, NewLocation: TFloatRect; DragState: TDragState; Shift: TShiftState); virtual;
    procedure DoSetLocation(const NewLocation: TFloatRect); override;
    function  GetDragState(X, Y: Integer): TDragState; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(ALayer: TCustomLayer); override;
    procedure Paint(Buffer: TBitmap32); override;
    procedure UpdateChildLayer;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    procedure SetFrameStipple(const Value: Array of TColor32);
    property ChildLayer: TPositionedLayer read FChildLayer write SetChildLayer;
    property Handles: TRBHandles read FHandles write SetHandles;
    property HandleSize: Integer read FHandleSize write SetHandleSize;
    property HandleFill: TColor32 read FHandleFill write SetHandleFill;
    property HandleFrame: TColor32 read FHandleFrame write SetHandleFrame;
    property FrameStippleStep: Single read FFrameStippleStep write SetFrameStippleStep;
    property MaxHeight: Single read FMaxHeight write FMaxHeight;
    property MaxWidth: Single read FMaxWidth write FMaxWidth;
    property MinHeight: Single read FMinHeight write FMinHeight;
    property MinWidth: Single read FMinWidth write FMinWidth;
    property OnUserChange: TNotifyEvent read FOnUserChange write FOnUserChange;
    property OnResizing: TRBResizingEvent read FOnResizing write FOnResizing;
  end;

implementation

uses TypInfo, GR32_Image, GR32_LowLevel, GR32_Transforms;

{ mouse state mapping }
const
  CStateMap: array [TMouseButton] of TLayerState =
    (lsMouseLeft, lsMouseRight, lsMouseMiddle);

type
  TImage32Access = class(TCustomImage32);

{ TLayerCollection }

function TLayerCollection.Add(ItemClass: TLayerClass): TCustomLayer;
begin
  Result := ItemClass.Create(Self);
end;

procedure TLayerCollection.Assign(Source: TPersistent);
var
  I: Integer;
  Item: TCustomLayer;
begin
  if Source is TLayerCollection then
  begin
    BeginUpdate;
    try
      while FItems.Count > 0 do TCustomLayer(FItems.Last).Free;
      for I := 0 to TLayerCollection(Source).Count - 1 do
      begin
        Item := TLayerCollection(Source).Items[I];
        Add(TLayerClass(Item.ClassType)).Assign(Item);
      end;
    finally
      EndUpdate;
    end;
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TLayerCollection.BeginUpdate;
begin
  if FUpdateCount = 0 then Changing;
  Inc(FUpdateCount);
end;

procedure TLayerCollection.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TLayerCollection.Changing;
begin
  if Assigned(FOnChanging) then FOnChanging(Self);
end;

procedure TLayerCollection.Clear;
begin
  BeginUpdate;
  try
    while FItems.Count > 0 do TCustomLayer(FItems.Last).Free;
  finally
    EndUpdate;
  end;
end;

constructor TLayerCollection.Create(AOwner: TComponent);
begin
  FOwner := AOwner;
  FItems := TList.Create;
  FMouseEvents := True;
end;

procedure TLayerCollection.Delete(Index: Integer);
begin
  TCollectionItem(FItems[Index]).Free;
end;

destructor TLayerCollection.Destroy;
begin
  FUpdateCount := 1; // disable update notification
  if FItems <> nil then Clear;
  FItems.Free;
  inherited;
end;

procedure TLayerCollection.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then Changed;
  Assert(FUpdateCount >= 0, 'Unpaired EndUpdate');
end;

function TLayerCollection.FindLayerAtPos(X, Y: Integer; OptionsMask: Cardinal): TCustomLayer;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
  begin
    Result := Items[I];
    if (Result.LayerOptions and OptionsMask) = 0 then Continue; // skip to the next one
    if Result.HitTest(X, Y) then Exit;
  end;
  Result := nil;
end;

procedure TLayerCollection.GDIUpdate;
begin
  if (FUpdateCount = 0) and Assigned(FOnGDIUpdate) then FOnGDIUpdate(Self);
end;

function TLayerCollection.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TLayerCollection.GetItem(Index: Integer): TCustomLayer;
begin
  Result := FItems[Index];
end;

function TLayerCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TLayerCollection.Insert(Index: Integer; ItemClass: TLayerClass): TCustomLayer;
begin
  BeginUpdate;
  try
    Result := Add(ItemClass);
    Result.Index := Index;
  finally
    EndUpdate;
  end;
end;

procedure TLayerCollection.InsertItem(Item: TCustomLayer);
begin
  BeginUpdate;
  try
    FItems.Add(Item);
    Item.FLayerCollection := Self;
  finally
    EndUpdate;
  end;
end;

function TLayerCollection.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): TCustomLayer;
begin
  if MouseListener <> nil then Result := MouseListener
  else Result := FindLayerAtPos(X, Y, LOB_MOUSE_EVENTS);

  if (Button = mbLeft) and (Result <> MouseListener) and
    ((Result = nil) or ((Result.FLayerOptions and LOB_NO_CAPTURE) = 0)) then
    MouseListener := Result; // capture the mouse

  if Assigned(MouseListener) then
  begin
    Include(MouseListener.FLayerStates, CStateMap[Button]);
    MouseListener.MouseDown(Button, Shift, X, Y);
  end;
end;

function TLayerCollection.MouseMove(Shift: TShiftState; X, Y: Integer): TCustomLayer;
begin
  Result := MouseListener;
  if Result = nil then Result := FindLayerAtPos(X, Y, LOB_MOUSE_EVENTS);
  if Assigned(Result) then Result.MouseMove(Shift, X, Y)
  else if FOwner is TControl then Screen.Cursor := TControl(FOwner).Cursor;
end;

function TLayerCollection.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): TCustomLayer;
begin
  Result := MouseListener;
  if Result = nil then Result := FindLayerAtPos(X, Y, LOB_MOUSE_EVENTS);

  if Assigned(Result) then
  begin
    Exclude(Result.FLayerStates, CStateMap[Button]);
    Result.MouseUp(Button, Shift, X, Y);
  end;

  if Assigned(MouseListener) and
    (MouseListener.FLayerStates *
      [lsMouseLeft, lsMouseRight, lsMouseMiddle] = []) then
    MouseListener := nil; // reset mouse capture
end;

procedure TLayerCollection.RemoveItem(Item: TCustomLayer);
begin
  BeginUpdate;
  try
    FItems.Remove(Item);
    Item.FLayerCollection := nil;
  finally
    EndUpdate;
  end;
end;

procedure TLayerCollection.SetItem(Index: Integer; Value: TCustomLayer);
begin
  TCollectionItem(FItems[Index]).Assign(Value);
end;

procedure TLayerCollection.SetMouseEvents(Value: Boolean);
begin
  FMouseEvents := Value;
  MouseListener := nil;
end;

procedure TLayerCollection.SetMouseListener(Value: TCustomLayer);
begin
  if Value <> FMouseListener then
  begin
    if FMouseListener <> nil then
      FMouseListener.FLayerStates := FMouseListener.FLayerStates -
        [lsMouseLeft, lsMouseRight, lsMouseMiddle];
    FMouseListener := Value;
  end;
end;

{ TCustomLayer }

procedure TCustomLayer.AddNotification(ALayer: TCustomLayer);
begin
  if not Assigned(FFreeNotifies) then FFreeNotifies := TList.Create;
  if FFreeNotifies.IndexOf(ALayer) < 0 then FFreeNotifies.Add(ALayer);
end;

procedure TCustomLayer.BeforeDestruction;
begin
  if Assigned(FOnDestroy) then FOnDestroy(Self);
  inherited;
end;

procedure TCustomLayer.BringToFront;
begin
  Index := LayerCollection.Count;
end;

procedure TCustomLayer.Changed;
begin
  if (FLayerCollection <> nil) and ((FLayerOptions and LOB_NO_UPDATE) = 0) then
  begin
    if Visible then FLayerCollection.Changed
    else if (FLayerOptions and LOB_GDI_OVERLAY) <> 0 then
      FLayerCollection.GDIUpdate;
  end;
end;

procedure TCustomLayer.Changing;
begin
  if Visible and (FLayerCollection <> nil) and
    ((FLayerOptions and LOB_NO_UPDATE) = 0) then
    FLayerCollection.Changing;
end;

constructor TCustomLayer.Create(ALayerCollection: TLayerCollection);
begin
  LayerCollection := ALayerCollection;
  FLayerOptions := LOB_VISIBLE;
end;

destructor TCustomLayer.Destroy;
var
  I: Integer;
begin
  if FFreeNotifies <> nil then
  begin
    for I := FFreeNotifies.Count - 1 downto 0 do
    begin
      TCustomLayer(FFreeNotifies[I]).Notification(Self);
      if FFreeNotifies = nil then Break;
    end;
    FFreeNotifies.Free;
    FFreeNotifies := nil;
  end;
  SetLayerCollection(nil);
  inherited;
end;

function TCustomLayer.DoHitTest(X, Y: Integer): Boolean;
begin
  Result := True;
end;

procedure TCustomLayer.DoPaint(Buffer: TBitmap32);
begin
  Paint(Buffer);
  if Assigned(FOnPaint) then FOnPaint(Self, Buffer);
end;

function TCustomLayer.GetIndex: Integer;
begin
  if FLayerCollection <> nil then Result := FLayerCollection.FItems.IndexOf(Self)
  else Result := -1;
end;

function TCustomLayer.GetMouseEvents: Boolean;
begin
  Result := FLayerOptions and LOB_MOUSE_EVENTS <> 0;
end;

function TCustomLayer.GetOwner: TPersistent;
begin
  Result := FLayerCollection;
end;

function TCustomLayer.GetVisible: Boolean;
begin
  Result := FLayerOptions and LOB_VISIBLE <> 0;
end;

function TCustomLayer.HitTest(X, Y: Integer): Boolean;
begin
  Result := DoHitTest(X, Y);
  if Assigned(FOnHitTest) then FOnHitTest(Self, X, Y, Result);
end;

procedure TCustomLayer.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseDown) then FOnMouseDown(Self, Button, Shift, X, Y);
end;

procedure TCustomLayer.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  Screen.Cursor := Cursor;
  if Assigned(FOnMouseMove) then FOnMouseMove(Self, Shift, X, Y);
end;

procedure TCustomLayer.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Screen.Cursor := crDefault;
  if Assigned(FOnMouseUp) then FOnMouseUp(Self, Button, Shift, X, Y);
end;

procedure TCustomLayer.Notification(ALayer: TCustomLayer);
begin
  // do nothing by default
end;

procedure TCustomLayer.Paint(Buffer: TBitmap32);
begin
  // descendants override this method
end;

procedure TCustomLayer.PaintGDI(Canvas: TCanvas);
begin
  // descendants override this method
end;

procedure TCustomLayer.RemoveNotification(ALayer: TCustomLayer);
begin
  if FFreeNotifies <> nil then
  begin
    FFreeNotifies.Remove(ALayer);
    if FFreeNotifies.Count = 0 then
    begin
      FFreeNotifies.Free;
      FFreeNotifies := nil;
    end;
  end;
end;

procedure TCustomLayer.SendToBack;
begin
  Index := 0;
end;

procedure TCustomLayer.SetAsMouseListener;
begin
  FLayerCollection.MouseListener := Self;
  Screen.Cursor := Cursor;
end;

procedure TCustomLayer.SetCursor(Value: TCursor);
begin
  if Value <> FCursor then
  begin
    FCursor := Value;
    if FLayerCollection.MouseListener = Self then Screen.Cursor := Value;
  end;
end;

procedure TCustomLayer.SetIndex(Value: Integer);
var
  CurIndex: Integer;
begin
  CurIndex := GetIndex;
  if (CurIndex >= 0) and (CurIndex <> Value) then
    with FLayerCollection do
    begin
      if Value < 0 then Value := 0;
      if Value >= Count then Value := Count - 1;
      if Value <> CurIndex then
      begin
        if Visible then BeginUpdate;
        try
          FLayerCollection.FItems.Move(CurIndex, Value);
        finally
          if Visible then EndUpdate;
        end;
      end;
    end;
end;

procedure TCustomLayer.SetLayerCollection(Value: TLayerCollection);
begin
  if FLayerCollection <> Value then
  begin
    if FLayerCollection <> nil then
    begin
      if FLayerCollection.MouseListener = Self then
        FLayerCollection.MouseListener := nil;
      FLayerCollection.RemoveItem(Self);
    end;
    if Value <> nil then Value.InsertItem(Self);
  end;
end;

procedure TCustomLayer.SetLayerOptions(Value: Cardinal);
begin
  Changing;
  FLayerOptions := Value;
  Changed;
end;

procedure TCustomLayer.SetMouseEvents(Value: Boolean);
begin
  if Value then LayerOptions := LayerOptions or LOB_MOUSE_EVENTS
  else LayerOptions := LayerOptions and not LOB_MOUSE_EVENTS;
end;

procedure TCustomLayer.SetVisible(Value: Boolean);
begin
  if Value then LayerOptions := LayerOptions or LOB_VISIBLE
  else LayerOptions := LayerOptions and not LOB_VISIBLE;
end;

{ TPositionedLayer }

constructor TPositionedLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;
  with FLocation do
  begin
    Left := 0;
    Top := 0;
    Right := 64;
    Bottom := 64;
  end;
  FLayerOptions := LOB_VISIBLE or LOB_MOUSE_EVENTS;
end;

function TPositionedLayer.DoHitTest(X, Y: Integer): Boolean;
begin
  with GetAdjustedLocation do
    Result := (X >= Left) and (X < Right) and (Y >= Top) and (Y < Bottom);
end;

procedure TPositionedLayer.DoSetLocation(const NewLocation: TFloatRect);
begin
  FLocation := NewLocation;
end;

function TPositionedLayer.GetAdjustedLocation: TFloatRect;

  procedure AdjustPoint(var APoint: TFloatPoint);
  begin
    with APoint, FLayerCollection.CoordXForm^ do
    begin
      X := X * ScaleX / 65536 + ShiftX;
      Y := Y * ScaleY / 65536 + ShiftY;
    end;
  end;

begin
  Result := FLocation;
  if Scaled and Assigned(FLayerCollection) and Assigned(FLayerCollection.CoordXForm) then
    with Result do
    begin
      AdjustPoint(TopLeft);
      AdjustPoint(BottomRight);
    end;
end;

procedure TPositionedLayer.SetLocation(const Value: TFloatRect);
begin
  Changing;
  DoSetLocation(Value);
  Changed;
end;

procedure TPositionedLayer.SetScaled(Value: Boolean);
begin
  if Value <> FScaled then
  begin
    Changing;
    FScaled := Value;
    Changed;
  end;
end;

{ TBitmapLayer }

procedure TBitmapLayer.BitmapChanged(Sender: TObject);
begin
  Changed;
end;

constructor TBitmapLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;
  FBitmap := TBitmap32.Create;
  FBitmap.OnChange := BitmapChanged;
end;

function TBitmapLayer.DoHitTest(X, Y: Integer): Boolean;
var
  BitmapX, BitmapY: Integer;
  LayerWidth, LayerHeight: Integer;
begin
  Result := inherited DoHitTest(X, Y);
  if Result and AlphaHit then
  begin
    with GetAdjustedLocation do
    begin
      LayerWidth := Round(Right - Left);
      LayerHeight := Round(Bottom - Top);
      if (LayerWidth < 0.5) or (LayerHeight < 0.5) then Result := False
      else
      begin
        // check the pixel alpha at (X, Y) position
        BitmapX := Round((X - Left) * Bitmap.Width / LayerWidth);
        BitmapY := Round((Y - Top) * Bitmap.Height / LayerHeight);
        if Bitmap.PixelS[BitmapX, BitmapY] and $FF000000 = 0 then Result := False;
      end;
    end;
  end;
end;

destructor TBitmapLayer.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

procedure TBitmapLayer.Paint(Buffer: TBitmap32);
var
  SrcRect, DstRect, ClipRect: TRect;
  ImageRect: TRect;
  LayerWidth, LayerHeight: Single;
begin
  if Bitmap.Empty then Exit;
  DstRect := MakeRect(GetAdjustedLocation);
  SrcRect := MakeRect(0, 0, Bitmap.Width, Bitmap.Height);
  ClipRect := Buffer.ClipRect;
  if Cropped and (LayerCollection.FOwner is TCustomImage32) and
    not (TImage32Access(LayerCollection.FOwner).PaintToMode) then
  begin
    with DstRect do
    begin
      LayerWidth := Right - Left;
      LayerHeight := Bottom - Top;
    end;
    if (LayerWidth < 0.5) or (LayerHeight < 0.5) then Exit;
    ImageRect := TCustomImage32(LayerCollection.FOwner).GetBitmapRect;
    IntersectRect(ClipRect, ClipRect, ImageRect);
  end;
  StretchTransfer(Buffer, DstRect, ClipRect, FBitmap, SrcRect,
    FBitmap.StretchFilter, FBitmap.DrawMode, FBitmap.OnPixelCombine);
end;

procedure TBitmapLayer.SetBitmap(Value: TBitmap32);
begin
  FBitmap.Assign(Value);
end;

procedure TBitmapLayer.SetCropped(Value: Boolean);
begin
  if Value <> FCropped then
  begin
    FCropped := Value;
    Changed;
  end;
end;

{ TRubberbandLayer }

constructor TRubberbandLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;
  FHandleFrame := clBlack32;
  FHandleFill := clWhite32;
  FHandles := [rhCenter, rhSides, rhCorners, rhFrame];
  FHandleSize := 3;
  FMinWidth := 10;
  FMinHeight := 10;
  FLayerOptions := LOB_VISIBLE or LOB_MOUSE_EVENTS;
  SetFrameStipple([clWhite32, clWhite32, clBlack32, clBlack32]);
  FFrameStippleStep := 1;
end;

function TRubberbandLayer.DoHitTest(X, Y: Integer): Boolean;
begin
  Result := GetDragState(X, Y) <> dsNone;
end;

procedure TRubberbandLayer.DoResizing(var OldLocation,
  NewLocation: TFloatRect; DragState: TDragState; Shift: TShiftState);
begin
  if Assigned(FOnResizing) then
    FOnResizing(Self, OldLocation, NewLocation, DragState, Shift);
end;

procedure TRubberbandLayer.DoSetLocation(const NewLocation: TFloatRect);
begin
  inherited;
  UpdateChildLayer;
end;

function TRubberbandLayer.GetDragState(X, Y: Integer): TDragState;
var
  R: TRect;
  dh_center, dh_sides, dh_corners: Boolean;
  dl, dt, dr, db, dx, dy: Boolean;
  Sz: Integer;
begin
  Result := dsNone;
  Sz := FHandleSize + 1;
  dh_center := rhCenter in FHandles;
  dh_sides := rhSides in FHandles;
  dh_corners := rhCorners in FHandles;
  R := MakeRect(GetAdjustedLocation);
  with R do
  begin
    Dec(Right);
    Dec(Bottom);
    dl := Abs(Left - X) <= Sz;
    dr := Abs(Right - X) <= Sz;
    dx := Abs((Left + Right) div 2 - X) <= Sz;
    dt := Abs(Top - Y) <= Sz;
    db := Abs(Bottom - Y) <= Sz;
    dy := Abs((Top + Bottom) div 2 - Y) <= Sz;
  end;

  if dr and db and dh_corners and not(rhNotBRCorner in FHandles) then Result := dsSizeBR
  else if dl and db and dh_corners and not(rhNotBLCorner in FHandles) then Result := dsSizeBL
  else if dr and dt and dh_corners and not(rhNotTRCorner in FHandles) then Result := dsSizeTR
  else if dl and dt and dh_corners and not(rhNotTLCorner in FHandles) then Result := dsSizeTL
  else if dr and dy and dh_sides and not(rhNotRightSide in FHandles) then Result := dsSizeR
  else if db and dx and dh_sides and not(rhNotBottomSide in FHandles) then Result := dsSizeB
  else if dl and dy and dh_sides and not(rhNotLeftSide in FHandles) then Result := dsSizeL
  else if dt and dx and dh_sides and not(rhNotTopSide in FHandles) then Result := dsSizeT
  else if dh_center and PtInRect(R, Point(X, Y)) then Result := dsMove;
end;

procedure TRubberbandLayer.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ALoc: TFloatRect;
begin
  if IsDragging then Exit;
  DragState := GetDragState(X, Y);
  IsDragging := DragState <> dsNone;
  if IsDragging then
  begin
    OldLocation := Location;
    ALoc := GetAdjustedLocation;
    case DragState of
      dsMove: MouseShift := FloatPoint(X - ALoc.Left, Y - ALoc.Top);
    else
      MouseShift := FloatPoint(0, 0);
    end;
  end;
  inherited;
end;

procedure TRubberbandLayer.MouseMove(Shift: TShiftState; X, Y: Integer);
const
  CURSOR_ID: array [TDragState] of TCursor = (crDefault, crDefault, crSizeWE,
    crSizeNS, crSizeWE, crSizeNS, crSizeNWSE, crSizeNESW, crSizeNESW, crSizeNWSE);
var
  Mx, My: Single;
  L, T, R, B, W, H: Single;
  ALoc, NewLocation: TFloatRect;

  procedure IncLT(var LT, RB: Single; Delta, MinSize, MaxSize: Single);
  begin
    LT := LT + Delta;
    if RB - LT < MinSize then LT := RB - MinSize;
    if MaxSize >= MinSize then if RB - LT > MaxSize then LT := RB - MaxSize;
  end;

  procedure IncRB(var LT, RB: Single; Delta, MinSize, MaxSize: Single);
  begin
    RB := RB + Delta;
    if RB - LT < MinSize then RB := LT + MinSize;
    if MaxSize >= MinSize then if RB - LT > MaxSize then RB := LT + MaxSize;
  end;

begin
  if not IsDragging then
  begin
    DragState := GetDragState(X, Y);
    if DragState = dsMove then Screen.Cursor := Cursor
    else Screen.Cursor := CURSOR_ID[DragState];
  end
  else
  begin
    Mx := X - MouseShift.X;
    My := Y - MouseShift.Y;
    if Scaled then with Location do
    begin
      ALoc := GetAdjustedLocation;
      if IsRectEmptyF(ALoc) then Exit;
      Mx := (Mx - ALoc.Left) / (ALoc.Right - ALoc.Left) * (Right - Left) + Left;
      My := (My - ALoc.Top) / (ALoc.Bottom - ALoc.Top) * (Bottom - Top) + Top;
    end;

    with OldLocation do
    begin
      L := Left; T := Top; R := Right; B := Bottom; W := R - L; H := B - T;
    end;

    if DragState = dsMove then
    begin
      L := Mx; T := My; R := L + W; B := T + H;
    end
    else
    begin
      if DragState in [dsSizeL, dsSizeTL, dsSizeBL] then
        IncLT(L, R, Mx - L, MinWidth, MaxWidth);
      if DragState in [dsSizeR, dsSizeTR, dsSizeBR] then
        IncRB(L, R, Mx - R, MinWidth, MaxWidth);
      if DragState in [dsSizeT, dsSizeTL, dsSizeTR] then
        IncLT(T, B, My - T, MinHeight, MaxHeight);
      if DragState in [dsSizeB, dsSizeBL, dsSizeBR] then
        IncRB(T, B, My - B, MinHeight, MaxHeight);
    end;

    NewLocation := FloatRect(L, T, R, B);
    DoResizing(OldLocation, NewLocation, DragState, Shift);

    if (NewLocation.Left <> Location.Left) or
      (NewLocation.Right <> Location.Right) or
      (NewLocation.Top <> Location.Top) or
      (NewLocation.Bottom <> Location.Bottom) then
    begin
      Location := NewLocation;
      if Assigned(FOnUserChange) then FOnUserChange(Self);
    end;
  end;
end;

procedure TRubberbandLayer.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  IsDragging := False;
  inherited;
end;

procedure TRubberbandLayer.Notification(ALayer: TCustomLayer);
begin
  if ALayer = FChildLayer then FChildLayer := nil;
  if FChildLayer <> nil then FLayerOptions := FLayerOptions or LOB_NO_UPDATE
  else FLayerOptions := FLayerOptions and not LOB_NO_UPDATE;
end;

procedure TRubberbandLayer.Paint(Buffer: TBitmap32);
var
  Cx, Cy: Integer;
  R: TRect;

  procedure DrawHandle(X, Y: Integer);
  begin
    Buffer.FillRectTS(X - FHandleSize, Y - FHandleSize, X + FHandleSize, Y + FHandleSize, FHandleFill);
    Buffer.FrameRectTS(X - FHandleSize, Y - FHandleSize, X + FHandleSize, Y + FHandleSize, FHandleFrame);
  end;

begin
  R := MakeRect(GetAdjustedLocation);
  with R do
  begin
    if rhFrame in FHandles then
    begin
      Buffer.SetStipple(FFrameStipplePattern);
      Buffer.StippleCounter := 0;
      Buffer.StippleStep := FFrameStippleStep;
      Buffer.FrameRectTSP(Left, Top, Right, Bottom);
    end;
    if rhCorners in FHandles then
    begin
      If not(rhNotTLCorner in FHandles) then DrawHandle(Left, Top);
      If not(rhNotTRCorner in FHandles) then DrawHandle(Right, Top);
      If not(rhNotBLCorner in FHandles) then DrawHandle(Left, Bottom);
      If not(rhNotBRCorner in FHandles) then DrawHandle(Right, Bottom);
    end;
    if rhSides in FHandles then
    begin
      Cx := (Left + Right) div 2;
      Cy := (Top + Bottom) div 2;
      If not(rhNotTopSide in FHandles) then DrawHandle(Cx, Top);
      If not(rhNotLeftSide in FHandles) then DrawHandle(Left, Cy);
      If not(rhNotRightSide in FHandles) then DrawHandle(Right, Cy);
      If not(rhNotBottomSide in FHandles) then DrawHandle(Cx, Bottom);
    end;
  end;
end;

procedure TRubberbandLayer.SetChildLayer(Value: TPositionedLayer);
begin
  if FChildLayer <> nil then RemoveNotification(FChildLayer);
  FChildLayer := Value;
  if Value <> nil then
  begin
    Location := Value.Location;
    Scaled := Value.Scaled;
    AddNotification(FChildLayer);
  end;
  if FChildLayer <> nil then FLayerOptions := FLayerOptions or LOB_NO_UPDATE
  else FLayerOptions := FLayerOptions and not LOB_NO_UPDATE;
end;

procedure TRubberbandLayer.SetHandleFill(Value: TColor32);
begin
  if Value <> FHandleFill then
  begin
    FHandleFill := Value;
    FLayerCollection.GDIUpdate;
  end;
end;

procedure TRubberbandLayer.SetHandleFrame(Value: TColor32);
begin
  if Value <> FHandleFrame then
  begin
    FHandleFrame := Value;
    FLayerCollection.GDIUpdate;
  end;
end;

procedure TRubberbandLayer.SetHandles(Value: TRBHandles);
begin
  if Value <> FHandles then
  begin
    FHandles := Value;
    FLayerCollection.GDIUpdate;
  end;
end;

procedure TRubberbandLayer.SetHandleSize(Value: Integer);
begin
  if Value < 1 then Value := 1;
  if Value <> FHandleSize then
  begin
    FHandleSize := Value;
    FLayerCollection.GDIUpdate;
  end;
end;

procedure TRubberbandLayer.SetFrameStipple(const Value: Array of TColor32);
var
  L: Integer;
begin
  L := High(Value) + 1;
  SetLength(FFrameStipplePattern, L);
  Move(Value[0], FFrameStipplePattern[0], L shl 2);
end;

procedure TRubberbandLayer.SetFrameStippleStep(const Value: Single);
begin
  if Value <> FFrameStippleStep then
  begin
    FFrameStippleStep := Value;
    FLayerCollection.GDIUpdate;
  end;
end;

procedure TRubberbandLayer.UpdateChildLayer;
begin
  if Assigned(FChildLayer) then FChildLayer.Location := Location;
end;

end.
