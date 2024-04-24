unit GR32_Layers;

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
 * ***** END LICENSE BLOCK ***** *)

interface

{$INCLUDE GR32.inc}

uses
{$if defined(FRAMEWORK_VCL)}
  System.UITypes,
  WinApi.Windows,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.Forms,
{$elseif defined(FRAMEWORK_FMX)}
  System.UITypes,
  WinApi.Windows,
  FMX.Types,
  FMX.Controls,
  FMX.Graphics,
  FMX.Forms,
{$elseif defined(FRAMEWORK_LCL)}
  Controls,
  Graphics,
  Forms,
{$ifend}
  Generics.Collections,
  Classes,
  SysUtils,
  Math,
  GR32;

const
  { Layer Options Bits }
  LOB_VISIBLE           = $80000000; // 31-st bit: Controls the layer visibility
  LOB_GDI_OVERLAY       = $40000000; // 30-th bit: Indicates that the layer performs drawing when its owner draws its GDI Overlays.
  LOB_MOUSE_EVENTS      = $20000000; // 29-th bit: Specifies whether the layer responds to mouse messages.
  LOB_NO_UPDATE         = $10000000; // 28-th bit: Disables automatic repainting when the layer changes its location or other properties.
  LOB_NO_CAPTURE        = $08000000; // 27-th bit: Allows to override automatic capturing of mouse messages when the left mouse is pressed on top of the layer. This bit has no effect if LOB_MOUSE_EVENTS is not set.
  LOB_INVALID           = $04000000; // 26-th bit: Used internall by repaint optimizer.
  LOB_FORCE_UPDATE      = $02000000; // 25-th bit: Used internally to force a layer to update when it is being hidden.
  LOB_RESERVED_24       = $01000000; // 24-th bit
  LOB_RESERVED_MASK     = $FF000000;

type
  TCustomLayer = class;
  TPositionedLayer = class;
  TRubberbandLayer = class;
  TLayerClass = class of TCustomLayer;

  TLayerCollection = class;

  TLayerUpdateEvent = procedure(Sender: TObject; Layer: TCustomLayer) of object;
  TAreaUpdateEvent = TAreaChangedEvent;
  TLayerListNotification = (lnLayerAdded, lnLayerInserted, lnLayerDeleted, lnCleared);
  TLayerListNotifyEvent = procedure(Sender: TLayerCollection; Action: TLayerListNotification;
    Layer: TCustomLayer; Index: Integer) of object;
  TGetScaleEvent = procedure(Sender: TObject; out ScaleX, ScaleY: TFloat) of object;
  TGetShiftEvent = procedure(Sender: TObject; out ShiftX, ShiftY: TFloat) of object;

  ILayerNotification = interface
    ['{5549DE7E-778E-4500-9F20-6455EC3BC574}']
    procedure LayerUpdated(ALayer: TCustomLayer);
    procedure LayerAreaUpdated(ALayer: TCustomLayer; const AArea: TRect; const AInfo: Cardinal);
    procedure LayerListNotify(ALayer: TCustomLayer; AAction: TLayerListNotification; AIndex: Integer);
  end;

  IUpdateRectNotification = interface
    ['{457C0840-F4C3-48CE-8440-C790CC2CA103}']
    procedure AreaUpdated(const AArea: TRect; const AInfo: Cardinal);
  end;

  ILayerUpdateNotification = interface
    ['{FE142F0F-D009-4B6A-8874-6F7BF2208E84}']
    procedure LayerUpdated(ALayer: TCustomLayer);
  end;

  ILayerListNotification = interface
    ['{7E8F0FC3-F9B7-4E38-9CF4-5B1A38901849}']
    procedure LayerListNotify(ALayer: TCustomLayer; AAction: TLayerListNotification; AIndex: Integer);
  end;

  TLayerCollection = class(TPersistent)
  private
    FItems: TList;
    FMouseEvents: Boolean;
    FMouseListener: TCustomLayer;
    FUpdateCount: Integer;
    FLockUpdateCount: Integer;
    FModified: boolean;
    FOwner: TPersistent;
    FSubscribers: TList<IInterface>;
    FOnChanging: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FOnGDIUpdate: TNotifyEvent;
    FOnListNotify: TLayerListNotifyEvent;
    FOnLayerUpdated: TLayerUpdateEvent;
    FOnAreaUpdated: TAreaUpdateEvent;
    FOnGetViewportScale: TGetScaleEvent;
    FOnGetViewportShift: TGetShiftEvent;
    function GetCount: Integer;
    procedure InsertItem(Item: TCustomLayer);
    procedure RemoveItem(Item: TCustomLayer);
    procedure SetMouseEvents(Value: Boolean);
    procedure SetMouseListener(Value: TCustomLayer);
  protected
    procedure BeginUpdate; {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure EndUpdate; {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure BeginLockUpdate;
    procedure EndLockUpdate;
    procedure Changed; {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure Changing; {$IFDEF USEINLINING} inline; {$ENDIF}
    function  FindLayerAtPos(X, Y: Integer; OptionsMask: Cardinal): TCustomLayer;
    function  GetItem(Index: Integer): TCustomLayer;
    function  GetOwner: TPersistent; override;
    procedure GDIUpdate;
    procedure DoUpdateLayer(Layer: TCustomLayer);
    procedure DoUpdateArea(const Rect: TRect; const Info: Cardinal);
    procedure Notify(Action: TLayerListNotification; Layer: TCustomLayer; Index: Integer);
    procedure SetItem(Index: Integer; Value: TCustomLayer);
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): TCustomLayer;
    function MouseMove(Shift: TShiftState; X, Y: Integer): TCustomLayer;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): TCustomLayer;

    property UpdateCount: Integer read FUpdateCount;
    property LockUpdateCount: Integer read FLockUpdateCount;
    property Modified: boolean read FModified;

    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnListNotify: TLayerListNotifyEvent read FOnListNotify write FOnListNotify;
    property OnGDIUpdate: TNotifyEvent read FOnGDIUpdate write FOnGDIUpdate;
    property OnLayerUpdated: TLayerUpdateEvent read FOnLayerUpdated write FOnLayerUpdated;
    property OnAreaUpdated: TAreaUpdateEvent read FOnAreaUpdated write FOnAreaUpdated;
    property OnGetViewportScale: TGetScaleEvent read FOnGetViewportScale write FOnGetViewportScale;
    property OnGetViewportShift: TGetShiftEvent read FOnGetViewportShift write FOnGetViewportShift;
  public
    constructor Create(AOwner: TPersistent); virtual;
    destructor Destroy; override;

    procedure Subscribe(const ASubscriber: IInterface);
    procedure Unsubscribe(const ASubscriber: IInterface);

    function  Add(ItemClass: TLayerClass): TCustomLayer;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    procedure Delete(Index: Integer);
    function  Insert(Index: Integer; ItemClass: TLayerClass): TCustomLayer;
    function  LocalToViewport(const APoint: TFloatPoint; AScaled: Boolean): TFloatPoint;
    function  ViewportToLocal(const APoint: TFloatPoint; AScaled: Boolean): TFloatPoint;
    procedure GetViewportScale(out ScaleX, ScaleY: TFloat); virtual;
    procedure GetViewportShift(out ShiftX, ShiftY: TFloat); virtual;

    property Count: Integer read GetCount;
    property Owner: TPersistent read FOwner;
    property Items[Index: Integer]: TCustomLayer read GetItem write SetItem; default;
    property MouseListener: TCustomLayer read FMouseListener write SetMouseListener;
    property MouseEvents: Boolean read FMouseEvents write SetMouseEvents;
  end;

  TLayerCollectionClass = class of TLayerCollection;

  TLayerEnum = class
   private
     FIndex: Integer;
     FLayerCollection: TLayerCollection;
   public
     constructor Create(ALayerCollection: TLayerCollection);

     function GetCurrent: TCustomLayer;
     function MoveNext: Boolean;

     property Current: TCustomLayer read GetCurrent;
   end;

   TLayerCollectionHelper = class Helper for TLayerCollection
   public
     function GetEnumerator: TLayerEnum;
   end;

  TLayerState = (lsMouseLeft, lsMouseRight, lsMouseMiddle);
  TLayerStates = set of TLayerState;

  TPaintLayerEvent = procedure(Sender: TObject; Buffer: TBitmap32) of object;
  THitTestEvent = procedure(Sender: TObject; X, Y: Integer; var Passed: Boolean) of object;

  TCustomLayer = class(TNotifiablePersistent)
  private
    FCursor: TCursor;
    FFreeNotifies: TList;
    FLayerCollection: TLayerCollection;
    FLayerStates: TLayerStates;
    FLayerOptions: Cardinal;
    FTag: NativeInt;
    FClicked: Boolean;
    FOnHitTest: THitTestEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseUp: TMouseEvent;
    FOnPaint: TPaintLayerEvent;
    FOnDestroy: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FOnClick: TNotifyEvent;
    function  GetIndex: Integer;
    function  GetMouseEvents: Boolean;
    function  GetVisible: Boolean;
    procedure SetMouseEvents(Value: Boolean);
    procedure SetVisible(Value: Boolean);
    function GetInvalid: Boolean;
    procedure SetInvalid(Value: Boolean);
    function GetForceUpdate: Boolean;
    procedure SetForceUpdate(Value: Boolean);
  protected
    procedure AddNotification(ALayer: TCustomLayer);
    procedure Changing;
    procedure Click; virtual;
    procedure DblClick; virtual;
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
    procedure SetCursor(Value: TCursor); virtual;
    procedure SetLayerCollection(Value: TLayerCollection); virtual;
    procedure SetLayerOptions(Value: Cardinal); virtual;
    procedure DoChanged; overload; override;
    procedure AreaUpdated(const AArea: TRect; const AInfo: Cardinal);
    procedure UpdateRect(const ARect: TRect);
    procedure Update(const ARect: TRect); overload; deprecated 'Use UpdateRect';
    procedure Changed(const Rect: TRect; const Info: Cardinal = 0); reintroduce; overload;
    property Invalid: Boolean read GetInvalid write SetInvalid;
    property ForceUpdate: Boolean read GetForceUpdate write SetForceUpdate;
  public
    constructor Create(ALayerCollection: TLayerCollection); virtual;
    destructor Destroy; override;

    procedure BeforeDestruction; override;
    procedure BringToFront;
    procedure Update; overload; virtual;
    function  HitTest(X, Y: Integer): Boolean;
    procedure SendToBack;
    procedure SetAsMouseListener;

    function LayerToControl(const p: TPoint; AScaleFromContent: boolean): TPoint; overload; virtual;
    function LayerToControl(const r: TRect; AScaleFromContent: boolean): TRect; overload; virtual;
    function LayerToControl(const p: TFloatPoint; AScaleFromContent: boolean): TFloatPoint; overload; virtual;
    function LayerToControl(const r: TFloatRect; AScaleFromContent: boolean): TFloatRect; overload; virtual;
    function ControlToLayer(const p: TPoint; AScaleToContent: boolean): TPoint; overload; virtual;
    function ControlToLayer(const r: TRect; AScaleToContent: boolean): TRect; overload; virtual;
    function ControlToLayer(const p: TFloatPoint; AScaleToContent: boolean): TFloatPoint; overload; virtual;
    function ControlToLayer(const r: TFloatRect; AScaleToContent: boolean): TFloatRect; overload; virtual;

    property Cursor: TCursor read FCursor write SetCursor;
    property Index: Integer read GetIndex write SetIndex;
    property LayerCollection: TLayerCollection read FLayerCollection write SetLayerCollection;
    property LayerOptions: Cardinal read FLayerOptions write SetLayerOptions;
    property LayerStates: TLayerStates read FLayerStates;
    property MouseEvents: Boolean read GetMouseEvents write SetMouseEvents;
    property Tag: NativeInt read FTag write FTag;
    property Visible: Boolean read GetVisible write SetVisible;

    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnHitTest: THitTestEvent read FOnHitTest write FOnHitTest;
    property OnPaint: TPaintLayerEvent read FOnPaint write FOnPaint;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
  end;

  TLayerGetUpdateRectEvent = procedure(Sender: TObject; var UpdateRect: TRect) of object;

  TPositionedLayer = class(TCustomLayer)
  private
    FLocation: TFloatRect;
    FScaled: Boolean;
    FOnGetUpdateRect: TLayerGetUpdateRectEvent;
    procedure SetLocation(const Value: TFloatRect);
    procedure SetScaled(Value: Boolean);
  protected
    function DoHitTest(X, Y: Integer): Boolean; override;
    procedure DoSetLocation(const NewLocation: TFloatRect); virtual;
    function DoGetUpdateRect: TRect; virtual;
    function GetUpdateRect: TRect;
    // GetContentSize: Size of layer content (e.g. the bitmap if is has one).
    // Used to translate between viewport (layer) and content coordinates.
    function GetContentSize: TPoint; virtual;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;

    procedure Update; override;

    function LayerToControl(const APoint: TPoint; AScaleFromContent: boolean): TPoint; overload; override;
    function LayerToControl(const ARect: TRect; AScaleFromContent: boolean): TRect; overload; override;
    function LayerToControl(const APoint: TFloatPoint; AScaleFromContent: boolean): TFloatPoint; overload; override;
    function LayerToControl(const ARect: TFloatRect; AScaleFromContent: boolean): TFloatRect; overload; override;
    function ControlToLayer(const APoint: TPoint; AScaleToContent: boolean): TPoint; overload; override;
    function ControlToLayer(const ARect: TRect; AScaleToContent: boolean): TRect; overload; override;
    function ControlToLayer(const APoint: TFloatPoint; AScaleToContent: boolean): TFloatPoint; overload; override;
    function ControlToLayer(const ARect: TFloatRect; AScaleToContent: boolean): TFloatRect; overload; override;

    function GetAdjustedRect(const R: TFloatRect): TFloatRect; virtual;
    function GetAdjustedLocation: TFloatRect;

    property Location: TFloatRect read FLocation write SetLocation;
    property Scaled: Boolean read FScaled write SetScaled;

    property OnGetUpdateRect: TLayerGetUpdateRectEvent read FOnGetUpdateRect write FOnGetUpdateRect;
  end;

  TCustomIndirectBitmapLayer = class(TPositionedLayer)
  strict private
    FAlphaHit: Boolean;
    FCropped: Boolean;
  strict protected
    FBitmap: TCustomBitmap32;
    function OwnsBitmap: boolean; virtual;
  private
    procedure DoSetBitmap(Value: TCustomBitmap32);
  protected
    function DoHitTest(X, Y: Integer): Boolean; override;
    procedure Paint(Buffer: TBitmap32); override;
    function GetContentSize: TPoint; override;
  protected
    procedure BitmapAreaChanged(Sender: TObject; const Area: TRect; const Info: Cardinal);
    procedure SetBitmap(Value: TCustomBitmap32); virtual;
    procedure SetCropped(Value: Boolean);
    property Bitmap: TCustomBitmap32 read FBitmap write SetBitmap;
  public
    constructor Create(ALayerCollection: TLayerCollection); overload; override;
    constructor Create(ALayerCollection: TLayerCollection; ABitmap: TCustomBitmap32); reintroduce; overload;
    destructor Destroy; override;


    property AlphaHit: Boolean read FAlphaHit write FAlphaHit;
    property Cropped: Boolean read FCropped write SetCropped;
  end;

  TIndirectBitmapLayer = class(TCustomIndirectBitmapLayer)
  public
    property Bitmap;
  end;

  TCustomBitmapLayer = class abstract(TCustomIndirectBitmapLayer)
  strict protected
    function OwnsBitmap: boolean; override;
  protected
    procedure SetBitmap(Value: TCustomBitmap32); override;
    function GetBitmapClass: TCustomBitmap32Class; virtual; abstract;
    function CreateBitmap: TCustomBitmap32; virtual;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
  end;

  TBitmapLayer = class(TCustomBitmapLayer)
  private
  protected
    function GetBitmapClass: TCustomBitmap32Class; override;
    function GetBitmap: TBitmap32;
    procedure SetBitmap(Value: TBitmap32); reintroduce;
  public
    property Bitmap: TBitmap32 read GetBitmap write SetBitmap;
  end;

  TRBDragState = (dsNone, dsMove, dsSizeL, dsSizeT, dsSizeR, dsSizeB,
    dsSizeTL, dsSizeTR, dsSizeBL, dsSizeBR);

  TRBHandles = set of (rhCenter, rhSides, rhCorners, rhFrame,
    rhNotLeftSide, rhNotRightSide, rhNotTopSide, rhNotBottomSide,
    rhNotTLCorner, rhNotTRCorner, rhNotBLCorner, rhNotBRCorner);

  TRBOptions = set of (roProportional, roConstrained, roQuantized);

  TRBResizingEvent = procedure(
    Sender: TObject;
    const OldLocation: TFloatRect;
    var NewLocation: TFloatRect;
    DragState: TRBDragState;
    Shift: TShiftState) of object;

  TRBConstrainEvent = TRBResizingEvent;

  TRubberbandPassMouse = class(TPersistent)
  private
    FOwner: TRubberbandLayer;
    FEnabled: Boolean;
    FToChild: Boolean;
    FLayerUnderCursor: Boolean;
    FCancelIfPassed: Boolean;
  protected
    function GetChildUnderCursor(X, Y: Integer): TPositionedLayer;
  public
    constructor Create(AOwner: TRubberbandLayer);

    property Enabled: Boolean read FEnabled write FEnabled default False;
    property ToChild: Boolean read FToChild write FToChild default False;
    property ToLayerUnderCursor: Boolean read FLayerUnderCursor write FLayerUnderCursor default False;
    property CancelIfPassed: Boolean read FCancelIfPassed write FCancelIfPassed default False;
  end;

  // TODO : Replace these with anonymous methods once FPC catches up (expected for FPC 4)
  TRBPaintFrameHandler = procedure(Buffer: TBitmap32; const r: TRect) of object;
  TRBPaintHandleHandler = procedure(Buffer: TBitmap32; X, Y: TFloat) of object;

  TRubberbandLayer = class(TPositionedLayer)
  private
    FChildLayer: TPositionedLayer;
    FFrameStipplePattern: TArrayOfColor32;
    FFrameStippleStep: TFloat;
    FFrameStippleCounter: TFloat;
    FHandleFrame: TColor32;
    FHandleFill: TColor32;
    FHandles: TRBHandles;
    FHandleSize: TFloat;
    FMinWidth: TFloat;
    FMaxHeight: TFloat;
    FMinHeight: TFloat;
    FMaxWidth: TFloat;
    FOnUserChange: TNotifyEvent;
    FOnResizing: TRBResizingEvent;
    FOnConstrain: TRBConstrainEvent;
    FOptions: TRBOptions;
    FQuantized: Integer;
    FPassMouse: TRubberbandPassMouse;
    procedure SetFrameStippleStep(const Value: TFloat);
    procedure SetFrameStippleCounter(const Value: TFloat);
    procedure SetChildLayer(Value: TPositionedLayer);
    procedure SetHandleFill(Value: TColor32);
    procedure SetHandleFrame(Value: TColor32);
    procedure SetHandles(Value: TRBHandles);
    procedure SetHandleSize(Value: TFloat);
    procedure SetOptions(const Value: TRBOptions);
    procedure SetQuantized(const Value: Integer);
  protected
    FIsDragging: Boolean;
    FDragState: TRBDragState;
    FOldLocation: TFloatRect;
    FMouseShift: TFloatPoint;
    function  DoHitTest(X, Y: Integer): Boolean; override;
    procedure DoResizing(var OldLocation, NewLocation: TFloatRect; DragState: TRBDragState; Shift: TShiftState); virtual;
    procedure DoConstrain(var OldLocation, NewLocation: TFloatRect; DragState: TRBDragState; Shift: TShiftState); virtual;
    procedure DoSetLocation(const NewLocation: TFloatRect); override;
    function  GetDragState(X, Y: Integer): TRBDragState; virtual;
    function GetHandleCursor(DragState: TRBDragState; Angle: integer): TCursor; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(ALayer: TCustomLayer); override;
    procedure Paint(Buffer: TBitmap32); override;
    procedure SetLayerOptions(Value: Cardinal); override;
    procedure SetDragState(const Value: TRBDragState); overload;
    procedure SetDragState(const Value: TRBDragState; const X, Y: Integer); overload;
    procedure UpdateChildLayer; virtual;
    procedure DrawHandle(Buffer: TBitmap32; X, Y: TFloat); virtual;
    procedure DrawFrame(Buffer: TBitmap32; const R: TRect);
    procedure UpdateHandle(Buffer: TBitmap32; X, Y: TFloat);
    procedure UpdateFrame(Buffer: TBitmap32; const R: TRect);
    procedure DoHandles(Buffer: TBitmap32; const R: TRect; DoFrame: TRBPaintFrameHandler; DoHandle: TRBPaintHandleHandler);
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;

    procedure Update; override;

    procedure SetFrameStipple(const Value: Array of TColor32);
    procedure Quantize;

    property ChildLayer: TPositionedLayer read FChildLayer write SetChildLayer;
    property Options: TRBOptions read FOptions write SetOptions;
    property Handles: TRBHandles read FHandles write SetHandles;
    property HandleSize: TFloat read FHandleSize write SetHandleSize;
    property HandleFill: TColor32 read FHandleFill write SetHandleFill;
    property HandleFrame: TColor32 read FHandleFrame write SetHandleFrame;
    property FrameStippleStep: TFloat read FFrameStippleStep write SetFrameStippleStep;
    property FrameStippleCounter: TFloat read FFrameStippleCounter write SetFrameStippleCounter;
    property MaxHeight: TFloat read FMaxHeight write FMaxHeight;
    property MaxWidth: TFloat read FMaxWidth write FMaxWidth;
    property MinHeight: TFloat read FMinHeight write FMinHeight;
    property MinWidth: TFloat read FMinWidth write FMinWidth;
    property Quantized: Integer read FQuantized write SetQuantized default 8;
    property PassMouseToChild: TRubberbandPassMouse read FPassMouse;

    property OnUserChange: TNotifyEvent read FOnUserChange write FOnUserChange;
    property OnConstrain: TRBConstrainEvent read FOnConstrain write FOnConstrain;
    property OnResizing: TRBResizingEvent read FOnResizing write FOnResizing;
  end;

type
  // Compas directions, counter clockwise, from 0 degress to 360.
  // Each one direction covers 45 degrees.
  // Used inside TRubberbandLayer.GetCursor instead of the poorly ordered TRBDragState enum.
  TResizeDirection = (ResizeDirectionE, ResizeDirectionNE, ResizeDirectionN, ResizeDirectionNW,
    ResizeDirectionW, ResizeDirectionSW, ResizeDirectionS, ResizeDirectionSE);

var
  // The TRubberbandLayer resize handle cursors.
  // These are the values returned by TRubberbandLayer.GetCursor
  DirectionCursors: array[TResizeDirection] of TCursor = (crSizeWE, crSizeNESW, crSizeNS, crSizeNWSE, crSizeWE, crSizeNESW, crSizeNS, crSizeNWSE);

implementation

uses
  TypInfo, GR32_Image, GR32_LowLevel, GR32_Resamplers, GR32_RepaintOpt, Types;

{ mouse state mapping }
const
  CStateMap: array [TMouseButton] of TLayerState =
    (lsMouseLeft, lsMouseRight, lsMouseMiddle {$IFDEF FPC}, lsMouseMiddle,
     lsMouseMiddle{$ENDIF});

type
  TImage32Access = class(TCustomImage32);

{ TLayerCollection }

constructor TLayerCollection.Create(AOwner: TPersistent);
begin
  inherited Create;

  FOwner := AOwner;
  FItems := TList.Create;
  FMouseEvents := True;
end;

destructor TLayerCollection.Destroy;
begin
  FUpdateCount := 1; // disable update notification
  if (FItems <> nil) then
    Clear;
  FItems.Free;
  FSubscribers.Free;
  inherited;
end;

function TLayerCollection.Add(ItemClass: TLayerClass): TCustomLayer;
begin
  Result := ItemClass.Create(Self);
  Result.Index := FItems.Count - 1;
  Notify(lnLayerAdded, Result, Result.Index);
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
      Changed;
    finally
      EndUpdate;
    end;
  end else
    inherited Assign(Source);
end;

procedure TLayerCollection.BeginUpdate;
begin
  if FUpdateCount = 0 then
    Changing;
  Inc(FUpdateCount);
end;

procedure TLayerCollection.EndUpdate;
begin
  Assert(FUpdateCount > 0, 'Unpaired EndUpdate');
  if FUpdateCount = 1 then
  begin
    if (FModified) and (Assigned(FOnChange)) then
      FOnChange(Self);
    FModified := False;
  end;
  Dec(FUpdateCount);
end;

procedure TLayerCollection.BeginLockUpdate;
begin
  Inc(FLockUpdateCount);
end;

procedure TLayerCollection.EndLockUpdate;
begin
  Dec(FLockUpdateCount);
end;

procedure TLayerCollection.Changed;
begin
  if (FLockUpdateCount > 0) then
    exit;
  BeginUpdate;
  FModified := True;
  EndUpdate;
end;

procedure TLayerCollection.Changing;
begin
  if Assigned(FOnChanging) then
    FOnChanging(Self);
end;

procedure TLayerCollection.Clear;
begin
  BeginUpdate;
  try
    while FItems.Count > 0 do
    begin
      TCustomLayer(FItems.Last).Visible := False;
      TCustomLayer(FItems.Last).Free;
    end;
    Notify(lnCleared, nil, 0);
    Changed;
  finally
    EndUpdate;
  end;
end;

procedure TLayerCollection.Delete(Index: Integer);
begin
  // Hide layer so the area covered by it will be invalidated
  TCustomLayer(FItems[Index]).Visible := False;

  TCustomLayer(FItems[Index]).Free;
end;

function TLayerCollection.FindLayerAtPos(X, Y: Integer; OptionsMask: Cardinal): TCustomLayer;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
  begin
    Result := Items[I];
    if (Result.LayerOptions and OptionsMask) = 0 then
      Continue; // skip to the next one
    if Result.HitTest(X, Y) then Exit;
  end;
  Result := nil;
end;

procedure TLayerCollection.GDIUpdate;
begin
  if (FUpdateCount = 0) and Assigned(FOnGDIUpdate) then
    FOnGDIUpdate(Self);
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
    Notify(lnLayerInserted, Result, Index);
    Changed;
  finally
    EndUpdate;
  end;
end;

procedure TLayerCollection.InsertItem(Item: TCustomLayer);
var
  Index: Integer;
begin
  BeginUpdate;
  try
    Index := FItems.Add(Item);
    Item.FLayerCollection := Self;
    Notify(lnLayerAdded, Item, Index);
    Changed;
  finally
    EndUpdate;
  end;
end;

function TLayerCollection.LocalToViewport(const APoint: TFloatPoint; AScaled: Boolean): TFloatPoint;
var
  ScaleX, ScaleY, ShiftX, ShiftY: TFloat;
begin
  if AScaled then
  begin
    GetViewportShift(ShiftX, ShiftY);
    GetViewportScale(ScaleX, ScaleY);

    Result.X := APoint.X * ScaleX + ShiftX;
    Result.Y := APoint.Y * ScaleY + ShiftY;
  end
  else
    Result := APoint;
end;

function TLayerCollection.ViewportToLocal(const APoint: TFloatPoint; AScaled: Boolean): TFloatPoint;
var
  ScaleX, ScaleY, ShiftX, ShiftY: TFloat;
begin
  if AScaled then
  begin
    GetViewportShift(ShiftX, ShiftY);
    GetViewportScale(ScaleX, ScaleY);

    Result.X := (APoint.X - ShiftX) / ScaleX;
    Result.Y := (APoint.Y - ShiftY) / ScaleY;
  end
  else
    Result := APoint;
end;

function TLayerCollection.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): TCustomLayer;
begin
  if (MouseListener <> nil) then
    Result := MouseListener
  else
    Result := FindLayerAtPos(X, Y, LOB_MOUSE_EVENTS);

  if (Result <> MouseListener) and ((Result = nil) or (Result.FLayerOptions and LOB_NO_CAPTURE = 0)) then
    MouseListener := Result; // capture the mouse

  if (MouseListener <> nil) then
  begin
    Include(MouseListener.FLayerStates, CStateMap[Button]);
    MouseListener.MouseDown(Button, Shift, X, Y);
  end;
end;

function TLayerCollection.MouseMove(Shift: TShiftState; X, Y: Integer): TCustomLayer;
begin
  Result := MouseListener;
  if Result = nil then
    Result := FindLayerAtPos(X, Y, LOB_MOUSE_EVENTS);

  if (Result <> nil) then
    Result.MouseMove(Shift, X, Y)
  else
  if FOwner is TControl then
    Screen.Cursor := TControl(FOwner).Cursor;
end;

function TLayerCollection.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): TCustomLayer;
begin
  Result := MouseListener;
  if Result = nil then
    Result := FindLayerAtPos(X, Y, LOB_MOUSE_EVENTS);

  if (Result <> nil) then
  begin
    Exclude(Result.FLayerStates, CStateMap[Button]);
    Result.MouseUp(Button, Shift, X, Y);
  end;

  if (MouseListener <> nil) and
    (MouseListener.FLayerStates * [lsMouseLeft, lsMouseRight, lsMouseMiddle] = []) then
    MouseListener := nil; // reset mouse capture
end;

procedure TLayerCollection.Notify(Action: TLayerListNotification; Layer: TCustomLayer; Index: Integer);
var
  i: integer;
  LayerListNotification: ILayerListNotification;
begin
  if (FSubscribers <> nil) then
    for i := FSubscribers.Count-1 downto 0 do
      if (Supports(FSubscribers[i], ILayerListNotification, LayerListNotification)) then
        LayerListNotification.LayerListNotify(Layer, Action, Index);

  if Assigned(FOnListNotify) then
    FOnListNotify(Self, Action, Layer, Index);
end;

procedure TLayerCollection.RemoveItem(Item: TCustomLayer);
var
  Index: Integer;
begin
  BeginUpdate;
  try
    Index := FItems.IndexOf(Item);
    if Index >= 0 then
    begin
      FItems.Delete(Index);
      Item.FLayerCollection := nil;
      Notify(lnLayerDeleted, Item, Index);
    end;
    Changed;
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
    if (FMouseListener <> nil) then
      FMouseListener.FLayerStates := FMouseListener.FLayerStates - [lsMouseLeft, lsMouseRight, lsMouseMiddle];
    FMouseListener := Value;
  end;
end;

procedure TLayerCollection.Subscribe(const ASubscriber: IInterface);
begin
  if (FSubscribers = nil) then
    FSubscribers := TList<IInterface>.Create;

  FSubscribers.Add(ASubscriber);
end;

procedure TLayerCollection.Unsubscribe(const ASubscriber: IInterface);
begin
  if (FSubscribers <> nil) then
    FSubscribers.Remove(ASubscriber);
end;

procedure TLayerCollection.DoUpdateArea(const Rect: TRect; const Info: Cardinal);
var
  i: integer;
  UpdateRectNotification: IUpdateRectNotification;
begin
  if (FSubscribers <> nil) then
    for i := FSubscribers.Count-1 downto 0 do
      if (Supports(FSubscribers[i], IUpdateRectNotification, UpdateRectNotification)) then
        UpdateRectNotification.AreaUpdated(Rect, Info);

  if Assigned(FOnAreaUpdated) then
    FOnAreaUpdated(Self, Rect, Info);

  Changed;
end;

procedure TLayerCollection.DoUpdateLayer(Layer: TCustomLayer);
var
  i: integer;
  LayerUpdateNotification: ILayerUpdateNotification;
begin
  if (FSubscribers <> nil) then
    for i := FSubscribers.Count-1 downto 0 do
      if (Supports(FSubscribers[i], ILayerUpdateNotification, LayerUpdateNotification)) then
        LayerUpdateNotification.LayerUpdated(Layer);

  if Assigned(FOnLayerUpdated) then
    FOnLayerUpdated(Self, Layer);

  Changed;
end;

procedure TLayerCollection.GetViewportScale(out ScaleX, ScaleY: TFloat);
begin
  if Assigned(FOnGetViewportScale) then
    FOnGetViewportScale(Self, ScaleX, ScaleY)
  else
  begin
    ScaleX := 1;
    ScaleY := 1;
  end;
end;

procedure TLayerCollection.GetViewportShift(out ShiftX, ShiftY: TFloat);
begin
  if Assigned(FOnGetViewportShift) then
    FOnGetViewportShift(Self, ShiftX, ShiftY)
  else
  begin
    ShiftX := 0;
    ShiftY := 0;
  end;
end;


{ TLayerEnum }

constructor TLayerEnum.Create(ALayerCollection: TLayerCollection);
begin
  inherited Create;
  FLayerCollection := ALayerCollection;
  FIndex := -1;
end;

function TLayerEnum.GetCurrent: TCustomLayer;
begin
  Result := FLayerCollection.Items[FIndex];
end;

function TLayerEnum.MoveNext: Boolean;
begin
  Result := FIndex < Pred(FLayerCollection.Count);
  if Result then
    Inc(FIndex);
end;


{ TLayerCollectionHelper }

function TLayerCollectionHelper.GetEnumerator: TLayerEnum;
begin
  Result := TLayerEnum.Create(Self);
end;


{ TCustomLayer }

constructor TCustomLayer.Create(ALayerCollection: TLayerCollection);
begin
  LayerCollection := ALayerCollection;
  FLayerOptions := LOB_VISIBLE;
end;

destructor TCustomLayer.Destroy;
var
  I: Integer;
begin
  if (FFreeNotifies <> nil) then
  begin
    for I := FFreeNotifies.Count - 1 downto 0 do
    begin
      TCustomLayer(FFreeNotifies[I]).Notification(Self);
      if FFreeNotifies = nil then
        Break;
    end;
    FFreeNotifies.Free;
    FFreeNotifies := nil;
  end;
  SetLayerCollection(nil);
  inherited;
end;

procedure TCustomLayer.AddNotification(ALayer: TCustomLayer);
begin
  if (FFreeNotifies = nil) then
    FFreeNotifies := TList.Create;
  if FFreeNotifies.IndexOf(ALayer) < 0 then
    FFreeNotifies.Add(ALayer);
end;

procedure TCustomLayer.BeforeDestruction;
begin
  if Assigned(FOnDestroy) then
    FOnDestroy(Self);
  inherited;
end;

procedure TCustomLayer.BringToFront;
begin
  Index := LayerCollection.Count;
end;

procedure TCustomLayer.DoChanged;
begin
  if (FLayerCollection <> nil) and (FLayerOptions and LOB_NO_UPDATE = 0) then
  begin
    Update;

    if Visible then
      FLayerCollection.Changed
    else
    if (FLayerOptions and LOB_GDI_OVERLAY <> 0) then
      FLayerCollection.GDIUpdate;

    inherited;
  end;
end;

procedure TCustomLayer.Changed(const Rect: TRect; const Info: Cardinal);
begin
  if UpdateCount > 0 then
    Exit;

  if (FLayerCollection <> nil) and (FLayerOptions and LOB_NO_UPDATE = 0) then
  begin
    AreaUpdated(Rect, Info);
    inherited DoChanged;
  end;
end;

procedure TCustomLayer.Changing;
begin
  if LockUpdateCount > 0 then
    Exit;

  if UpdateCount > 0 then
    Exit;

  if Visible and (FLayerCollection <> nil) and (FLayerOptions and LOB_NO_UPDATE = 0) then
    FLayerCollection.Changing;
end;

procedure TCustomLayer.Click;
begin
  FClicked := False;
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TCustomLayer.DblClick;
begin
  FClicked := False;
  if Assigned(FOnDblClick) then
    FOnDblClick(Self);
end;

function TCustomLayer.DoHitTest(X, Y: Integer): Boolean;
begin
  Result := Visible;
end;

procedure TCustomLayer.DoPaint(Buffer: TBitmap32);
begin
  Paint(Buffer);
  if Assigned(FOnPaint) then
    FOnPaint(Self, Buffer);
end;

function TCustomLayer.GetIndex: Integer;
begin
  if (FLayerCollection <> nil) then
    Result := FLayerCollection.FItems.IndexOf(Self)
  else
    Result := -1;
end;

function TCustomLayer.GetMouseEvents: Boolean;
begin
  Result := (FLayerOptions and LOB_MOUSE_EVENTS <> 0);
end;

function TCustomLayer.GetOwner: TPersistent;
begin
  Result := FLayerCollection;
end;

function TCustomLayer.GetVisible: Boolean;
begin
  Result := (FLayerOptions and LOB_VISIBLE <> 0);
end;

function TCustomLayer.HitTest(X, Y: Integer): Boolean;
begin
  Result := DoHitTest(X, Y);
  if Assigned(FOnHitTest) then
    FOnHitTest(Self, X, Y, Result);
end;

//------------------------------------------------------------------------------

function TCustomLayer.ControlToLayer(const p: TPoint; AScaleToContent: boolean): TPoint;
begin
  Result := p;
end;

function TCustomLayer.ControlToLayer(const r: TRect; AScaleToContent: boolean): TRect;
begin
  Result := r;
end;

function TCustomLayer.ControlToLayer(const r: TFloatRect; AScaleToContent: boolean): TFloatRect;
begin
  Result := r;
end;

function TCustomLayer.ControlToLayer(const p: TFloatPoint; AScaleToContent: boolean): TFloatPoint;
begin
  Result := p;
end;

//------------------------------------------------------------------------------

function TCustomLayer.LayerToControl(const r: TRect; AScaleFromContent: boolean): TRect;
begin
  Result := r;
end;

function TCustomLayer.LayerToControl(const p: TPoint; AScaleFromContent: boolean): TPoint;
begin
  Result := p;
end;

function TCustomLayer.LayerToControl(const p: TFloatPoint; AScaleFromContent: boolean): TFloatPoint;
begin
  Result := p;
end;

function TCustomLayer.LayerToControl(const r: TFloatRect; AScaleFromContent: boolean): TFloatRect;
begin
  Result := r;
end;

//------------------------------------------------------------------------------

procedure TCustomLayer.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) then
  begin
    if (ssDouble in Shift) then
      DblClick
    else
      FClicked := True;
  end;
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, Button, Shift, X, Y);
end;

procedure TCustomLayer.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  Screen.Cursor := Cursor;
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, Shift, X, Y);
end;

procedure TCustomLayer.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Screen.Cursor := crDefault;
  if (Button = mbLeft) and FClicked then
    Click;
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, Button, Shift, X, Y);
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
  if (FFreeNotifies <> nil) then
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
    if FLayerCollection.MouseListener = Self then
      Screen.Cursor := Value;
  end;
end;

procedure TCustomLayer.SetIndex(Value: Integer);
var
  CurIndex: Integer;
begin
  CurIndex := GetIndex;
  if (CurIndex < 0) or (CurIndex = Value) then
    exit;

  if Value < 0 then
    Value := 0;
  if Value >= FLayerCollection.Count then
    Value := FLayerCollection.Count - 1;

  if Value <> CurIndex then
  begin
    FLayerCollection.BeginUpdate;
    try
      FLayerCollection.FItems.Move(CurIndex, Value);

      if Visible then
        FLayerCollection.Changed;
    finally
      FLayerCollection.EndUpdate;
    end;
  end;
end;

procedure TCustomLayer.SetLayerCollection(Value: TLayerCollection);
begin
  if FLayerCollection <> Value then
  begin
    if (FLayerCollection <> nil) then
    begin
      if FLayerCollection.MouseListener = Self then
        FLayerCollection.MouseListener := nil;
      FLayerCollection.RemoveItem(Self);
    end;
    if (Value <> nil) then
      Value.InsertItem(Self);
    FLayerCollection := Value;
  end;
end;

procedure TCustomLayer.SetLayerOptions(Value: Cardinal);
begin
  if (FLayerOptions <> Value) then
  begin
    Changing;
    FLayerOptions := Value;
    Changed;
  end;
end;

procedure TCustomLayer.SetMouseEvents(Value: Boolean);
begin
  if Value then
    LayerOptions := LayerOptions or LOB_MOUSE_EVENTS
  else
    LayerOptions := LayerOptions and not LOB_MOUSE_EVENTS;
end;

procedure TCustomLayer.SetVisible(Value: Boolean);
begin
  if Value then
    LayerOptions := LayerOptions or LOB_VISIBLE
  else
  begin
    ForceUpdate := True;
    LayerOptions := LayerOptions and not LOB_VISIBLE;
    ForceUpdate := False;    
  end;
end;

procedure TCustomLayer.Update;
begin
  if (FLayerCollection <> nil) and (Visible or ForceUpdate) then
    FLayerCollection.DoUpdateLayer(Self);
end;

procedure TCustomLayer.Update(const ARect: TRect);
begin
  UpdateRect(ARect);
end;

procedure TCustomLayer.UpdateRect(const ARect: TRect);
begin
  AreaUpdated(ARect, AREAINFO_RECT);
end;

procedure TCustomLayer.AreaUpdated(const AArea: TRect; const AInfo: Cardinal);
begin
  // Note: Rect is in ViewPort coordinates

  if (FLayerCollection = nil) then
    exit;

  if (Visible or ForceUpdate) then
    FLayerCollection.DoUpdateArea(AArea, AInfo)
  else
  if (FLayerOptions and LOB_GDI_OVERLAY) <> 0 then
    FLayerCollection.GDIUpdate;
end;

function TCustomLayer.GetInvalid: Boolean;
begin
  Result := (LayerOptions and LOB_INVALID <> 0);
end;

procedure TCustomLayer.SetInvalid(Value: Boolean);
begin
  // don't use LayerOptions here since this is internal and we don't want to
  // trigger Changing and Changed as this will definitely cause a stack overflow.
  if Value then
    FLayerOptions := FLayerOptions or LOB_INVALID
  else
    FLayerOptions := FLayerOptions and not LOB_INVALID;
end;

function TCustomLayer.GetForceUpdate: Boolean;
begin
  Result := (LayerOptions and LOB_FORCE_UPDATE <> 0);
end;

procedure TCustomLayer.SetForceUpdate(Value: Boolean);
begin
  // don't use LayerOptions here since this is internal and we don't want to
  // trigger Changing and Changed as this will definitely cause a stack overflow.
  if Value then
    FLayerOptions := FLayerOptions or LOB_FORCE_UPDATE
  else
    FLayerOptions := FLayerOptions and not LOB_FORCE_UPDATE;
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
  with GetAdjustedRect(FLocation) do
    Result := (X >= Left) and (X < Right) and (Y >= Top) and (Y < Bottom) and
      inherited DoHitTest(X, Y);
end;

procedure TPositionedLayer.DoSetLocation(const NewLocation: TFloatRect);
begin
  FLocation := NewLocation;
end;

//------------------------------------------------------------------------------

function TPositionedLayer.GetAdjustedLocation: TFloatRect;
begin
  Result := GetAdjustedRect(FLocation);
end;

function TPositionedLayer.GetAdjustedRect(const R: TFloatRect): TFloatRect;
var
  ScaleX, ScaleY, ShiftX, ShiftY: TFloat;
begin
  if Scaled and (FLayerCollection <> nil) then
  begin
    FLayerCollection.GetViewportShift(ShiftX, ShiftY);
    FLayerCollection.GetViewportScale(ScaleX, ScaleY);

    with Result do
    begin
      Left := R.Left * ScaleX + ShiftX;
      Top := R.Top * ScaleY + ShiftY;
      Right := R.Right * ScaleX + ShiftX;
      Bottom := R.Bottom * ScaleY + ShiftY;
    end;
  end
  else
    Result := R;
end;

//------------------------------------------------------------------------------

function TPositionedLayer.GetContentSize: TPoint;
begin
  Result.X := 0;
  Result.Y := 0;
end;

//------------------------------------------------------------------------------

function TPositionedLayer.ControlToLayer(const APoint: TPoint; AScaleToContent: boolean): TPoint;
begin
  Result := GR32.Point(ControlToLayer(FloatPoint(APoint), AScaleToContent));
end;

function TPositionedLayer.ControlToLayer(const ARect: TRect; AScaleToContent: boolean): TRect;
begin
  Result := MakeRect(ControlToLayer(FloatRect(ARect), AScaleToContent), rrOutside);
end;

function TPositionedLayer.ControlToLayer(const APoint: TFloatPoint; AScaleToContent: boolean): TFloatPoint;
var
  ViewPort: TFloatRect;
  Size: TPoint;
  LayerWidth, LayerHeight: TFloat;
begin
  if Scaled and (FLayerCollection <> nil) then
  begin
    ViewPort := GetAdjustedLocation;
    Result := APoint - ViewPort.TopLeft;

    if (AScaleToContent) then
    begin
      LayerWidth := ViewPort.Width;
      LayerHeight := ViewPort.Height;

      Size := GetContentSize;

      if (not Size.IsZero) and (LayerWidth > 0.5) and (LayerHeight > 0.5) and
        ((Size.X <> LayerWidth) or (Size.Y <> LayerHeight)) then
      begin
        Result.X := Result.X * Size.X / LayerWidth;
        Result.Y := Result.Y * Size.Y / LayerHeight;
      end;
    end;
  end else
    Result := APoint - FLocation.TopLeft;
end;

function TPositionedLayer.ControlToLayer(const ARect: TFloatRect; AScaleToContent: boolean): TFloatRect;
var
  ViewPort: TFloatRect;
  Size: TPoint;
  LayerWidth, LayerHeight: TFloat;
begin
  Result := ARect;

  if Scaled and (FLayerCollection <> nil) then
  begin
    ViewPort := GetAdjustedLocation;
    Result.Offset(-ViewPort.Top, -ViewPort.Left);

    if (AScaleToContent) then
    begin
      LayerWidth := ViewPort.Width;
      LayerHeight := ViewPort.Height;

      Size := GetContentSize;

      if (not Size.IsZero) and (LayerWidth > 0.5) and (LayerHeight > 0.5) and
        ((Size.X <> LayerWidth) or (Size.Y <> LayerHeight)) then
      begin
        LayerWidth := Size.X / LayerWidth;
        LayerHeight := Size.Y / LayerHeight;

        Result.Left := Result.Left * LayerWidth;
        Result.Top := Result.Top * LayerHeight;
        Result.Right := Result.Right * LayerWidth;
        Result.Bottom := Result.Bottom * LayerHeight;
      end;
    end;
  end else
    Result.Offset(-FLocation.Top, -FLocation.Left);
end;

//------------------------------------------------------------------------------

function TPositionedLayer.LayerToControl(const ARect: TRect; AScaleFromContent: boolean): TRect;
begin
  Result := MakeRect(LayerToControl(FloatRect(ARect), AScaleFromContent), rrOutside);
end;

function TPositionedLayer.LayerToControl(const APoint: TPoint; AScaleFromContent: boolean): TPoint;
begin
  Result := GR32.Point(LayerToControl(FloatPoint(APoint), AScaleFromContent));
end;

function TPositionedLayer.LayerToControl(const APoint: TFloatPoint; AScaleFromContent: boolean): TFloatPoint;
var
  ViewPort: TFloatRect;
  Size: TPoint;
  LayerWidth, LayerHeight: TFloat;
begin
  Result := APoint;

  if Scaled and (FLayerCollection <> nil) then
  begin
    ViewPort := GetAdjustedLocation;

    if (AScaleFromContent) then
    begin
      LayerWidth := ViewPort.Width;
      LayerHeight := ViewPort.Height;

      Size := GetContentSize;

      if (not Size.IsZero) and (LayerWidth > 0.5) and (LayerHeight > 0.5) and
        ((Size.X <> LayerWidth) or (Size.Y <> LayerHeight)) then
      begin
        Result.X := Result.X * LayerWidth / Size.X;
        Result.Y := Result.Y * LayerHeight / Size.Y;
      end;
    end;

    Result.Offset(ViewPort.TopLeft);
  end else
    Result.Offset(FLocation.TopLeft);
end;

function TPositionedLayer.LayerToControl(const ARect: TFloatRect; AScaleFromContent: boolean): TFloatRect;
var
  ViewPort: TFloatRect;
  Size: TPoint;
  LayerWidth, LayerHeight: TFloat;
begin
  Result := ARect;

  if Scaled and (FLayerCollection <> nil) then
  begin
    ViewPort := GetAdjustedLocation;

    if (AScaleFromContent) then
    begin
      LayerWidth := ViewPort.Width;
      LayerHeight := ViewPort.Height;

      Size := GetContentSize;

      if (not Size.IsZero) and (LayerWidth > 0.5) and (LayerHeight > 0.5) and
        ((Size.X <> LayerWidth) or (Size.Y <> LayerHeight)) then
      begin
        LayerWidth := LayerWidth / Size.X;
        LayerHeight := LayerHeight / Size.Y;

        Result.Left := Result.Left * LayerWidth;
        Result.Top := Result.Top * LayerHeight;
        Result.Right := Result.Right * LayerWidth;
        Result.Bottom := Result.Bottom * LayerHeight;
      end;
    end;

    Result.Offset(ViewPort.Top, ViewPort.Left);
  end else
    Result.Offset(FLocation.Top, FLocation.Left);
end;

//------------------------------------------------------------------------------

function TPositionedLayer.DoGetUpdateRect: TRect;
begin
  // Note: Result is in ViewPort coordinates
  Result := MakeRect(GetAdjustedLocation, rrOutside);
end;

function TPositionedLayer.GetUpdateRect: TRect;
begin
  Result := DoGetUpdateRect;

  if (Assigned(FOnGetUpdateRect)) then
    FOnGetUpdateRect(Self, Result);
end;

procedure TPositionedLayer.SetLocation(const Value: TFloatRect);
begin
  if (GR32.EqualRect(Value, FLocation)) then
    exit;

  Changing;

  // Invalidate old location
  if (FLayerCollection <> nil) and (FLayerOptions and LOB_NO_UPDATE = 0) then
    Update;

  DoSetLocation(Value);

  // Invalidate new location
  Changed;
end;

procedure TPositionedLayer.SetScaled(Value: Boolean);
begin
  if (Value = FScaled) then
    exit;

  // Changing Scaled can change size and position so treat it as if we did
  Changing;

  // Invalidate old location
  if (FLayerCollection <> nil) and (FLayerOptions and LOB_NO_UPDATE = 0) then
    Update;

  FScaled := Value;

  // Invalidate new location
  Changed;
end;

procedure TPositionedLayer.Update;
begin
  UpdateRect(GetUpdateRect);
end;

{ TCustomIndirectBitmapLayer }

constructor TCustomIndirectBitmapLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited Create(ALayerCollection);
end;

constructor TCustomIndirectBitmapLayer.Create(ALayerCollection: TLayerCollection; ABitmap: TCustomBitmap32);
begin
  inherited Create(ALayerCollection);
  DoSetBitmap(ABitmap);
end;

destructor TCustomIndirectBitmapLayer.Destroy;
begin
  if (OwnsBitmap) then
    FreeAndNil(FBitmap)
  else
    DoSetBitmap(nil);
  inherited;
end;

procedure TCustomIndirectBitmapLayer.BitmapAreaChanged(Sender: TObject; const Area: TRect; const Info: Cardinal);
var
  T: TRect;
  ScaleX, ScaleY: TFloat;
  Width: Integer;
  r: TFloatRect;
begin
  if (FBitmap.Empty) then
    Exit;

  if (Area.Left = Area.Right) or (Area.Top = Area.Bottom) then // Don't use IsEmpty; Rect can be negative
    Exit; // Empty area

  if (FLayerCollection = nil) or (FLayerOptions and LOB_NO_UPDATE <> 0) then
    exit;

  r := GetAdjustedLocation;

  ScaleX := r.Width / FBitmap.Width;
  ScaleY := r.Height / FBitmap.Height;

  // Common case: Positive rect
  // More rare: Negative rect (e.g. line going from right to left)
  if (Area.Left < Area.Right) then
  begin
    T.Left := Floor(r.Left + Area.Left * ScaleX);
    T.Right := Ceil(r.Left + Area.Right * ScaleX);
  end else
  begin
    T.Left := Ceil(r.Left + Area.Left * ScaleX);
    T.Right := Floor(r.Left + Area.Right * ScaleX);
  end;

  if (Area.Top < Area.Bottom) then
  begin
    T.Top := Floor(r.Top + Area.Top * ScaleY);
    T.Bottom := Ceil(r.Top + Area.Bottom * ScaleY);
  end else
  begin
    T.Top := Ceil(r.Top + Area.Top * ScaleY);
    T.Bottom := Floor(r.Top + Area.Bottom * ScaleY);
  end;

  // TODO : Possible scaling issue here; Should Width be scaled?
  // See: TCustomImage32.BitmapAreaChangeHandler
  Width := Ceil(FBitmap.Resampler.Width);
  InflateArea(T, Width, Width);

  Changed(T, Info);
end;

function TCustomIndirectBitmapLayer.DoHitTest(X, Y: Integer): Boolean;
var
  BitmapX, BitmapY: Integer;
  LayerWidth, LayerHeight: TFloat;
  r: TFloatRect;
begin
  Result := inherited DoHitTest(X, Y);

  if (Result) and (AlphaHit) and (FBitmap <> nil) then
  begin
    r := GetAdjustedRect(FLocation);

    LayerWidth := r.Width;
    LayerHeight := r.Height;

    if (LayerWidth < 0.5) or (LayerHeight < 0.5) then
      Result := False
    else
    begin
      // check the pixel alpha at (X, Y) position
      BitmapX := Round((X - r.Left) * FBitmap.Width / LayerWidth);
      BitmapY := Round((Y - r.Top) * FBitmap.Height / LayerHeight);
      if (FBitmap.PixelS[BitmapX, BitmapY] and $FF000000 = 0) then
        Result := False;
    end;
  end;
end;

procedure TCustomIndirectBitmapLayer.Paint(Buffer: TBitmap32);
var
  SrcRect, DstRect, ClipRect, TempRect: TRect;
  ImageRect: TRect;
begin
  if (FBitmap = nil) or (FBitmap.Empty) then
    Exit;

  DstRect := MakeRect(GetAdjustedRect(FLocation));
  ClipRect := Buffer.ClipRect;
  GR32.IntersectRect(TempRect, ClipRect, DstRect);
  if GR32.IsRectEmpty(TempRect) then
    Exit;

  SrcRect := MakeRect(0, 0, FBitmap.Width, FBitmap.Height);
  if Cropped and (LayerCollection.FOwner is TCustomImage32) and
    not (TImage32Access(LayerCollection.FOwner).PaintToMode) then
  begin
    if (DstRect.Width < 0.5) or (DstRect.Height < 0.5) then
      Exit;
    ImageRect := TCustomImage32(LayerCollection.FOwner).GetBitmapRect;
    GR32.IntersectRect(ClipRect, ClipRect, ImageRect);
  end;
  StretchTransfer(Buffer, DstRect, ClipRect, FBitmap, SrcRect, FBitmap.Resampler, FBitmap.DrawMode, FBitmap.OnPixelCombine);
end;

procedure TCustomIndirectBitmapLayer.DoSetBitmap(Value: TCustomBitmap32);
begin
  if (Value = FBitmap) then
    exit;

  if (FBitmap <> nil) then
    FBitmap.OnAreaChanged := nil;

  FBitmap := Value;

  if (FBitmap <> nil) then
    FBitmap.OnAreaChanged := BitmapAreaChanged;
end;

function TCustomIndirectBitmapLayer.GetContentSize: TPoint;
begin
  Result.X := Bitmap.Width;
  Result.Y := Bitmap.Height;
end;

function TCustomIndirectBitmapLayer.OwnsBitmap: boolean;
begin
  Result := False;
end;

procedure TCustomIndirectBitmapLayer.SetBitmap(Value: TCustomBitmap32);
begin
  DoSetBitmap(Value);
  Changed;
end;

procedure TCustomIndirectBitmapLayer.SetCropped(Value: Boolean);
begin
  if (Value <> FCropped) then
  begin
    FCropped := Value;
    Changed;
  end;
end;


{ TCustomBitmapLayer }

constructor TCustomBitmapLayer.Create(ALayerCollection: TLayerCollection);
var
  LayerBitmap: TCustomBitmap32;
begin
  LayerBitmap := CreateBitmap;
  try

    inherited Create(ALayerCollection, LayerBitmap);

  except
    if (Bitmap = nil) then
      LayerBitmap.Free; // Free if we didn't take ownership of the bitmap
    raise;
  end;
end;

function TCustomBitmapLayer.OwnsBitmap: boolean;
begin
  Result := True;
end;

function TCustomBitmapLayer.CreateBitmap: TCustomBitmap32;
begin
  Result := GetBitmapClass.Create;
end;

procedure TCustomBitmapLayer.SetBitmap(Value: TCustomBitmap32);
begin
  Bitmap.Assign(Value);
end;

{ TBitmapLayer }

function TBitmapLayer.GetBitmap: TBitmap32;
begin
  Result := TBitmap32(inherited Bitmap);
end;

procedure TBitmapLayer.SetBitmap(Value: TBitmap32);
begin
  inherited SetBitmap(Value);
end;

function TBitmapLayer.GetBitmapClass: TCustomBitmap32Class;
begin
  Result := TBitmap32;
end;


{ TRubberbandPassMouse }

constructor TRubberbandPassMouse.Create(AOwner: TRubberbandLayer);
begin
  FOwner := AOwner;
  FEnabled := False;
  FToChild := False;
  FLayerUnderCursor := False;
  FCancelIfPassed := False;
end;

function TRubberbandPassMouse.GetChildUnderCursor(X, Y: Integer): TPositionedLayer;
var
  Layer: TCustomLayer;
  Index: Integer;
begin
  Result := nil;
  for Index := FOwner.LayerCollection.Count - 1 downto 0 do
  begin
    Layer := FOwner.LayerCollection.Items[Index];
    if ((Layer.LayerOptions and LOB_MOUSE_EVENTS) > 0) and
      (Layer is TPositionedLayer) and Layer.HitTest(X, Y) then
    begin
      Result := TPositionedLayer(Layer);
      Exit;
    end;
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
  FQuantized := 8;
  FLayerOptions := LOB_VISIBLE or LOB_MOUSE_EVENTS;
  SetFrameStipple([clWhite32, clWhite32, clBlack32, clBlack32]);
  FPassMouse := TRubberbandPassMouse.Create(Self);
  FFrameStippleStep := 1;
  FFrameStippleCounter := 0;
end;

destructor TRubberbandLayer.Destroy;
begin
  FPassMouse.Free;
  inherited;
end;

function TRubberbandLayer.DoHitTest(X, Y: Integer): Boolean;
begin
  if (Visible) then
    Result := (GetDragState(X, Y) <> dsNone)
  else
    Result := False;
end;

procedure TRubberbandLayer.DoResizing(var OldLocation,
  NewLocation: TFloatRect; DragState: TRBDragState; Shift: TShiftState);
begin
  if Assigned(FOnResizing) then
    FOnResizing(Self, OldLocation, NewLocation, DragState, Shift);
end;

procedure TRubberbandLayer.DoConstrain(var OldLocation,
  NewLocation: TFloatRect; DragState: TRBDragState; Shift: TShiftState);
begin
  if Assigned(FOnConstrain) then
    FOnConstrain(Self, OldLocation, NewLocation, DragState, Shift);
end;

procedure TRubberbandLayer.DoSetLocation(const NewLocation: TFloatRect);
begin
  inherited;
  UpdateChildLayer;
end;

function SnapAngleTo45(Angle: integer): integer;
begin
  Result := (((Angle + 45 div 2) div 45) * 45 + 360) mod 360;
end;

function AngleToDirection(Angle: integer): TResizeDirection;
begin
  Result := TResizeDirection(SnapAngleTo45(Angle) div 45);
end;

function TRubberbandLayer.GetHandleCursor(DragState: TRBDragState; Angle: integer): TCursor;
var
  Direction: TResizeDirection;
begin
  if (DragState in [dsNone, dsMove]) then
    Exit(Cursor);

  Direction := AngleToDirection(Angle);

  Result := DirectionCursors[Direction];
end;

function TRubberbandLayer.GetDragState(X, Y: Integer): TRBDragState;
var
  R: TRect;
  dh_center, dh_sides, dh_corners: Boolean;
  dl, dt, dr, db, dx, dy: Boolean;
  Sz: Integer;
const
  DragZone = 1;
begin
  Result := dsNone;
  Sz := Ceil(FHandleSize + DragZone);
  dh_center := rhCenter in FHandles;
  dh_sides := rhSides in FHandles;
  dh_corners := rhCorners in FHandles;

  R := MakeRect(GetAdjustedRect(FLocation));
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
  else if dh_center and GR32.PtInRect(R, GR32.Point(X, Y)) then Result := dsMove;
end;

procedure TRubberbandLayer.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  PositionedLayer: TPositionedLayer;
begin
  if FPassMouse.Enabled then
  begin
    if FPassMouse.ToLayerUnderCursor then
      PositionedLayer := FPassMouse.GetChildUnderCursor(X, Y)
    else
      PositionedLayer := ChildLayer;

    if FPassMouse.ToChild and (ChildLayer <> nil) then
    begin
      ChildLayer.MouseDown(Button, Shift, X, Y);
      if FPassMouse.CancelIfPassed then
        Exit;
    end;

    if (PositionedLayer <> ChildLayer) and (PositionedLayer <> nil) then
    begin
      PositionedLayer.MouseDown(Button, Shift, X, Y);
      if FPassMouse.CancelIfPassed then
        Exit;
    end;
  end;

  if FIsDragging then Exit;
  SetDragState(GetDragState(X, Y), X, Y);
  inherited;
end;

procedure TRubberbandLayer.MouseMove(Shift: TShiftState; X, Y: Integer);

  procedure IncLT(var LT, RB: TFloat; Delta, MinSize, MaxSize: TFloat);
  begin
    LT := LT + Delta;
    if RB - LT < MinSize then
      LT := RB - MinSize;
    if MaxSize >= MinSize then
      if RB - LT > MaxSize then
        LT := RB - MaxSize;
  end;

  procedure IncRB(var LT, RB: TFloat; Delta, MinSize, MaxSize: TFloat);
  begin
    RB := RB + Delta;
    if RB - LT < MinSize then
      RB := LT + MinSize;
    if MaxSize >= MinSize then
      if RB - LT > MaxSize then
        RB := LT + MaxSize;
  end;

var
  Mx, My: TFloat;
  L, T, R, B, W, H: TFloat;
  LQuantize: Boolean;
  ALoc, NewLocation: TFloatRect;
  Angle: integer;
const
  DragStateToAngle: array[TRBDragState] of integer = (-1, -1, 180, 90, 0, 270, 135, 45, 225, 315);
begin
  if not FIsDragging then
  begin
    FDragState := GetDragState(X, Y);
    Angle := DragStateToAngle[FDragState];
    Screen.Cursor := GetHandleCursor(FDragState, Angle);
  end
  else
  begin
    Mx := X - FMouseShift.X;
    My := Y - FMouseShift.Y;
    if Scaled then
    with Location do
    begin
      ALoc := GetAdjustedRect(FLocation);
      if GR32.IsRectEmpty(ALoc) then Exit;
      Mx := (Mx - ALoc.Left) / (ALoc.Right - ALoc.Left) * (Right - Left) + Left;
      My := (My - ALoc.Top) / (ALoc.Bottom - ALoc.Top) * (Bottom - Top) + Top;
    end;

    with FOldLocation do
    begin
      L := Left;
      T := Top;
      R := Right;
      B := Bottom;
      W := R - L;
      H := B - T;
    end;

    LQuantize := (roQuantized in Options) and not (ssAlt in Shift);

    if FDragState = dsMove then
    begin
      L := Mx;
      T := My;
      if LQuantize then
      begin
        L := Round(L / FQuantized) * FQuantized;
        T := Round(T / FQuantized) * FQuantized;
      end;
      R := L + W;
      B := T + H;
    end
    else
    begin
      if FDragState in [dsSizeL, dsSizeTL, dsSizeBL] then
      begin
        IncLT(L, R, Mx - L, MinWidth, MaxWidth);
        if LQuantize then
          L := Round(L / FQuantized) * FQuantized;
      end;

      if FDragState in [dsSizeR, dsSizeTR, dsSizeBR] then
      begin
        IncRB(L, R, Mx - R, MinWidth, MaxWidth);
        if LQuantize then
          R := Round(R / FQuantized) * FQuantized;
      end;

      if FDragState in [dsSizeT, dsSizeTL, dsSizeTR] then
      begin
        IncLT(T, B, My - T, MinHeight, MaxHeight);
        if LQuantize then
          T := Round(T / FQuantized) * FQuantized;
      end;

      if FDragState in [dsSizeB, dsSizeBL, dsSizeBR] then
      begin
        IncRB(T, B, My - B, MinHeight, MaxHeight);
        if LQuantize then
          B := Round(B / FQuantized) * FQuantized;
      end;
    end;

    NewLocation := FloatRect(L, T, R, B);

    if roConstrained in FOptions then
      DoConstrain(FOldLocation, NewLocation, FDragState, Shift);

    if roProportional in FOptions then
    begin
      case FDragState of
        dsSizeB, dsSizeBR:
          NewLocation.Right := FOldLocation.Left + (FOldLocation.Right - FOldLocation.Left) * (NewLocation.Bottom - NewLocation.Top) / (FOldLocation.Bottom - FOldLocation.Top);
        dsSizeT, dsSizeTL:
          NewLocation.Left := FOldLocation.Right - (FOldLocation.Right - FOldLocation.Left) * (NewLocation.Bottom - NewLocation.Top) / (FOldLocation.Bottom - FOldLocation.Top);
        dsSizeR, dsSizeBL:
          NewLocation.Bottom := FOldLocation.Top + (FOldLocation.Bottom - FOldLocation.Top) * (NewLocation.Right - NewLocation.Left) / (FOldLocation.Right - FOldLocation.Left);
        dsSizeL, dsSizeTR:
          NewLocation.Top := FOldLocation.Bottom - (FOldLocation.Bottom - FOldLocation.Top) * (NewLocation.Right - NewLocation.Left) / (FOldLocation.Right - FOldLocation.Left);
      end;
    end;

    DoResizing(FOldLocation, NewLocation, FDragState, Shift);

    if (NewLocation.Left <> Location.Left) or
      (NewLocation.Right <> Location.Right) or
      (NewLocation.Top <> Location.Top) or
      (NewLocation.Bottom <> Location.Bottom) then
    begin
      Location := NewLocation;
      if Assigned(FOnUserChange) then
        FOnUserChange(Self);
    end;
  end;
end;

procedure TRubberbandLayer.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  PositionedLayer: TPositionedLayer;
begin
  if FPassMouse.Enabled then
  begin
    if FPassMouse.ToLayerUnderCursor then
      PositionedLayer := FPassMouse.GetChildUnderCursor(X, Y)
    else
      PositionedLayer := ChildLayer;

    if FPassMouse.ToChild and (ChildLayer <> nil) then
    begin
      ChildLayer.MouseUp(Button, Shift, X, Y);
      if FPassMouse.CancelIfPassed then
        Exit;
    end;

    if (PositionedLayer <> ChildLayer) and (PositionedLayer <> nil) then
    begin
      PositionedLayer.MouseUp(Button, Shift, X, Y);
      if FPassMouse.CancelIfPassed then
        Exit;
    end;
  end;

  FIsDragging := False;
  inherited;
end;

procedure TRubberbandLayer.Notification(ALayer: TCustomLayer);
begin
  if ALayer = FChildLayer then
    FChildLayer := nil;
end;

procedure TRubberbandLayer.DrawHandle(Buffer: TBitmap32; X, Y: TFloat);
var
  HandleRect: TRect;
begin
  // Coordinate specifies exact center of handle. I.e. center of
  // pixel if handle is odd number of pixels wide.

  HandleRect.Left := Floor(X - FHandleSize);
  HandleRect.Right := HandleRect.Left + Ceil(FHandleSize*2);
  HandleRect.Top := Floor(Y - FHandleSize);
  HandleRect.Bottom := HandleRect.Top + Ceil(FHandleSize*2);

  Buffer.FrameRectTS(HandleRect, FHandleFrame);

  GR32.InflateRect(HandleRect, -1, -1);
  Buffer.FillRectTS(HandleRect, FHandleFill);
end;

procedure TRubberbandLayer.DoHandles(Buffer: TBitmap32; const R: TRect;
  DoFrame: TRBPaintFrameHandler;
  DoHandle: TRBPaintHandleHandler);
var
  CenterX, CenterY: TFloat;
begin
  with R do
  begin
    if rhFrame in FHandles then
      DoFrame(Buffer, R);

    if rhCorners in FHandles then
    begin
      if not(rhNotTLCorner in FHandles) then
        DoHandle(Buffer, Left+0.5, Top+0.5);
      if not(rhNotTRCorner in FHandles) then
        DoHandle(Buffer, Right-0.5, Top+0.5);
      if not(rhNotBLCorner in FHandles) then
        DoHandle(Buffer, Left+0.5, Bottom-0.5);
      if not(rhNotBRCorner in FHandles) then
        DoHandle(Buffer, Right-0.5, Bottom-0.5);
    end;
    if rhSides in FHandles then
    begin
      CenterX := (Left + Right) / 2;
      CenterY := (Top + Bottom) / 2;
      if not(rhNotTopSide in FHandles) then
        DoHandle(Buffer, CenterX, Top+0.5);
      if not(rhNotLeftSide in FHandles) then
        DoHandle(Buffer, Left+0.5, CenterY);
      if not(rhNotRightSide in FHandles) then
        DoHandle(Buffer, Right-0.5, CenterY);
      if not(rhNotBottomSide in FHandles) then
        DoHandle(Buffer, CenterX, Bottom-0.5);
    end;
  end;
end;

procedure TRubberbandLayer.DrawFrame(Buffer: TBitmap32; const R: TRect);
begin
  Buffer.SetStipple(FFrameStipplePattern);
  Buffer.StippleCounter := 0;
  Buffer.StippleStep := FFrameStippleStep;
  Buffer.StippleCounter := FFrameStippleCounter;
  Buffer.FrameRectTSP(R.Left, R.Top, R.Right, R.Bottom);
end;

procedure TRubberbandLayer.Paint(Buffer: TBitmap32);
var
  R: TRect;
begin
  R := MakeRect(GetAdjustedRect(FLocation));

  DoHandles(Buffer, R, DrawFrame, DrawHandle);
end;

procedure TRubberbandLayer.Quantize;
begin
  Location := FloatRect(
    Round(Location.Left / Quantized) * Quantized,
    Round(Location.Top / Quantized) * Quantized,
    Round(Location.Right / Quantized) * Quantized,
    Round(Location.Bottom / Quantized) * Quantized);
end;

procedure TRubberbandLayer.SetChildLayer(Value: TPositionedLayer);
begin
  if (FChildLayer <> nil) then
    RemoveNotification(FChildLayer);
    
  FChildLayer := Value;

  if (FChildLayer <> nil) then
  begin
    BeginUpdate;
    try
      Location := FChildLayer.Location;
      Scaled := FChildLayer.Scaled;
    finally
      EndUpdate;
    end;
    AddNotification(FChildLayer);
  end;
end;

procedure TRubberbandLayer.SetDragState(const Value: TRBDragState);
begin
  SetDragState(Value, 0, 0);
end;

procedure TRubberbandLayer.SetDragState(const Value: TRBDragState; const X, Y: Integer);
var
  ALoc: TFloatRect;
begin
  FDragState := Value;
  FIsDragging := FDragState <> dsNone;

  if FIsDragging then
  begin
    FOldLocation := Location;

    ALoc := GetAdjustedRect(FLocation);

    case FDragState of
      dsMove: FMouseShift := FloatPoint(X - ALoc.Left, Y - ALoc.Top);
    else
      FMouseShift := FloatPoint(0, 0);
    end;
  end;
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
    // Erase old
    Update;

    FHandles := Value;

    // Paint new
    Update;
  end;
end;

procedure TRubberbandLayer.SetHandleSize(Value: TFloat);
begin
  if Value < 1 then
    Value := 1;

  if Value <> FHandleSize then
  begin
    // Erase old
    Update;

    FHandleSize := Value;

    // Paint new
    Update;
  end;
end;

procedure TRubberbandLayer.SetFrameStipple(const Value: Array of TColor32);
var
  L: Integer;
begin
  L := High(Value) + 1;
  SetLength(FFrameStipplePattern, L);
  MoveLongword(Value[0], FFrameStipplePattern[0], L);
end;

procedure TRubberbandLayer.SetFrameStippleStep(const Value: TFloat);
begin
  if Value <> FFrameStippleStep then
  begin
    FFrameStippleStep := Value;

    // Repaint
    Update;
  end;
end;

procedure TRubberbandLayer.UpdateFrame(Buffer: TBitmap32; const R: TRect);
begin
  // Left
  UpdateRect(Rect(R.Left, R.Top, R.Left+1, R.Bottom));
  // Right
  UpdateRect(Rect(R.Right-1, R.Top, R.Right, R.Bottom));
  // Top
  UpdateRect(Rect(R.Left+1, R.Top, R.Right-1, R.Top+1));
  // Bottom
  UpdateRect(Rect(R.Left+1, R.Bottom-1, R.Right-1, R.Bottom));
end;

procedure TRubberbandLayer.UpdateHandle(Buffer: TBitmap32; X, Y: TFloat);
var
  HandleRect: TRect;
begin
  HandleRect.Left := Floor(X - FHandleSize);
  HandleRect.Right := HandleRect.Left + Ceil(FHandleSize*2);
  HandleRect.Top := Floor(Y - FHandleSize);
  HandleRect.Bottom := HandleRect.Top + Ceil(FHandleSize*2);
  UpdateRect(HandleRect);
end;

procedure TRubberbandLayer.Update;
var
  R: TRect;
begin
  // Since the handles are partially outside the layer rect we need to
  // invalidate the area covered by those.
  // We could just inflate the rect being invalidated by the size of the handles
  //
  //   InflateRect(R, Ceil(FHandleSize), Ceil(FHandleSize));
  //   Update(R);
  //
  // ...but instead we go for the "slightly" more complex and correct solution
  // of only invalidating the area actually covered by the frame and the handles.

  R := MakeRect(GetAdjustedRect(FLocation));

  DoHandles(nil, R, UpdateFrame, UpdateHandle);
end;

procedure TRubberbandLayer.UpdateChildLayer;
begin
  if (FChildLayer <> nil) then
    FChildLayer.Location := Location;
end;

procedure TRubberbandLayer.SetFrameStippleCounter(const Value: TFloat);
begin
  if Value <> FFrameStippleCounter then
  begin
    FFrameStippleCounter := Value;
    // Repaint
    Update;
  end;
end;

procedure TRubberbandLayer.SetLayerOptions(Value: Cardinal);
begin
  Changing;
  FLayerOptions := Value and not LOB_NO_UPDATE; // workaround for changed behaviour
  Changed;
end;

procedure TRubberbandLayer.SetOptions(const Value: TRBOptions);
begin
  FOptions := Value;
end;

procedure TRubberbandLayer.SetQuantized(const Value: Integer);
begin
  if Value < 1 then
    raise Exception.Create('Value must be larger than zero!');

  FQuantized := Value;
end;

end.
