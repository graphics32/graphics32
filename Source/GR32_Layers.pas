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

//------------------------------------------------------------------------------
//
//      Layer option bit flags
//
//------------------------------------------------------------------------------
// Used by TCustomLayer.LayerOptions
//------------------------------------------------------------------------------
const
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
  TLayerClass = class of TCustomLayer;

  TLayerCollection = class;

//------------------------------------------------------------------------------
//
//      Layer event types
//
//------------------------------------------------------------------------------
  TLayerUpdateEvent = procedure(Sender: TObject; Layer: TCustomLayer) of object;
  TAreaUpdateEvent = TAreaChangedEvent;

  TLayerListNotification = (lnLayerAdded, lnLayerInserted, lnLayerDeleted, lnCleared);
  TLayerListNotifyEvent = procedure(Sender: TLayerCollection; Action: TLayerListNotification; Layer: TCustomLayer; Index: Integer) of object;

  TGetScaleEvent = procedure(Sender: TObject; out ScaleX, ScaleY: TFloat) of object;
  TGetShiftEvent = procedure(Sender: TObject; out ShiftX, ShiftY: TFloat) of object;


//------------------------------------------------------------------------------
//
//      Layer notification interfaces
//
//------------------------------------------------------------------------------
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


//------------------------------------------------------------------------------
//
//      TLayerCollection
//
//------------------------------------------------------------------------------
// A collection of layers.
//------------------------------------------------------------------------------
  TLayerCollection = class(TPersistent)
  strict private type
    TLayerList = TList<TCustomLayer>;
  strict private
    FItems: TLayerList;
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
  protected
    // Friend-methods; Used by TCustomLayer
    procedure InsertItem(Item: TCustomLayer);
    procedure ExtractItem(Item: TCustomLayer);
    procedure MoveItem(Item: TCustomLayer; NewIndex: Integer);
  protected
    procedure BeginUpdate; {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure EndUpdate; {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure BeginLockUpdate;
    procedure EndLockUpdate;
    procedure Changed; {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure Changing; {$IFDEF USEINLINING} inline; {$ENDIF}
    function  FindLayerAtPos(X, Y: Integer; OptionsMask: Cardinal): TCustomLayer;
    procedure GDIUpdate;
    procedure DoUpdateLayer(Layer: TCustomLayer);
    procedure DoUpdateArea(const Rect: TRect; const Info: Cardinal);
    procedure Notify(Action: TLayerListNotification; Layer: TCustomLayer; Index: Integer);
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): TCustomLayer;
    function MouseMove(Shift: TShiftState; X, Y: Integer): TCustomLayer;
    function MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): TCustomLayer;

    function GetCount: Integer;
    function GetItem(Index: Integer): TCustomLayer;
    function GetOwner: TPersistent; override;
    procedure SetItem(Index: Integer; Value: TCustomLayer);
    procedure SetMouseEvents(Value: Boolean);
    procedure SetMouseListener(Value: TCustomLayer);

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

    function GetEnumerator: TEnumerator<TCustomLayer>;

    procedure Subscribe(const ASubscriber: IInterface);
    procedure Unsubscribe(const ASubscriber: IInterface);

    function Add(ItemClass: TLayerClass): TCustomLayer; overload;
    function Insert(Index: Integer; ItemClass: TLayerClass): TCustomLayer; overload;
{$if defined(FPC) or (CompilerVersion > 29.0)} // Delphi 10 or later
    function Add<T: TCustomLayer>: T; overload;
    function Insert<T: TCustomLayer>(Index: Integer): T; overload;
{$ifend}
    procedure Delete(Index: Integer);
    procedure Clear;
    function IndexOf(Item: TCustomLayer): integer;

    procedure Assign(Source: TPersistent); override;

    // LocalToViewport: Convert from bitmap (local) coordinates to buffer/control (viewport) coordinates
    function LocalToViewport(const APoint: TFloatPoint; AScaled: Boolean): TFloatPoint; overload;
    function LocalToViewport(const APoint: TPoint; AScaled: Boolean): TFloatPoint; overload; // Needed because FPC lacks implicit TPoint<->TFloatPoint conversion
    // ViewportToLocal: Convert from buffer/control (viewport) coordinates to bitmap (local) coordinates
    function ViewportToLocal(const APoint: TFloatPoint; AScaled: Boolean): TFloatPoint; overload;
    function ViewportToLocal(const APoint: TPoint; AScaled: Boolean): TFloatPoint; overload; // Needed because FPC lacks implicit TPoint<->TFloatPoint conversion

    procedure GetViewportScale(out ScaleX, ScaleY: TFloat); virtual;
    procedure GetViewportShift(out ShiftX, ShiftY: TFloat); virtual;

    property Count: Integer read GetCount;
    property Owner: TPersistent read FOwner;
    property Items[Index: Integer]: TCustomLayer read GetItem write SetItem; default;
    property MouseListener: TCustomLayer read FMouseListener write SetMouseListener;
    property MouseEvents: Boolean read FMouseEvents write SetMouseEvents;
  end;

  TLayerCollectionClass = class of TLayerCollection;


//------------------------------------------------------------------------------
//
//      TCustomLayer
//
//------------------------------------------------------------------------------
// The layer base class.
//------------------------------------------------------------------------------
  TLayerState = (lsMouseLeft, lsMouseRight, lsMouseMiddle);
  TLayerStates = set of TLayerState;

  TPaintLayerEvent = procedure(Sender: TObject; Buffer: TBitmap32) of object;
  THitTestEvent = procedure(Sender: TObject; X, Y: Integer; var Passed: Boolean) of object;

  TCustomLayer = class(TNotifiablePersistent)
  strict private
    FCursor: TCursor;
    FFreeNotifies: TList<TCustomLayer>;
    FLayerCollection: TLayerCollection;
    FTag: NativeInt;
    FClicked: Boolean;
    FPendingForceUpdate: boolean;
    FOnHitTest: THitTestEvent;
    FOnKeyDown: TKeyEvent;
    FOnKeyUp: TKeyEvent;
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
    // Members that need friend access from TLayerCollection
    FLayerStates: TLayerStates;
  strict protected
    FLayerOptions: Cardinal;
  protected
    procedure AddNotification(ALayer: TCustomLayer); deprecated 'Use AddFreeNotification instead';
    procedure RemoveNotification(ALayer: TCustomLayer); deprecated 'Use RemoveFreeNotification instead';
    procedure Notification(ALayer: TCustomLayer); deprecated 'Use FreeNotification instead'; // No longer virtual; We want to force desecendant to use FreeNotification.
    procedure AddFreeNotification(ALayer: TCustomLayer);
    procedure RemoveFreeNotification(ALayer: TCustomLayer);
    procedure FreeNotification(ALayer: TCustomLayer); virtual;
  protected
    procedure Changing;
    procedure Click; virtual;
    procedure DblClick; virtual;
    function  DoHitTest(X, Y: Integer): Boolean; virtual;
    procedure DoPaint(Buffer: TBitmap32);
    function  GetOwner: TPersistent; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure KeyUp(var Key: Word; Shift: TShiftState); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseEnter; virtual;
    procedure MouseLeave; virtual;
    procedure Paint(Buffer: TBitmap32); virtual;
    procedure PaintGDI(Canvas: TCanvas); virtual;
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

    procedure Update; overload; virtual;

    function HitTest(X, Y: Integer): Boolean;
    procedure BringToFront;
    procedure SendToBack;
    procedure SetAsMouseListener;

    // LayerToControl: Convert from layer coordinates to viewport (buffer/control) coordinates.
    function LayerToControl(const p: TPoint): TPoint; overload; virtual;
    function LayerToControl(const r: TRect): TRect; overload; virtual;
    function LayerToControl(const p: TFloatPoint): TFloatPoint; overload; virtual;
    function LayerToControl(const r: TFloatRect): TFloatRect; overload; virtual;

    // ControlToLayer: Convert from viewport (buffer) coordinates to layer coordinates.
    function ControlToLayer(const p: TPoint): TPoint; overload; virtual;
    function ControlToLayer(const r: TRect): TRect; overload; virtual;
    function ControlToLayer(const p: TFloatPoint): TFloatPoint; overload; virtual;
    function ControlToLayer(const r: TFloatRect): TFloatRect; overload; virtual;

    // LayerToContent: Convert from layer to content coordinates, taking the layer's
    // internal content scaling into account.
    // Used, for example, with TBitmapLayers that must stretch their bitmap to fill
    // the layer.
    function LayerToContent(const APoint: TPoint): TPoint; overload; virtual;
    function LayerToContent(const APoint: TFloatPoint): TFloatPoint; overload; virtual;
    // ContentToLayer: Convert from content to layer coordinates, taking the layer's
    // internal content scaling into account.
    function ContentToLayer(const APoint: TPoint): TPoint; overload; virtual;
    function ContentToLayer(const APoint: TFloatPoint): TFloatPoint; overload; virtual;

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
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyUp: TKeyEvent read FOnKeyUp write FOnKeyUp;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
  end;


//------------------------------------------------------------------------------
//
//      TPositionedLayer
//
//------------------------------------------------------------------------------
// Base class for layers that have position and size.
//------------------------------------------------------------------------------
type
  TLayerGetUpdateRectEvent = procedure(Sender: TObject; var UpdateRect: TRect) of object;

  TPositionedLayer = class(TCustomLayer)
  strict private
    FLocation: TFloatRect;
    FScaled: Boolean;
    FOnGetUpdateRect: TLayerGetUpdateRectEvent;
    procedure SetLocation(const Value: TFloatRect);
  protected
    function DoHitTest(X, Y: Integer): Boolean; override;
    procedure DoSetLocation(const NewLocation: TFloatRect); virtual;
    function GetScaled: Boolean; virtual;
    procedure SetScaled(Value: Boolean); virtual;
    function DoGetUpdateRect: TRect; virtual;
    // GetUpdateRect: Returns the area covered by the layer in viewport (buffer/control) coordinates.
    // By default returns the same area as GetAdjustedLocation. Result can be customized
    // via the OnGetUpdateRect event or by overriding the DoGetUpdateRect method.
    function GetUpdateRect: TRect;
    // GetContentSize: Size of layer content (e.g. the bitmap if is has one).
    // Used by LayerToContent and ContentToLayer to translate between layer and content
    // coordinates.
    // Returns (0, 0) if the layer does not perform content scaling.
    function GetContentSize: TPoint; virtual;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;

    procedure Update; override;

    function LayerToControl(const APoint: TPoint): TPoint; overload; override;
    function LayerToControl(const ARect: TRect): TRect; overload; override;
    function LayerToControl(const APoint: TFloatPoint): TFloatPoint; overload; override;
    function LayerToControl(const ARect: TFloatRect): TFloatRect; overload; override;

    function ControlToLayer(const APoint: TPoint): TPoint; overload; override;
    function ControlToLayer(const ARect: TRect): TRect; overload; override;
    function ControlToLayer(const APoint: TFloatPoint): TFloatPoint; overload; override;
    function ControlToLayer(const ARect: TFloatRect): TFloatRect; overload; override;

    function LayerToContent(const APoint: TPoint): TPoint; overload; override;
    function LayerToContent(const APoint: TFloatPoint): TFloatPoint; overload; override;
    function ContentToLayer(const APoint: TPoint): TPoint; overload; override;
    function ContentToLayer(const APoint: TFloatPoint): TFloatPoint; overload; override;

    // GetAdjustedRect: Convert from bitmap coordinates to viewport (buffer/control) coordinates.
    function GetAdjustedRect(const R: TFloatRect): TFloatRect; virtual;
    // GetAdjustedLocation: Return the layer's location in viewport (buffer/control) coordinates.
    function GetAdjustedLocation: TFloatRect;

    // Location: The layer's position & size.
    // If Scaled=True, the coordinates are relative to the bitmap, in bitmap coordinates.
    // If Scaled=False, the coordinates are relative to the control/viewport, in control coordinates.
    property Location: TFloatRect read FLocation write SetLocation;
    property Scaled: Boolean read GetScaled write SetScaled;

    property OnGetUpdateRect: TLayerGetUpdateRectEvent read FOnGetUpdateRect write FOnGetUpdateRect;
  end;


//------------------------------------------------------------------------------
//
//      TCustomIndirectBitmapLayer
//
//------------------------------------------------------------------------------
// Base class for layers referencing a bitmap. The layer does not own the bitmap.
//------------------------------------------------------------------------------
type
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


//------------------------------------------------------------------------------
//
//      TCustomBitmapLayer
//
//------------------------------------------------------------------------------
// Abstract base class for layers containing a bitmap. The layer owns the bitmap.
//------------------------------------------------------------------------------
type
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


//------------------------------------------------------------------------------
//
//      TBitmapLayer
//
//------------------------------------------------------------------------------
// A layer containing a TBitmap32. The layer owns the bitmap.
//------------------------------------------------------------------------------
type
  TBitmapLayer = class(TCustomBitmapLayer)
  protected
    function GetBitmapClass: TCustomBitmap32Class; override;
    function GetBitmap: TBitmap32;
    procedure SetBitmap(Value: TBitmap32); reintroduce;
  public
    property Bitmap: TBitmap32 read GetBitmap write SetBitmap;
  end;


//------------------------------------------------------------------------------
//
//      TCustomRubberBandLayer
//
//------------------------------------------------------------------------------
// Base class for design layers displaying a stippled polygon with optional
// selection handles at the vertices.
//------------------------------------------------------------------------------
type
  TCustomRubberBandLayer = class;

  TRubberbandPassMouse = class(TPersistent)
  strict private
    FOwner: TCustomRubberBandLayer;
    FEnabled: Boolean;
    FToChild: Boolean;
    FLayerUnderCursor: Boolean;
    FCancelIfPassed: Boolean;
  protected
    function GetChildUnderCursor(X, Y: Integer; Exclude: TPositionedLayer = nil): TPositionedLayer;
  public
    constructor Create(AOwner: TCustomRubberBandLayer);

    property Enabled: Boolean read FEnabled write FEnabled default False;
    property ToChild: Boolean read FToChild write FToChild default False;
    property ToLayerUnderCursor: Boolean read FLayerUnderCursor write FLayerUnderCursor default False;
    property CancelIfPassed: Boolean read FCancelIfPassed write FCancelIfPassed default False;
  end;

  ILayerHitTest = interface
    ['{5F458999-F3BE-47F1-9215-B496927D7BA9}']
    // Layer position/size when the context was created
    function GetStartLocation: TFloatRect;
    procedure SetStartLocation(const Value: TFloatRect);
    property StartLocation: TFloatRect read GetStartLocation write SetStartLocation;

    // Mouse position when context was created
    function GetStartPosition: TPoint;
    property StartPosition: TPoint read GetStartPosition;

    // Current mouse position
    procedure SetCurrentPosition(const Value: TPoint);
    function GetCurrentPosition: TPoint;
    property CurrentPosition: TPoint read GetCurrentPosition write SetCurrentPosition;

    // Current shift state
    function GetShift: TShiftState;
    procedure SetShift(Value: TShiftState);
    property Shift: TShiftState read GetShift write SetShift;

    // Cursor corresponding to current position and shift state
    function GetCursor: integer;
    procedure SetCursor(Value: integer);
    property Cursor: integer read GetCursor write SetCursor;
  end;

  ILayerHitTestVertex = interface(ILayerHitTest)
    ['{6BFC44FB-02FA-4999-BBCD-1085FC81F9DC}']
    // The index of the vertex being dragged
    function GetVertex: integer;
    procedure SetVertex(Value: integer);
    property Vertex: integer read GetVertex write SetVertex;

    // The initial value of the vertex being dragged
    function GetStartValue: TFloatPoint;
    procedure SetStartValue(const Value: TFloatPoint);
    property StartValue: TFloatPoint read GetStartValue write SetStartValue;
  end;

  ILayerHitTestMove = interface(ILayerHitTest)
    ['{3CA95766-7294-42FB-A5F6-85153376F0B4}']
  end;

  TRubberBandHandleStyle = (hsSquare, hsCircle, hsDiamond);

  TRubberBandHandleDrawParams = record
    HandleStyle: TRubberBandHandleStyle;
    HandleSize: TFloat;
    HandleFill: TColor32;
    HandleFrame: TColor32;
    HandleFrameSize: TFloat;
  end;

  TRubberBandHandleEvent = procedure(Sender: TCustomRubberBandLayer; AIndex: integer) of object;
  TRubberBandHandleMoveEvent = procedure(Sender: TCustomRubberBandLayer; AIndex: integer; var APos: TFloatPoint) of object;
  TRubberBandPaintHandleEvent = procedure(Sender: TCustomRubberBandLayer; Buffer: TBitmap32; const p: TFloatPoint; AIndex: integer; var ADrawParams: TRubberBandHandleDrawParams; var Handled: boolean) of object;
  TRubberBandUpdateHandleEvent = procedure(Sender: TCustomRubberBandLayer; Buffer: TBitmap32; const p: TFloatPoint; AIndex: integer; var UpdateRect: TRect; var Handled: boolean) of object;

  TLayerShiftState = TShiftState; // Actually only [ssShift, ssAlt, ssCtrl] but we can't subtype because of the way TShiftState is declared.

  TCustomRubberBandLayer = class(TPositionedLayer)
  strict protected type
    // TODO : Replace these with anonymous methods once FPC catches up (expected for FPC 4)
    TRubberBandPaintFrameHandler = procedure(Buffer: TBitmap32; const r: TRect) of object;
    TRubberBandPaintHandleHandler = procedure(Buffer: TBitmap32; const r: TRect; Index: integer) of object;
    TRubberBandPaintHandlesHandler = procedure(Buffer: TBitmap32; const r: TRect; var Handled: boolean) of object;
  strict private
    FChildLayer: TPositionedLayer;
    FVertices: TArrayOfFloatPoint;
    FFrameStipplePattern: TArrayOfColor32;
    FFrameStippleStep: TFloat;
    FFrameStippleCounter: TFloat;
    FHandleFrame: TColor32;
    FHandleFill: TColor32;
    FHandleSize: TFloat;
    FHandleHitZone: TFloat;
    FHandleFrameSize: TFloat;
    FHandleStyle: TRubberBandHandleStyle;
    FOnUserChange: TNotifyEvent;
    FOnHandleClicked: TRubberBandHandleEvent;
    FOnHandleMove: TRubberBandHandleMoveEvent;
    FOnHandleMoved: TRubberBandHandleEvent;
    FOnPaintHandle: TRubberBandPaintHandleEvent;
    FOnUpdateHandle: TRubberBandUpdateHandleEvent;
    FQuantized: Integer;
    FQuantizeShiftToggle: TLayerShiftState;
    FPassMouse: TRubberbandPassMouse;
    FHitTest: ILayerHitTest;
    procedure SetFrameStipple(const Value: TArrayOfColor32);
    procedure SetFrameStippleStep(const Value: TFloat);
    procedure SetFrameStippleCounter(const Value: TFloat);
    procedure SetChildLayer(Value: TPositionedLayer);
    procedure SetHandleStyle(const Value: TRubberBandHandleStyle);
    procedure SetHandleSize(Value: TFloat);
    procedure SetHandleHitZone(const Value: TFloat);
    procedure SetHandleFill(Value: TColor32);
    procedure SetHandleFrame(Value: TColor32);
    procedure SetHandleFrameSize(Value: TFloat);
    procedure SetQuantized(const Value: Integer);
    procedure SetVertices(const Value: TArrayOfFloatPoint);
    procedure SetVertex(Index: integer; const Value: TFloatPoint);
    function GetVertex(Index: integer): TFloatPoint;
  protected
    FIsDragging: Boolean; // For backward compatibility. Equals (ActiveHitTest <> nil)
    function  DoHitTest(X, Y: Integer): Boolean; override;
    procedure DoSetLocation(const NewLocation: TFloatRect); override;
    function GetScaled: Boolean; override; //TODO : We need to be notified+repainted when child.Scaled/Location changes
    procedure SetScaled(Value: Boolean); override;
    function FindVertex(const APosition: TPoint): integer; virtual;
    function GetHitTest(const APosition: TPoint; AShift: TShiftState = []): ILayerHitTest; virtual;
    procedure SetHitTest(const AHitTest: ILayerHitTest); virtual;
    procedure ApplyHitTestCursor(const AHitTest: ILayerHitTest); virtual;
    function GetHitTestCursor(const AHitTest: ILayerHitTest): TCursor; virtual;
    procedure DoHandleClicked(VertexIndex: integer); virtual;
    procedure DoHandleMove(VertexIndex: integer; var APos: TFloatPoint); virtual;
    procedure DoHandleMoved(VertexIndex: integer); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure FreeNotification(ALayer: TCustomLayer); override;
    procedure Paint(Buffer: TBitmap32); override;
    procedure SetLayerOptions(Value: Cardinal); override;
    procedure UpdateChildLayer; virtual;
    function IsFrameVisible: boolean; virtual;
    function IsVertexVisible(VertexIndex: integer): boolean; virtual;
    function AllowMove: boolean; virtual;
    procedure DrawHandle(Buffer: TBitmap32; const p: TFloatPoint; AIndex: integer; const DrawParams: TRubberBandHandleDrawParams); virtual;
    procedure DoDrawVertex(Buffer: TBitmap32; const R: TRect; VertexIndex: integer); virtual;
    procedure DoDrawVertices(Buffer: TBitmap32; const R: TRect; var Handled: boolean); virtual;
    procedure DrawFrame(Buffer: TBitmap32; const R: TRect); virtual;
    procedure DoUpdateVertex(Buffer: TBitmap32; const R: TRect; VertexIndex: integer); virtual;
    procedure DoUpdateVertices(Buffer: TBitmap32; const R: TRect; var Handled: boolean); virtual;
    procedure DoUpdateFrame(Buffer: TBitmap32; const R: TRect); virtual;
    procedure DoDrawUpdate(Buffer: TBitmap32; FrameHandler: TRubberBandPaintFrameHandler;
      VerticesHandler: TRubberBandPaintHandlesHandler; VertexHandler: TRubberBandPaintHandleHandler);
    procedure UpdateFrame;
    procedure UpdateVertices;
    function ApplyOffset(const AHitTest: ILayerHitTest; AQuantize: boolean): boolean; virtual;
    function CanQuantize: boolean; virtual;
    function ShouldQuantize(const AHitTest: ILayerHitTest): boolean; virtual;

    property Vertices: TArrayOfFloatPoint read FVertices write SetVertices;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;

    procedure Update; override;

    procedure Quantize;

    property ChildLayer: TPositionedLayer read FChildLayer write SetChildLayer;
    property Vertex[Index: integer]: TFloatPoint read GetVertex write SetVertex;

    property HandleStyle: TRubberBandHandleStyle read FHandleStyle write SetHandleStyle;
    // HandleSize: Radius of handle
    property HandleSize: TFloat read FHandleSize write SetHandleSize;
    // HandleHitZone: Width of extra "invisible" area around handle where the handle can be clicked
    property HandleHitZone: TFloat read FHandleHitZone write SetHandleHitZone;
    // HandleFill: Handle fill color
    property HandleFill: TColor32 read FHandleFill write SetHandleFill;
    // HandleFrame: Handle frame/outline color
    property HandleFrame: TColor32 read FHandleFrame write SetHandleFrame;
    // HandleFrameSize: Width of handle frame/outline
    property HandleFrameSize: TFloat read FHandleFrameSize write SetHandleFrameSize;

    property FrameStipple: TArrayOfColor32 read FFrameStipplePattern write SetFrameStipple;
    property FrameStippleStep: TFloat read FFrameStippleStep write SetFrameStippleStep;
    property FrameStippleCounter: TFloat read FFrameStippleCounter write SetFrameStippleCounter;

    property Quantized: Integer read FQuantized write SetQuantized default 1;
    property QuantizeShiftToggle: TLayerShiftState read FQuantizeShiftToggle write FQuantizeShiftToggle default [ssAlt];

    property PassMouseToChild: TRubberbandPassMouse read FPassMouse;

    property ActiveHitTest: ILayerHitTest read FHitTest;

    property OnUserChange: TNotifyEvent read FOnUserChange write FOnUserChange;
    property OnHandleClicked: TRubberBandHandleEvent read FOnHandleClicked write FOnHandleClicked;
    property OnHandleMove: TRubberBandHandleMoveEvent read FOnHandleMove write FOnHandleMove;
    property OnHandleMoved: TRubberBandHandleEvent read FOnHandleMoved write FOnHandleMoved;
    property OnPaintHandle: TRubberBandPaintHandleEvent read FOnPaintHandle write FOnPaintHandle;
    property OnUpdateHandle: TRubberBandUpdateHandleEvent read FOnUpdateHandle write FOnUpdateHandle;
  end;

type
  // Compas directions, counter clockwise, from 0 degress to 360.
  // Each one direction covers 45 degrees.
  // Used inside TCustomRubberBandLayer.GetCursor instead of the poorly ordered TRBDragState enum.
  TResizeDirection = (ResizeDirectionE, ResizeDirectionNE, ResizeDirectionN, ResizeDirectionNW,
    ResizeDirectionW, ResizeDirectionSW, ResizeDirectionS, ResizeDirectionSE);

var
  // The TCustomRubberBandLayer resize handle cursors.
  // These are the values returned by TCustomRubberBandLayer.GetCursor
  DirectionCursors: array[TResizeDirection] of TCursor = (crSizeWE, crSizeNESW, crSizeNS, crSizeNWSE, crSizeWE, crSizeNESW, crSizeNS, crSizeNWSE);

type
  TPolygonRubberbandLayer = class(TCustomRubberBandLayer)
  public
    property Vertices;
  end;


//------------------------------------------------------------------------------
//
//      TRubberbandLayer
//
//------------------------------------------------------------------------------
// Rectangular rubber band selection design layer.
//------------------------------------------------------------------------------
type
  TRBDragState = (dsNone, dsMove, dsSizeL, dsSizeT, dsSizeR, dsSizeB, dsSizeTL, dsSizeTR, dsSizeBL, dsSizeBR);

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

const
  VertexToDragState: array[0..7] of TRBDragState =
    // 0         1        2
    // 7                  3
    // 6         5        4
    (dsSizeTL, dsSizeT, dsSizeTR, dsSizeR, dsSizeBR, dsSizeB, dsSizeBL, dsSizeL);

  DragStateToVertex: array[TRBDragState] of integer = (-1, -1, 7, 1, 3, 5, 0, 2, 6, 4);

type
  TValidDragStates = set of TRBDragState;

  TRubberbandLayer = class(TCustomRubberBandLayer)
  strict private
    FHandles: TRBHandles;
    FOptions: TRBOptions;
    FMinWidth: TFloat;
    FMaxHeight: TFloat;
    FMinHeight: TFloat;
    FMaxWidth: TFloat;
    FOnResizing: TRBResizingEvent;
    FOnConstrain: TRBConstrainEvent;
  protected
    FDragState: TRBDragState;
    FValidDragStates: TValidDragStates;
  protected
    procedure SetHandles(Value: TRBHandles);
    procedure SetOptions(const Value: TRBOptions);
    function GetValidDragStates: TValidDragStates;
    function CanQuantize: boolean; override;
    procedure DoSetLocation(const NewLocation: TFloatRect); override;
    function GetHitTest(const APosition: TPoint; AShift: TShiftState = []): ILayerHitTest; override;
    function GetHitTestCursor(const AHitTest: ILayerHitTest): TCursor; override;
    function IsFrameVisible: boolean; override;
    function IsVertexVisible(VertexIndex: integer): boolean; override;
    function AllowMove: boolean; override;
    procedure DrawFrame(Buffer: TBitmap32; const R: TRect); override;
    procedure DoUpdateFrame(Buffer: TBitmap32; const R: TRect); override;
    function ApplyOffset(const AHitTest: ILayerHitTest; AQuantize: boolean): boolean; override;
    procedure DoResizing(const OldLocation: TFloatRect; var NewLocation: TFloatRect; DragState: TRBDragState; Shift: TShiftState); virtual;
    procedure DoConstrain(const OldLocation: TFloatRect; var NewLocation: TFloatRect; DragState: TRBDragState; Shift: TShiftState); virtual;
    // Backward compatibility
    function GetDragState(X, Y: Integer): TRBDragState; overload; virtual; deprecated 'Use GetHitTest';
    procedure DoSetDragState(const Value: TRBDragState; const X, Y: Integer); overload;
    procedure SetDragState(const Value: TRBDragState); overload; deprecated 'Use SetHitTest';
    procedure SetDragState(const Value: TRBDragState; const X, Y: Integer); overload; deprecated 'Use SetHitTest';
    function GetHandleCursor(DragState: TRBDragState; Angle: integer): TCursor; virtual; // Deprecated
  public
    constructor Create(ALayerCollection: TLayerCollection); override;

    property Options: TRBOptions read FOptions write SetOptions;
    property Handles: TRBHandles read FHandles write SetHandles;
    property MaxHeight: TFloat read FMaxHeight write FMaxHeight;
    property MaxWidth: TFloat read FMaxWidth write FMaxWidth;
    property MinHeight: TFloat read FMinHeight write FMinHeight;
    property MinWidth: TFloat read FMinWidth write FMinWidth;
    property OnConstrain: TRBConstrainEvent read FOnConstrain write FOnConstrain;
    property OnResizing: TRBResizingEvent read FOnResizing write FOnResizing;
  end;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  TypInfo,
  Types,
  GR32_Image,
  GR32_LowLevel,
  GR32_Math,
  GR32_Geometry,
  GR32_VectorUtils,
  GR32_Polygons,
  GR32_Resamplers,
  GR32_RepaintOpt;

{ mouse state mapping }
const
  CStateMap: array [TMouseButton] of TLayerState =
    (lsMouseLeft, lsMouseRight, lsMouseMiddle
    {$IFDEF FPC}, lsMouseMiddle, lsMouseMiddle{$ENDIF});

type
  TImage32Access = class(TCustomImage32);


//------------------------------------------------------------------------------
//
//      TLayerCollection
//
//------------------------------------------------------------------------------
constructor TLayerCollection.Create(AOwner: TPersistent);
begin
  inherited Create;

  FOwner := AOwner;
  FItems := TObjectList<TCustomLayer>.Create;
  FMouseEvents := True;
end;

destructor TLayerCollection.Destroy;
begin
  FUpdateCount := 1; // disable update notification
  Clear;
  FItems.Free;
  FSubscribers.Free;
  inherited;
end;

function TLayerCollection.Add(ItemClass: TLayerClass): TCustomLayer;
begin
  Result := ItemClass.Create(Self);
  Assert(Result.LayerCollection = Self);

  Result.Index := FItems.Count - 1;

  Notify(lnLayerAdded, Result, Result.Index);
end;

{$if defined(FPC) or (CompilerVersion > 29.0)}
function TLayerCollection.Add<T>: T;
begin
  Result := T(Add(T));
end;
{$ifend}

procedure TLayerCollection.Assign(Source: TPersistent);
var
  I: Integer;
  Item: TCustomLayer;
begin
  if Source is TLayerCollection then
  begin
    BeginUpdate;
    try
      FItems.Clear;

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
var
  Item: TCustomLayer;
begin
  BeginUpdate;
  try
    for Item in FItems.ToArray do // ToArray for stability
      Item.Visible := False;

    FItems.Clear;

    Notify(lnCleared, nil, 0);
    Changed;
  finally
    EndUpdate;
  end;
end;

procedure TLayerCollection.Delete(Index: Integer);
begin
  // Hide layer so the area covered by it will be invalidated
  FItems[Index].Visible := False;
  FItems.Delete(Index);
end;

function TLayerCollection.FindLayerAtPos(X, Y: Integer; OptionsMask: Cardinal): TCustomLayer;
var
  i: Integer;
begin
  for i := FItems.Count-1 downto 0 do
  begin
    Result := Items[i];

    if (Result.LayerOptions and OptionsMask) = 0 then
      Continue; // skip to the next one

    if Result.HitTest(X, Y) then
      Exit;
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

function TLayerCollection.GetEnumerator: TEnumerator<TCustomLayer>;
begin
  Result := FItems.GetEnumerator;
end;

function TLayerCollection.GetItem(Index: Integer): TCustomLayer;
begin
  Result := FItems[Index];
end;

function TLayerCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TLayerCollection.IndexOf(Item: TCustomLayer): integer;
begin
  Result := FItems.IndexOf(Item);
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

{$if defined(FPC) or (CompilerVersion > 29.0)}
function TLayerCollection.Insert<T>(Index: Integer): T;
begin
  Result := T(Insert(Index, T));
end;
{$ifend}

procedure TLayerCollection.InsertItem(Item: TCustomLayer);
var
  Index: Integer;
begin
  // We are called from TCustomLayer.SetLayerCollection which should have already
  // set its LayerCollection
  Assert(Item.LayerCollection = Self);

  BeginUpdate;
  try
    Index := FItems.Add(Item);
    Notify(lnLayerAdded, Item, Index);
    Changed;
  finally
    EndUpdate;
  end;
end;

procedure TLayerCollection.ExtractItem(Item: TCustomLayer);
var
  Index: Integer;
begin
  Index := FItems.IndexOf(Item);
  if (Index = -1) then
    exit;

  // We are called from TCustomLayer.SetLayerCollection which should have already
  // nilled its LayerCollection
  Assert(Item.LayerCollection = nil);

  BeginUpdate;
  try
    FItems.Extract(Item);
    Notify(lnLayerDeleted, Item, Index);
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
  end else
    Result := APoint;
end;

function TLayerCollection.LocalToViewport(const APoint: TPoint; AScaled: Boolean): TFloatPoint;
begin
  Result := LocalToViewport(FloatPoint(APoint), AScaled);
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
  end else
    Result := APoint;
end;

function TLayerCollection.ViewportToLocal(const APoint: TPoint; AScaled: Boolean): TFloatPoint;
begin
  Result := ViewportToLocal(FloatPoint(APoint), AScaled);
end;

function TLayerCollection.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): TCustomLayer;
begin
  if (MouseListener <> nil) then
    Result := MouseListener
  else
    Result := FindLayerAtPos(X, Y, LOB_MOUSE_EVENTS);

  if (Result <> MouseListener) and ((Result = nil) or (Result.LayerOptions and LOB_NO_CAPTURE = 0)) then
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

  if (Result = nil) then
    Result := FindLayerAtPos(X, Y, LOB_MOUSE_EVENTS);

  if (Result <> nil) then
    Result.MouseMove(Shift, X, Y);
end;

function TLayerCollection.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): TCustomLayer;
begin
  Result := MouseListener;

  if (Result = nil) then
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

procedure TLayerCollection.MoveItem(Item: TCustomLayer; NewIndex: Integer);
var
  CurrentIndex: integer;
begin
  if NewIndex < 0 then
    NewIndex := 0;
  if NewIndex >= Count then
    NewIndex := Count-1;

  CurrentIndex := Item.Index;
  if (CurrentIndex = NewIndex) then
    exit;

  BeginUpdate;
  try
    FItems.Move(CurrentIndex, NewIndex);

    if Item.Visible then
      Changed;
  finally
    EndUpdate;
  end;
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

procedure TLayerCollection.SetItem(Index: Integer; Value: TCustomLayer);
begin
  FItems[Index].Assign(Value);
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


//------------------------------------------------------------------------------
//
//      TCustomLayer
//
//------------------------------------------------------------------------------
constructor TCustomLayer.Create(ALayerCollection: TLayerCollection);
begin
  LayerCollection := ALayerCollection;
  FLayerOptions := LOB_VISIBLE;
end;

destructor TCustomLayer.Destroy;
var
  Subscriber: TCustomLayer;
begin
  if (FFreeNotifies <> nil) then
  begin
    for Subscriber in FFreeNotifies.ToArray do // ToArray for stability while items are removed from the list
      Subscriber.FreeNotification(Self);

    // List might have been freed while we looped but Free can handle that
    FFreeNotifies.Free;
    FFreeNotifies := nil;
  end;

  SetLayerCollection(nil);

  inherited;
end;

//------------------------------------------------------------------------------

procedure TCustomLayer.BeforeDestruction;
begin
  if Assigned(FOnDestroy) then
    FOnDestroy(Self);

  inherited;
end;

//------------------------------------------------------------------------------

procedure TCustomLayer.AddFreeNotification(ALayer: TCustomLayer);
begin
  if (FFreeNotifies = nil) then
    FFreeNotifies := TList<TCustomLayer>.Create;

  if not FFreeNotifies.Contains(ALayer) then
    FFreeNotifies.Add(ALayer);
end;

procedure TCustomLayer.RemoveFreeNotification(ALayer: TCustomLayer);
begin
  if (FFreeNotifies = nil) then
    exit;

  FFreeNotifies.Remove(ALayer);

  if FFreeNotifies.Count = 0 then
  begin
    FFreeNotifies.Free;
    FFreeNotifies := nil;
  end;
end;

procedure TCustomLayer.FreeNotification(ALayer: TCustomLayer);
begin
  // do nothing by default
end;

procedure TCustomLayer.AddNotification(ALayer: TCustomLayer);
begin
  AddFreeNotification(ALayer);
end;

procedure TCustomLayer.RemoveNotification(ALayer: TCustomLayer);
begin
  RemoveFreeNotification(ALayer);
end;

//------------------------------------------------------------------------------

procedure TCustomLayer.Notification(ALayer: TCustomLayer);
begin
end;

//------------------------------------------------------------------------------

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

    // We use FPendingForceUpdate to handle the situation where
    // ForceUpdate is set during a batched update and thus suprepressed.
    // When FPendingForceUpdate=True then ForceUpdate will return True
    // until FPendingForceUpdate has been reset to False - which is what
    // we do here once the updates have been processed.
    FPendingForceUpdate := False;

    inherited;
  end;
end;

procedure TCustomLayer.Changed(const Rect: TRect; const Info: Cardinal);
begin
  if (UpdateCount > 0) then
  begin
    Changed; // Ensure modified flag is set
    Exit;
  end;

  if (FLayerCollection <> nil) and (FLayerOptions and LOB_NO_UPDATE = 0) then
  begin
    AreaUpdated(Rect, Info);
    inherited DoChanged;
  end;
end;

procedure TCustomLayer.Changing;
begin
  if (LockUpdateCount > 0) then
    Exit;

  if (UpdateCount > 0) then
    Exit;

  if Visible and (FLayerCollection <> nil) and (FLayerOptions and LOB_NO_UPDATE = 0) then
    FLayerCollection.Changing;
end;

//------------------------------------------------------------------------------

procedure TCustomLayer.BringToFront;
begin
  Index := LayerCollection.Count;
end;

procedure TCustomLayer.SendToBack;
begin
  Index := 0;
end;

//------------------------------------------------------------------------------

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

procedure TCustomLayer.MouseEnter;
begin
end;

procedure TCustomLayer.MouseLeave;
begin
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

//------------------------------------------------------------------------------

procedure TCustomLayer.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Assigned(FOnKeyDown)) then
    FOnKeyDown(Self, Key, Shift);
end;

procedure TCustomLayer.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if (Assigned(FOnKeyUp)) then
    FOnKeyUp(Self, Key, Shift);
end;

//------------------------------------------------------------------------------

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
    Result := FLayerCollection.IndexOf(Self)
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

function TCustomLayer.ControlToLayer(const p: TPoint): TPoint;
begin
  Result := p;
end;

function TCustomLayer.ControlToLayer(const r: TRect): TRect;
begin
  Result := r;
end;

function TCustomLayer.ControlToLayer(const r: TFloatRect): TFloatRect;
begin
  Result := r;
end;

function TCustomLayer.ControlToLayer(const p: TFloatPoint): TFloatPoint;
begin
  Result := p;
end;

//------------------------------------------------------------------------------

function TCustomLayer.LayerToControl(const r: TRect): TRect;
begin
  Result := r;
end;

function TCustomLayer.LayerToControl(const p: TPoint): TPoint;
begin
  Result := p;
end;

function TCustomLayer.LayerToControl(const p: TFloatPoint): TFloatPoint;
begin
  Result := p;
end;

function TCustomLayer.LayerToControl(const r: TFloatRect): TFloatRect;
begin
  Result := r;
end;

//------------------------------------------------------------------------------

function TCustomLayer.ContentToLayer(const APoint: TPoint): TPoint;
begin
  Result := APoint;
  end;

function TCustomLayer.ContentToLayer(const APoint: TFloatPoint): TFloatPoint;
begin
  Result := APoint;
end;

function TCustomLayer.LayerToContent(const APoint: TPoint): TPoint;
begin
  Result := APoint;
end;

function TCustomLayer.LayerToContent(const APoint: TFloatPoint): TFloatPoint;
begin
  Result := APoint;
end;

//------------------------------------------------------------------------------

procedure TCustomLayer.Paint(Buffer: TBitmap32);
begin
  // descendants override this method
end;

procedure TCustomLayer.PaintGDI(Canvas: TCanvas);
begin
  // descendants override this method
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
begin
  if (FLayerCollection = nil) then
    exit;

  FLayerCollection.MoveItem(Self, Value);
end;

procedure TCustomLayer.SetLayerCollection(Value: TLayerCollection);
var
  OldLayerCollection: TLayerCollection;
begin
  if (FLayerCollection = Value) then
    exit;

  OldLayerCollection := FLayerCollection;
  FLayerCollection := nil; // Prevent recursion

  if (OldLayerCollection <> nil) then
  begin
    if OldLayerCollection.MouseListener = Self then
      OldLayerCollection.MouseListener := nil;

    OldLayerCollection.ExtractItem(Self);
  end;

  FLayerCollection := Value;

  if (FLayerCollection <> nil) then
    FLayerCollection.InsertItem(Self);
end;

procedure TCustomLayer.SetLayerOptions(Value: Cardinal);
var
  LayerHiding: boolean;
begin
  if (FLayerOptions = Value) then
    exit;

  LayerHiding := (FLayerOptions and LOB_VISIBLE <> 0) and (Value and LOB_VISIBLE = 0);
  if (LayerHiding) then
    ForceUpdate := True;

    Changing;
    FLayerOptions := Value;
    Changed;

  if (LayerHiding) then
    ForceUpdate := False;
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
    LayerOptions := LayerOptions and not LOB_VISIBLE;
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
  Result := (LayerOptions and LOB_FORCE_UPDATE <> 0) or FPendingForceUpdate;
end;

procedure TCustomLayer.SetForceUpdate(Value: Boolean);
begin
  // don't use LayerOptions here since this is internal and we don't want to
  // trigger Changing and Changed as this will definitely cause a stack overflow.
  if Value then
  begin
    FLayerOptions := FLayerOptions or LOB_FORCE_UPDATE;
    FPendingForceUpdate := True;
  end else
    FLayerOptions := FLayerOptions and not LOB_FORCE_UPDATE;
end;


//------------------------------------------------------------------------------
//
//      TPositionedLayer
//
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------

function TPositionedLayer.DoHitTest(X, Y: Integer): Boolean;
var
  r: TFLoatRect;
begin
  r := GetAdjustedRect(FLocation);
  Result := (X >= r.Left) and (X < r.Right) and (Y >= r.Top) and (Y < r.Bottom) and
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
  if Scaled and (LayerCollection <> nil) then
  begin
    LayerCollection.GetViewportScale(ScaleX, ScaleY);
    Result.Left := R.Left * ScaleX;
    Result.Top := R.Top * ScaleY;
    Result.Right := R.Right * ScaleX;
    Result.Bottom := R.Bottom * ScaleY;

    LayerCollection.GetViewportShift(ShiftX, ShiftY);
    Result.Offset(ShiftX, ShiftY);
  end else
    Result := R;
end;

//------------------------------------------------------------------------------

function TPositionedLayer.GetContentSize: TPoint;
begin
  Result.X := 0;
  Result.Y := 0;
end;

//------------------------------------------------------------------------------

function TPositionedLayer.ControlToLayer(const APoint: TPoint): TPoint;
begin
  Result := GR32.Point(ControlToLayer(FloatPoint(APoint)));
end;

function TPositionedLayer.ControlToLayer(const ARect: TRect): TRect;
begin
  Result := MakeRect(ControlToLayer(FloatRect(ARect)), rrOutside);
end;

function TPositionedLayer.ControlToLayer(const APoint: TFloatPoint): TFloatPoint;
var
  ScaleX, ScaleY, ShiftX, ShiftY: TFloat;
begin
  // Scaled=True: Coordinates must be scaled and translated
  // Scaled=False: Coordinates must be translated (layer has same scale as control; 1:1)

  if Scaled and (LayerCollection <> nil) then
  begin
    LayerCollection.GetViewportShift(ShiftX, ShiftY);
    LayerCollection.GetViewportScale(ScaleX, ScaleY);

    // Translate from control/buffer/viewport coordinates...
    // ...to bitmap coordinates...
    // ...and unscale...
    // ...and finally to layer coordinates
    Result.X := (APoint.X - ShiftX) / ScaleX - Location.Left;
    Result.Y := (APoint.Y - ShiftY) / ScaleY - Location.Top;

  end else
  begin
    Result.X := APoint.X - Location.Left;
    Result.Y := APoint.Y - Location.Top;
  end;
end;

function TPositionedLayer.ControlToLayer(const ARect: TFloatRect): TFloatRect;
var
  ScaleX, ScaleY, ShiftX, ShiftY: TFloat;
begin
  // Scaled=True: Coordinates must be scaled and translated
  // Scaled=False: Coordinates must be translated (layer has same scale as control; 1:1)

  if Scaled and (LayerCollection <> nil) then
  begin
    LayerCollection.GetViewportShift(ShiftX, ShiftY);
    LayerCollection.GetViewportScale(ScaleX, ScaleY);

    // Translate from control/buffer/viewport coordinates...
    // ...to bitmap coordinates...
    // ...and unscale...
    // ...and finally to layer coordinates
    Result.Left := (ARect.Left - ShiftX) / ScaleX - Location.Left;
    Result.Right := (ARect.Right - ShiftX) / ScaleX - Location.Left;
    Result.Top := (ARect.Top - ShiftY) / ScaleY - Location.Top;
    Result.Bottom := (ARect.Bottom - ShiftY) / ScaleY - Location.Top;

  end else
  begin
    Result.Left := ARect.Left - Location.Left;
    Result.Right := ARect.Right - Location.Left;
    Result.Top := ARect.Top - Location.Top;
    Result.Bottom := ARect.Bottom - Location.Top;
  end;
end;

//------------------------------------------------------------------------------

function TPositionedLayer.LayerToControl(const ARect: TRect): TRect;
begin
  Result := MakeRect(LayerToControl(FloatRect(ARect)), rrOutside);
end;

function TPositionedLayer.LayerToControl(const APoint: TPoint): TPoint;
begin
  Result := GR32.Point(LayerToControl(FloatPoint(APoint)));
end;

function TPositionedLayer.LayerToControl(const APoint: TFloatPoint): TFloatPoint;
var
  ScaleX, ScaleY, ShiftX, ShiftY: TFloat;
begin
  // Scaled=True: Coordinates must be scaled and translated
  // Scaled=False: Coordinates must be translated (layer has same scale as control; 1:1)

  if Scaled and (LayerCollection <> nil) then
  begin
    LayerCollection.GetViewportShift(ShiftX, ShiftY);
    LayerCollection.GetViewportScale(ScaleX, ScaleY);

    // Translate from layer coordinates to control/buffer/viewport
    Result.X := (APoint.X + Location.Left) * ScaleX + ShiftX;
    Result.Y := (APoint.Y + Location.Top) * ScaleY + ShiftY;

  end else
  begin
    Result.X := APoint.X + Location.Left;
    Result.Y := APoint.Y + Location.Top;
  end;
end;

function TPositionedLayer.LayerToControl(const ARect: TFloatRect): TFloatRect;
var
  ScaleX, ScaleY, ShiftX, ShiftY: TFloat;
begin
  // Scaled=True: Coordinates must be scaled and translated
  // Scaled=False: Coordinates must be translated (layer has same scale as control; 1:1)

  if Scaled and (LayerCollection <> nil) then
  begin
    LayerCollection.GetViewportShift(ShiftX, ShiftY);
    LayerCollection.GetViewportScale(ScaleX, ScaleY);

    // Translate from layer coordinates to control/buffer/viewport
    Result.Left := (ARect.Left + Location.Left) * ScaleX + ShiftX;
    Result.Right := (ARect.Right + Location.Left) * ScaleX + ShiftX;
    Result.Top := (ARect.Top + Location.Top) * ScaleY + ShiftY;
    Result.Bottom := (ARect.Bottom + Location.Top) * ScaleY + ShiftY;

  end else
  begin
    Result.Left := ARect.Left + Location.Left;
    Result.Right := ARect.Right + Location.Left;
    Result.Top := ARect.Top + Location.Top;
    Result.Bottom := ARect.Bottom + Location.Top;
  end;
end;

//------------------------------------------------------------------------------

function TPositionedLayer.ContentToLayer(const APoint: TPoint): TPoint;
    begin
  Result := GR32.Point(ContentToLayer(FloatPoint(APoint)));
end;

function TPositionedLayer.ContentToLayer(const APoint: TFloatPoint): TFloatPoint;
var
  Size: TPoint;
  LayerWidth, LayerHeight: TFloat;
begin
  Result := APoint;

      Size := GetContentSize;
  if (Size.IsZero) then
    Exit;

  LayerWidth := Location.Width;
  LayerHeight := Location.Height;

  if (LayerWidth > 0.5) and (LayerHeight > 0.5) and
        ((Size.X <> LayerWidth) or (Size.Y <> LayerHeight)) then
      begin
    Result.X := Result.X * LayerWidth / Size.X;
    Result.Y := Result.Y * LayerHeight / Size.Y;
      end;
    end;

function TPositionedLayer.LayerToContent(const APoint: TPoint): TPoint;
begin
  Result := GR32.Point(LayerToContent(FloatPoint(APoint)));
end;

function TPositionedLayer.LayerToContent(const APoint: TFloatPoint): TFloatPoint;
var
  Size: TPoint;
  LayerWidth, LayerHeight: TFloat;
  begin
  Result := APoint;

  Size := GetContentSize;
  if (Size.IsZero) then
    Exit;

  LayerWidth := Location.Width;
  LayerHeight := Location.Height;

  if (LayerWidth > 0.5) and (LayerHeight > 0.5) and
    ((Size.X <> LayerWidth) or (Size.Y <> LayerHeight)) then
  begin
    Result.X := Result.X * Size.X / LayerWidth;
    Result.Y := Result.Y * Size.Y / LayerHeight;
  end;
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
  if (LayerCollection <> nil) and (LayerOptions and LOB_NO_UPDATE = 0) then
    Update;

  DoSetLocation(Value);

  // Invalidate new location
  Changed;
end;

function TPositionedLayer.GetScaled: Boolean;
begin
  Result := FScaled;
end;

procedure TPositionedLayer.SetScaled(Value: Boolean);
begin
  if (Value = FScaled) then
    exit;

  // Changing Scaled can change size and position so treat it as if we did
  Changing;

  // Invalidate old location
  if (LayerCollection <> nil) and (LayerOptions and LOB_NO_UPDATE = 0) then
    Update;

  FScaled := Value;

  // Invalidate new location
  Changed;
end;

procedure TPositionedLayer.Update;
begin
  UpdateRect(GetUpdateRect);
end;


//------------------------------------------------------------------------------
//
//      TCustomIndirectBitmapLayer
//
//------------------------------------------------------------------------------
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

  if (LayerCollection = nil) or (LayerOptions and LOB_NO_UPDATE <> 0) then
    exit;

  // All the stuff below is in vain if updates are batched so bail early.
  if (UpdateCount > 0) then
    Exit;

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
    r := GetAdjustedLocation;

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

  DstRect := MakeRect(GetAdjustedLocation);
  ClipRect := Buffer.ClipRect;
  GR32.IntersectRect(TempRect, ClipRect, DstRect);
  if GR32.IsRectEmpty(TempRect) then
    Exit;

  SrcRect := MakeRect(0, 0, FBitmap.Width, FBitmap.Height);
  if Cropped and (LayerCollection.Owner is TCustomImage32) and
    not (TImage32Access(LayerCollection.Owner).PaintToMode) then
  begin
    if (DstRect.Width < 0.5) or (DstRect.Height < 0.5) then
      Exit;
    ImageRect := TCustomImage32(LayerCollection.Owner).GetBitmapRect;
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


//------------------------------------------------------------------------------
//
//      TCustomBitmapLayer
//
//------------------------------------------------------------------------------
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


//------------------------------------------------------------------------------
//
//      TBitmapLayer
//
//------------------------------------------------------------------------------
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


//------------------------------------------------------------------------------
// TRubberbandPassMouse
//------------------------------------------------------------------------------
constructor TRubberbandPassMouse.Create(AOwner: TCustomRubberBandLayer);
begin
  FOwner := AOwner;
  FEnabled := False;
  FToChild := False;
  FLayerUnderCursor := False;
  FCancelIfPassed := False;
end;

function TRubberbandPassMouse.GetChildUnderCursor(X, Y: Integer; Exclude: TPositionedLayer): TPositionedLayer;
var
  Layer: TCustomLayer;
  Index: Integer;
begin
  Result := nil;
  for Index := FOwner.LayerCollection.Count - 1 downto 0 do
  begin
    Layer := FOwner.LayerCollection[Index];
    if (Layer <> Exclude) and
      (Layer.LayerOptions and LOB_MOUSE_EVENTS <> 0) and
      (Layer is TPositionedLayer) and Layer.HitTest(X, Y) then
    begin
      Result := TPositionedLayer(Layer);
      Exit;
    end;
  end;
end;


//------------------------------------------------------------------------------
// ILayerHitTest and friends
//------------------------------------------------------------------------------
type
  TLayerHitTest = class(TInterfacedObject, ILayerHitTest)
  private
    FStartLocation: TFloatRect;
    FStartPosition: TPoint;
    FCurrentPosition: TPoint;
    FShift: TShiftState;
    FCursor: integer;
  private
    // ILayerHitTest
    function GetStartLocation: TFloatRect;
    procedure SetStartLocation(const Value: TFloatRect);
    function GetStartPosition: TPoint;
    procedure SetCurrentPosition(const Value: TPoint);
    function GetCurrentPosition: TPoint;
    function GetShift: TShiftState;
    procedure SetShift(Value: TShiftState);
    function GetCursor: integer; virtual;
    procedure SetCursor(Value: integer);
  public
    constructor Create(const AMousePosition: TPoint);
  end;

constructor TLayerHitTest.Create(const AMousePosition: TPoint);
begin
  inherited Create;
  FStartPosition := AMousePosition;
  FCurrentPosition := FStartPosition;
  FCursor := crDefault;
end;

function TLayerHitTest.GetCursor: integer;
begin
  Result := FCursor;
end;

function TLayerHitTest.GetCurrentPosition: TPoint;
begin
  Result := FCurrentPosition;
end;

procedure TLayerHitTest.SetCursor(Value: integer);
begin
  FCursor := Value;
end;

procedure TLayerHitTest.SetCurrentPosition(const Value: TPoint);
begin
  FCurrentPosition := Value;
end;

procedure TLayerHitTest.SetShift(Value: TShiftState);
begin
  FShift := Value;
end;

procedure TLayerHitTest.SetStartLocation(const Value: TFloatRect);
begin
  FStartLocation := Value;
end;

function TLayerHitTest.GetStartLocation: TFloatRect;
begin
  Result := FStartLocation;
end;

function TLayerHitTest.GetStartPosition: TPoint;
begin
  Result := FStartPosition;
end;

function TLayerHitTest.GetShift: TShiftState;
begin
  Result := FShift;
end;

type
  TLayerHitTestVertex = class(TLayerHitTest, ILayerHitTestVertex)
  private
    FVertex: integer;
    FStartValue: TFloatPoint;
  private
    // ILayerHitTestVertex
    function GetVertex: integer;
    procedure SetVertex(Value: integer);
    function GetStartValue: TFloatPoint;
    procedure SetStartValue(const Value: TFloatPoint);
  public
    constructor Create(const AMousePosition: TPoint; AVertex: integer; const AStartValue: TFloatPoint);
  end;

constructor TLayerHitTestVertex.Create(const AMousePosition: TPoint; AVertex: integer; const AStartValue: TFloatPoint);
begin
  inherited Create(AMousePosition);
  FVertex := AVertex;
  FStartValue := AStartValue;
end;

function TLayerHitTestVertex.GetStartValue: TFloatPoint;
begin
  Result := FStartValue;
end;

function TLayerHitTestVertex.GetVertex: integer;
begin
  Result := FVertex;
end;

procedure TLayerHitTestVertex.SetStartValue(const Value: TFloatPoint);
begin
  FStartValue := Value;
end;

procedure TLayerHitTestVertex.SetVertex(Value: integer);
begin
  FVertex := Value;
end;

type
  TLayerHitTestMove = class(TLayerHitTest, ILayerHitTestMove)
  end;


//------------------------------------------------------------------------------
//
//      TCustomRubberBandLayer
//
//------------------------------------------------------------------------------
constructor TCustomRubberBandLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;
  FHandleFrame := clBlack32;
  FHandleFill := clWhite32;
  FHandleSize := 3;
  FHandleHitZone := 1; // Just a tiny bit to make it easier to hit the handle
  FHandleFrameSize := 1;
  FQuantized := 1;
  FQuantizeShiftToggle := [ssAlt];
  FLayerOptions := LOB_VISIBLE or LOB_MOUSE_EVENTS;
  SetFrameStipple([clWhite32, clWhite32, clBlack32, clBlack32]);
  FPassMouse := TRubberbandPassMouse.Create(Self);
  FFrameStippleStep := 1;
  FFrameStippleCounter := 0;
end;

destructor TCustomRubberBandLayer.Destroy;
begin
  ChildLayer := nil;
  FPassMouse.Free;
  inherited;
end;

procedure TCustomRubberBandLayer.FreeNotification(ALayer: TCustomLayer);
begin
  if ALayer = FChildLayer then
    ChildLayer := nil;
end;

function TCustomRubberBandLayer.DoHitTest(X, Y: Integer): Boolean;
begin
  if (Visible) then
    Result := (GetHitTest(GR32.Point(X, Y)) <> nil)
  else
    Result := False;
end;

function TCustomRubberBandLayer.IsFrameVisible: boolean;
begin
  Result := (Length(FFrameStipplePattern) > 0);
end;

function TCustomRubberBandLayer.CanQuantize: boolean;
begin
  Result := (FQuantized > 0);
end;

function TCustomRubberBandLayer.ShouldQuantize(const AHitTest: ILayerHitTest): boolean;
begin
  Result := (CanQuantize) and ((QuantizeShiftToggle = []) or (AHitTest.Shift * [ssShift, ssAlt, ssCtrl] <> QuantizeShiftToggle));
end;

function TCustomRubberBandLayer.IsVertexVisible(VertexIndex: integer): boolean;
begin
  Result := (VertexIndex >= 0) and (VertexIndex <= High(Vertices));
end;

procedure TCustomRubberBandLayer.DoSetLocation(const NewLocation: TFloatRect);
var
  i: integer;
  Delta: TFloatPoint;
begin
  // Save current location
  Delta := Location.TopLeft;

  // Set new location
  inherited;

  UpdateChildLayer;

  // If the layer was moved...
  if (Delta <> Location.TopLeft) then
  begin
    // Calculate how much we moved the layer
    Delta := Location.TopLeft - Delta;

    // Move all vertices along with layer
    for i := 0 to High(FVertices) do
      FVertices[i] := FVertices[i] + Delta;

    Update;
  end;
end;

function TCustomRubberBandLayer.GetScaled: Boolean;
begin
  if (FChildLayer <> nil) then
    Result := FChildLayer.Scaled
  else
    Result := inherited GetScaled;
end;

procedure TCustomRubberBandLayer.SetScaled(Value: Boolean);
begin
  if (FChildLayer <> nil) then
    FChildLayer.Scaled := Value
  else
    inherited SetScaled(Value);
end;

function TCustomRubberBandLayer.FindVertex(const APosition: TPoint): integer;
var
  i: integer;
  Pos: TFloatPoint;
  HitZone: TFloatPoint;
  ScaleX, ScaleY: TFloat;
begin
  // If layer has Scaled=True then vertices are relative to bitmap,
  // otherwise they are relative to control.
  Pos := LayerCollection.ViewportToLocal(APosition, Scaled);

  HitZone.X := FHandleSize + FHandleHitZone;
  HitZone.Y := HitZone.X;

  if (Scaled) and (LayerCollection <> nil) then
  begin
    LayerCollection.GetViewportScale(ScaleX, ScaleY);
    HitZone.X := HitZone.X / ScaleX;
    HitZone.Y := HitZone.Y / ScaleY;
  end;

  for i := 0 to High(Vertices) do
    if (IsVertexVisible(i)) then
    begin
      if (Abs(Vertices[i].X - Pos.X) <= HitZone.X) and (Abs(Vertices[i].Y - Pos.Y) <= HitZone.Y) then
        Exit(i);
    end;

  Result := -1;
end;

function TCustomRubberBandLayer.GetHitTest(const APosition: TPoint; AShift: TShiftState): ILayerHitTest;
var
  Vertex: integer;
  p: TFloatPoint;
begin
  // APosition is in control coordinates
  Result := nil;

  Vertex := FindVertex(APosition);

  if (Vertex <> -1) then
  begin
    Result := TLayerHitTestVertex.Create(APosition, Vertex, Vertices[Vertex]);
    Result.Shift := AShift;
    Result.Cursor := GetHitTestCursor(Result);
    Result.StartLocation := Location;
  end else
  if AllowMove then
  begin
    // If layer has Scaled=True then vertices are relative to bitmap,
    // otherwise they are relative to control.
    p := LayerCollection.ViewportToLocal(APosition, Scaled);

    if PointInPolygon(p, FVertices) then
    begin
      Result := TLayerHitTestMove.Create(APosition);
      Result.Shift := AShift;
      Result.Cursor := GetHitTestCursor(Result);
      Result.StartLocation := Location;
    end;
end;
end;

procedure TCustomRubberBandLayer.SetHitTest(const AHitTest: ILayerHitTest);
begin
  FHitTest := AHitTest;
  FIsDragging := (FHitTest <> nil); // For backward compatibility
  end;

function TCustomRubberBandLayer.AllowMove: boolean;
begin
  Result := True;
end;

procedure TCustomRubberBandLayer.ApplyHitTestCursor(const AHitTest: ILayerHitTest);
var
  NewCursor: TCursor;
begin
  NewCursor := crDefault;

  if (AHitTest <> nil) then
    NewCursor := AHitTest.Cursor;

  if (NewCursor = crDefault) then
    NewCursor := Cursor;

  Screen.Cursor := NewCursor;
end;

function TCustomRubberBandLayer.GetHitTestCursor(const AHitTest: ILayerHitTest): TCursor;
var
  HitTestVertex: ILayerHitTestVertex;
begin
  Result := crDefault;

  if (AHitTest <> nil) then
  begin
    if Supports(AHitTest, ILayerHitTestVertex, HitTestVertex) then
    begin
      if (IsVertexVisible(HitTestVertex.Vertex)) then
        Result := crHandPoint;
    end else
    if Supports(AHitTest, ILayerHitTestMove) then
    begin
      if (AllowMove) then
        Result := crSizeAll;
    end;
  end;
end;

function TCustomRubberBandLayer.ApplyOffset(const AHitTest: ILayerHitTest; AQuantize: boolean): boolean;
var
  Delta: TFloatPoint;
  ScaleX, ScaleY: TFloat;
  NewLocation: TFloatRect;
  HitTestVertex: ILayerHitTestVertex;
  NewVertex: TFloatPoint;
begin
  Result := False;

  Delta := FloatPoint(AHitTest.CurrentPosition - AHitTest.StartPosition);

  if Scaled then
  begin
    LayerCollection.GetViewportScale(ScaleX, ScaleY);

    Delta.X := Delta.X / ScaleX;
    Delta.Y := Delta.Y / ScaleY;
  end;

  (*
  ** Move layer
  *)
  if Supports(AHitTest, ILayerHitTestMove) then
  begin

    // Apply delta/offset relative to start location
    NewLocation := AHitTest.StartLocation;

    if AQuantize then
    begin
      NewLocation.Left := Round((NewLocation.Left + Delta.X) / Quantized) * Quantized;
      NewLocation.Top := Round((NewLocation.Top + Delta.Y) / Quantized) * Quantized;
    end else
      NewLocation.TopLeft := NewLocation.TopLeft + Delta;

    DoHandleMove(-1, NewLocation.TopLeft);

    // Set new loaction but keep old width/height
    NewLocation.Right := NewLocation.Left + NewLocation.Width;
    NewLocation.Bottom := NewLocation.Top + NewLocation.Height;

    if (NewLocation <> Location) then
    begin
      Location := NewLocation;
      Result := True;
    end;

  end else
  (*
  ** Move handle
  *)
  if Supports(AHitTest, ILayerHitTestVertex, HitTestVertex) then
  begin

    // Apply delta/offset relative to start vertex position
    NewVertex := HitTestVertex.StartValue;

    if AQuantize then
    begin
      // Quantize top/left and...
      NewVertex.X := Round((NewVertex.X + Delta.X) / Quantized) * Quantized;
      NewVertex.Y := Round((NewVertex.Y + Delta.Y) / Quantized) * Quantized;
    end else
      NewVertex := NewVertex + Delta;

    DoHandleMove(HitTestVertex.Vertex, NewVertex);

    if (NewVertex <> Vertices[HitTestVertex.Vertex]) then
    begin
      // Erase old, update, paint new
      Vertex[HitTestVertex.Vertex] := NewVertex;
      Result := True;
    end;

end;
end;

procedure TCustomRubberBandLayer.DoHandleClicked(VertexIndex: integer);
begin
  if (Assigned(FOnHandleClicked)) then
    FOnHandleClicked(Self, VertexIndex);
end;

procedure TCustomRubberBandLayer.DoHandleMove(VertexIndex: integer; var APos: TFloatPoint);
begin
  if (Assigned(FOnHandleMove)) then
    FOnHandleMove(Self, VertexIndex, APos);
end;

procedure TCustomRubberBandLayer.DoHandleMoved(VertexIndex: integer);
begin
  if (Assigned(FOnHandleMoved)) then
    FOnHandleMoved(Self, VertexIndex);
end;

procedure TCustomRubberBandLayer.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;

  // Update hittest shift state
  if (ActiveHitTest <> nil) and (ActiveHitTest.Shift <> Shift) and (LayerCollection.MouseListener = Self) then
    // Generate mouse move
    MouseMove(Shift, ActiveHitTest.CurrentPosition.X, ActiveHitTest.CurrentPosition.Y);
end;

procedure TCustomRubberBandLayer.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;

  // Update hittest shift state
  if (ActiveHitTest <> nil) and (ActiveHitTest.Shift <> Shift) and (LayerCollection.MouseListener = Self) then
    // Generate mouse move
    MouseMove(Shift, ActiveHitTest.CurrentPosition.X, ActiveHitTest.CurrentPosition.Y);
end;

procedure TCustomRubberBandLayer.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  PositionedLayer: TPositionedLayer;
  HitTestVertex: ILayerHitTestVertex;
  VertexIndex: integer;
  NewHitTest: ILayerHitTest;
begin
  // Pass mouse event to other layers...
  if FPassMouse.Enabled then
  begin
    // First pass to child layer
    if FPassMouse.ToChild and (ChildLayer <> nil) then
    begin
      ChildLayer.MouseDown(Button, Shift, X, Y);

      if FPassMouse.CancelIfPassed then
        Exit;
    end;

    // Then pass to layer under mouse cursor
    if FPassMouse.ToLayerUnderCursor then
    begin
      PositionedLayer := FPassMouse.GetChildUnderCursor(X, Y, Self);

      // ...unless it's the same as the child layer and we handled the child layer above
      if (PositionedLayer <> nil) and ((not FPassMouse.ToChild) or (PositionedLayer <> ChildLayer)) then
      begin
      PositionedLayer.MouseDown(Button, Shift, X, Y);

      if FPassMouse.CancelIfPassed then
        Exit;
    end;
  end;
  end;

  if (ActiveHitTest <> nil) then
    Exit;

  // Create a new HitTest context
  NewHitTest := GetHitTest(GR32.Point(X, Y), Shift);
  SetHitTest(NewHitTest);

  if (ActiveHitTest <> nil) then
  begin
    // Did we click a vertex/handle?
    if (Supports(ActiveHitTest, ILayerHitTestVertex, HitTestVertex)) then
      VertexIndex := HitTestVertex.Vertex
    else
    VertexIndex := -1;

    // Generate an OnHandleClicked event
    DoHandleClicked(VertexIndex);
  end;

  inherited;
end;

procedure TCustomRubberBandLayer.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  MoveHitTest: ILayerHitTest;
  DoQuantize: Boolean;
  HitTestVertex: ILayerHitTestVertex;
  VertexIndex: integer;
begin
  // If there's no active HitTest context then we're just moving the mouse, otherwise
  // a drag is in progress.
  if (ActiveHitTest = nil) then
  begin
    // Create a temporary HitTest context
    MoveHitTest := GetHitTest(GR32.Point(X, Y), Shift);

    // Use the HitTest context to update the cursor
    ApplyHitTestCursor(MoveHitTest);

    exit;
  end;

  // We are dragging; Update the HitTest context with the current state
  ActiveHitTest.Shift := Shift;
  ActiveHitTest.CurrentPosition := GR32.Point(X, Y);

  // Use the HitTest context to update the cursor
  ApplyHitTestCursor(ActiveHitTest);

  // Determine if we should quantize the coordinates.
  DoQuantize := ShouldQuantize(ActiveHitTest);

  if ApplyOffset(ActiveHitTest, DoQuantize) then
  begin
    if (ActiveHitTest <> nil) then
  begin
      // Are we dragging a vertex/handle?
      if (Supports(ActiveHitTest, ILayerHitTestVertex, HitTestVertex)) then
        VertexIndex := HitTestVertex.Vertex
      else
      VertexIndex := -1;

      // Generate an OnHandleMoved event
      DoHandleMoved(VertexIndex);
    end;

    // Generate an OnUserChange event (backward compatibility)
    if Assigned(FOnUserChange) then
      FOnUserChange(Self);
  end;
end;

procedure TCustomRubberBandLayer.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  PositionedLayer: TPositionedLayer;
begin
  // Pass mouse event to other layers...
  if FPassMouse.Enabled then
  begin
    // First pass to child layer
    if FPassMouse.ToChild and (ChildLayer <> nil) then
    begin
      ChildLayer.MouseUp(Button, Shift, X, Y);

      if FPassMouse.CancelIfPassed then
        Exit;
    end;

    // Then pass to layer under mouse cursor
    if FPassMouse.ToLayerUnderCursor then
    begin
      PositionedLayer := FPassMouse.GetChildUnderCursor(X, Y, Self);

      // ...unless it's the same as the child layer and we handled the child layer above
      if (PositionedLayer <> nil) and ((not FPassMouse.ToChild) or (PositionedLayer <> ChildLayer)) then
      begin
      PositionedLayer.MouseUp(Button, Shift, X, Y);

      if FPassMouse.CancelIfPassed then
        Exit;
    end;
  end;
  end;

  SetHitTest(nil);

  inherited;
end;

procedure TCustomRubberBandLayer.DrawHandle(Buffer: TBitmap32; const p: TFloatPoint; AIndex: integer; const DrawParams: TRubberBandHandleDrawParams);

  function Diamond(const p: TFloatPoint; const Radius: TFloat): TArrayOfFloatPoint; {$IFDEF USEINLINING} inline; {$ENDIF}
  begin
    SetLength(Result, 4);
    Result[0] := FloatPoint(p.X, p.Y - Radius);
    Result[1] := FloatPoint(p.X + Radius, p.Y);
    Result[2] := FloatPoint(p.X, p.Y + Radius);
    Result[3] := FloatPoint(p.X - Radius, p.Y);
  end;

var
  Handle: TFloatRect;
  HandleRect: TRect;
  Shape: TArrayOfArrayOfFloatPoint;
  Colors: array[0..1] of TColor32;
  Renderer: TPolygonRenderer32VPR;
begin
  if (DrawParams.HandleStyle = hsSquare) and (DrawParams.HandleFrameSize = 1.0) and (Frac(DrawParams.HandleSize) = 0.0) then
  begin
    // Simple 1px framed square

    Handle := FloatRect(p, p);
    GR32.InflateRect(Handle, DrawParams.HandleSize, DrawParams.HandleSize);
    HandleRect := MakeRect(Handle, rrClosest);

    if (AlphaComponent(DrawParams.HandleFrame) > 0) then
    begin
      Buffer.FrameRectTS(HandleRect, DrawParams.HandleFrame);
      GR32.InflateRect(HandleRect, -1, -1);
    end;

    if (AlphaComponent(DrawParams.HandleFill) > 0) then
      Buffer.FillRectTS(HandleRect, DrawParams.HandleFill);

    exit;
  end;

  // Outer: Shape[0]
  // Inner: Shape[1]
  // Stroke: Shape[0]+Shape[1]
  // Fill: Shape[1]
  SetLength(Shape, 2);

  case DrawParams.HandleStyle of
    hsSquare:
      begin
        Handle := FloatRect(p, p);
        GR32.InflateRect(Handle, DrawParams.HandleSize, DrawParams.HandleSize);
        Shape[0] := Rectangle(Handle);
      end;

    hsCircle:
      Shape[0] := Circle(p, DrawParams.HandleSize);

    hsDiamond:
      Shape[0] := Diamond(p, DrawParams.HandleSize);
  end;

  if (DrawParams.HandleFrameSize = DrawParams.HandleSize) then
  begin
    // Frame completely covers area
    Shape[1] := Shape[0];
    Shape[0] := nil;
    Colors[1] := DrawParams.HandleFrame;
  end else
  if (DrawParams.HandleFrameSize > 0) then
  begin
    Shape[1] := ReversePolygon(Grow(Shape[0], -DrawParams.HandleFrameSize, jsBevel));
    Colors[0] := DrawParams.HandleFrame;
    Colors[1] := DrawParams.HandleFill;
  end else
  begin
    // No frame
    Shape[1] := Shape[0];
    Shape[0] := nil;
    Colors[1] := DrawParams.HandleFill;
  end;

  Renderer := TPolygonRenderer32VPR.Create(Buffer);
  try

    // Fill
    if (Shape[1] <> nil) and (AlphaComponent(Colors[1]) > 0) then
    begin
      Renderer.Color := Colors[1];
      Renderer.PolygonFS(Shape[1]);
    end;

    // Stroke
    if (Shape[0] <> nil) and (AlphaComponent(Colors[0]) > 0) then
    begin
      Renderer.Color := Colors[0];
      Renderer.PolyPolygonFS(Shape);
    end;

  finally
    Renderer.Free;
  end;
end;

procedure TCustomRubberBandLayer.DoDrawVertex(Buffer: TBitmap32; const R: TRect; VertexIndex: integer);
var
  p: TFloatPoint;
  DrawParams: TRubberBandHandleDrawParams;
  Handled: boolean;
begin
  // Coordinate specifies exact center of handle. I.e. center of
  // pixel if handle is odd number of pixels wide.

  p := LayerCollection.LocalToViewport(FVertices[VertexIndex], Scaled);

  DrawParams.HandleStyle := HandleStyle;
  DrawParams.HandleSize := HandleSize;
  DrawParams.HandleFill := HandleFill;
  DrawParams.HandleFrame := HandleFrame;
  DrawParams.HandleFrameSize := HandleFrameSize;

  Handled := False;
  if Assigned(FOnPaintHandle) then
    FOnPaintHandle(Self, Buffer, p, VertexIndex, DrawParams, Handled);

  if (not Handled) then
    DrawHandle(Buffer, p, VertexIndex, DrawParams);
end;

procedure TCustomRubberBandLayer.DoDrawVertices(Buffer: TBitmap32; const R: TRect; var Handled: boolean);
var
  i: integer;
begin
  for i := 0 to High(FVertices) do
    if (IsVertexVisible(i)) then
      DoDrawVertex(Buffer, R, i);
  Handled := True;
end;

procedure TCustomRubberBandLayer.DrawFrame(Buffer: TBitmap32; const R: TRect);
var
  i: integer;
  p: TFloatPoint;
begin
  if (Length(FVertices) = 0) then
    exit;

  Buffer.SetStipple(FrameStipple);
  Buffer.StippleStep := FrameStippleStep;
  Buffer.StippleCounter := FrameStippleCounter;

  p := LayerCollection.LocalToViewport(FVertices[High(FVertices)], Scaled);
    Buffer.MoveToF(p.X, p.Y);

  for i := 0 to High(FVertices) do
  begin
    p := LayerCollection.LocalToViewport(FVertices[i], Scaled);
    Buffer.LineToFSP(p.X, p.Y);
  end;
end;

procedure TCustomRubberBandLayer.DoUpdateFrame(Buffer: TBitmap32; const R: TRect);
var
  ScaleX, ScaleY, ShiftX, ShiftY: TFloat;
  DoScale: boolean;
  i: integer;
  Index: integer;
  Segment: TFloatRect;
  LineRect: TRect;
begin
  if (Length(FVertices) = 0) then
    exit;

  if (Scaled) and (LayerCollection <> nil) then
  begin
    LayerCollection.GetViewportShift(ShiftX, ShiftY);
    LayerCollection.GetViewportScale(ScaleX, ScaleY);
    DoScale := True;
  end else
    DoScale := False;

  for i := 0 to Length(FVertices) do // Note: Upper bound is Length(FVertices) on purpose
  begin
    Index := i mod Length(FVertices);

    // Same as: LayerCollection.LocalToViewport(FVertices[i], Scaled)
    if (DoScale) then
    begin
      Segment.Right := FVertices[Index].X * ScaleX + ShiftX;
      Segment.Bottom := FVertices[Index].Y * ScaleY + ShiftY;
    end else
      Segment.BottomRight := FVertices[Index];

    if (i > 0) then
    begin
      // Invalidate segment
      LineRect := MakeRect(Segment, rrOutside);
      Changed(LineRect, AREAINFO_LINE + 1);
    end;

    Segment.TopLeft := Segment.BottomRight;
  end;
end;

procedure TCustomRubberBandLayer.DoUpdateVertex(Buffer: TBitmap32; const R: TRect; VertexIndex: integer);
var
  p: TFloatPoint;
  Handle: TFloatRect;
  HandleRect: TRect;
  Handled: boolean;
begin
  p := LayerCollection.LocalToViewport(FVertices[VertexIndex], Scaled);
  Handle.TopLeft := p;
  Handle.BottomRight := Handle.TopLeft;
  Handle.Inflate(FHandleSize, FHandleSize);
  HandleRect := MakeRect(Handle, rrOutside);

  Handled := False;
  if Assigned(FOnUpdateHandle) then
    FOnUpdateHandle(Self, Buffer, p, VertexIndex, HandleRect, Handled);

  if (not Handled) then
    UpdateRect(HandleRect);
end;

procedure TCustomRubberBandLayer.DoUpdateVertices(Buffer: TBitmap32; const R: TRect; var Handled: boolean);
var
  ScaleX, ScaleY, ShiftX, ShiftY: TFloat;
  DoScale: boolean;
  i: integer;
  Handle: TFloatRect;
  HandleRect: TRect;
begin
  if (Length(FVertices) = 0) then
    exit;

  if (Scaled) and (LayerCollection <> nil) then
  begin
    LayerCollection.GetViewportShift(ShiftX, ShiftY);
    LayerCollection.GetViewportScale(ScaleX, ScaleY);
    DoScale := True;
  end else
    DoScale := False;

  if Assigned(FOnUpdateHandle) then
  begin

    for i := 0 to High(FVertices) do
      if (IsVertexVisible(i)) then
        DoUpdateVertex(Buffer, R, i);

  end else
  begin

    for i := 0 to High(FVertices) do
      if (IsVertexVisible(i)) then
      begin
        // Same as: LayerCollection.LocalToViewport(FVertices[i], Scaled)
        if (DoScale) then
        begin
          Handle.Left := FVertices[i].X * ScaleX + ShiftX;
          Handle.Top := FVertices[i].Y * ScaleY + ShiftY;
        end else
          Handle.TopLeft := FVertices[i];
        Handle.BottomRight := Handle.TopLeft;
        GR32.InflateRect(Handle, FHandleSize, FHandleSize);
        HandleRect := MakeRect(Handle, rrOutside);
        UpdateRect(HandleRect);
      end;

  end;

  Handled := True;
end;

procedure TCustomRubberBandLayer.DoDrawUpdate(Buffer: TBitmap32;
  FrameHandler: TRubberBandPaintFrameHandler;
  VerticesHandler: TRubberBandPaintHandlesHandler;
  VertexHandler: TRubberBandPaintHandleHandler);
var
  R: TRect;
  i: integer;
  Handled: boolean;
begin
  R := MakeRect(GetAdjustedLocation);

  if (Assigned(FrameHandler)) and (IsFrameVisible) then
    FrameHandler(Buffer, R);

  Handled := False;
  if (Assigned(VerticesHandler)) then
    VerticesHandler(Buffer, R, Handled);

  if (not Handled) and (Assigned(VertexHandler)) then
    for i := 0 to High(Vertices) do
      if (IsVertexVisible(i)) then
        VertexHandler(Buffer, R, i);
end;

procedure TCustomRubberBandLayer.Paint(Buffer: TBitmap32);
begin
  DoDrawUpdate(Buffer, DrawFrame, DoDrawVertices, DoDrawVertex);
end;

procedure TCustomRubberBandLayer.Quantize;
begin
  if (Quantized <> 0) then
    Location := FloatRect(
      Round(Location.Left / Quantized) * Quantized,
      Round(Location.Top / Quantized) * Quantized,
      Round(Location.Right / Quantized) * Quantized,
      Round(Location.Bottom / Quantized) * Quantized);
end;

procedure TCustomRubberBandLayer.SetChildLayer(Value: TPositionedLayer);
begin
  if (FChildLayer <> nil) then
    FChildLayer.RemoveFreeNotification(Self);
    
  FChildLayer := Value;

  if (FChildLayer <> nil) then
  begin
    BeginUpdate;
    try
      Location := FChildLayer.Location;
      inherited SetScaled(FChildLayer.Scaled); // Not really necessary
    finally
      EndUpdate;
    end;
    FChildLayer.AddFreeNotification(Self);
  end;
end;

procedure TCustomRubberBandLayer.SetHandleFrameSize(Value: TFloat);
begin
  if Value < 0.0 then
    Value := 0
  else
  if Value > FHandleSize then
    Value := FHandleSize;

  if Value <> FHandleFrameSize then
  begin
    // Size doesn't change; No need to erase old

    FHandleFrameSize := Value;
    UpdateVertices;
  end;
end;

procedure TCustomRubberBandLayer.SetHandleHitZone(const Value: TFloat);
begin
  if (Value >= 0) then
    FHandleHitZone := Value;
end;

procedure TCustomRubberBandLayer.SetHandleFill(Value: TColor32);
begin
  if Value <> FHandleFill then
  begin
    // Size doesn't change; No need to erase old

    FHandleFill := Value;
    UpdateVertices;
  end;
end;

procedure TCustomRubberBandLayer.SetHandleFrame(Value: TColor32);
begin
  if Value <> FHandleFrame then
  begin
    // Size doesn't change; No need to erase old

    FHandleFrame := Value;
    UpdateVertices;
  end;
end;

procedure TCustomRubberBandLayer.SetHandleSize(Value: TFloat);
begin
  if Value < 1 then
    Value := 1;

  if Value <> FHandleSize then
  begin
    // Erase old
    UpdateVertices;

    FHandleSize := Value;
    if FHandleSize < FHandleFrameSize then
      FHandleFrameSize := FHandleSize;

    // Paint new
    UpdateVertices;
  end;
end;

procedure TCustomRubberBandLayer.SetHandleStyle(const Value: TRubberBandHandleStyle);
begin
  if (FHandleStyle <> Value) then
  begin
    // Erase old
    UpdateVertices;

    FHandleStyle := Value;

    // Paint new
    UpdateVertices;
  end;
end;

procedure TCustomRubberBandLayer.SetFrameStipple(const Value: TArrayOfColor32);
begin
  FFrameStipplePattern := Copy(Value);
  FFrameStippleCounter := Wrap(FFrameStippleCounter, Length(FFrameStipplePattern));
  UpdateFrame;
end;

procedure TCustomRubberBandLayer.SetFrameStippleStep(const Value: TFloat);
begin
  if Value <> FFrameStippleStep then
  begin
    FFrameStippleStep := Value;
    UpdateFrame;;
  end;
end;

procedure TCustomRubberBandLayer.UpdateFrame;
begin
  DoDrawUpdate(nil, DoUpdateFrame, nil, nil);
end;

procedure TCustomRubberBandLayer.UpdateVertices;
begin
  DoDrawUpdate(nil, nil, DoUpdateVertices, DoUpdateVertex);
end;

procedure TCustomRubberBandLayer.Update;
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

  DoDrawUpdate(nil, DoUpdateFrame, DoUpdateVertices, DoUpdateVertex);
end;

procedure TCustomRubberBandLayer.UpdateChildLayer;
begin
  if (FChildLayer <> nil) then
    FChildLayer.Location := Location;
end;

procedure TCustomRubberBandLayer.SetFrameStippleCounter(const Value: TFloat);
begin
  if Value <> FFrameStippleCounter then
  begin
    FFrameStippleCounter := Wrap(Value, Length(FFrameStipplePattern));
    UpdateFrame;
  end;
end;

procedure TCustomRubberBandLayer.SetLayerOptions(Value: Cardinal);
begin
  inherited SetLayerOptions(Value and not LOB_NO_UPDATE); // workaround for changed behaviour
end;

procedure TCustomRubberBandLayer.SetQuantized(const Value: Integer);
begin
  if Value < 1 then
    raise Exception.Create('Value must be larger than zero!');

  FQuantized := Value;
end;

function TCustomRubberBandLayer.GetVertex(Index: integer): TFloatPoint;
begin
  Result := FVertices[Index];
end;

procedure TCustomRubberBandLayer.SetVertex(Index: integer; const Value: TFloatPoint);
begin
  if (FVertices[Index] = Value) then
    exit;

  // Erase old
  Update;

  FVertices[Index] := Value;

  // Paint new
  Update;
end;

procedure TCustomRubberBandLayer.SetVertices(const Value: TArrayOfFloatPoint);
begin
  // Erase old
  Update;

  FVertices := Copy(Value);

  // Paint new
  Update;
end;


//------------------------------------------------------------------------------
//
//      TRubberbandLayer
//
//------------------------------------------------------------------------------
constructor TRubberbandLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;

  FHandles := [rhCenter, rhSides, rhCorners, rhFrame];
  FValidDragStates := GetValidDragStates;

  FMinWidth := 10;
  FMinHeight := 10;
  Quantized := 8;
end;

function TRubberbandLayer.GetHitTest(const APosition: TPoint; AShift: TShiftState): ILayerHitTest;
var
  R: TRect;
begin
  // APosition is in control coordinates

  Result := inherited;

  // Hit test against the layer bounding rectangle.
  // This is only kept for backward compatibility as the base class
  // already does hit testing against the vertex polygon.
  if (Result = nil) and AllowMove then
  begin
    R := MakeRect(GetAdjustedLocation);

    if (GR32.PtInRect(R, APosition)) then
    begin
      Result := TLayerHitTestMove.Create(APosition);
      Result.Shift := AShift;
      Result.Cursor := GetHitTestCursor(Result);
    end;
  end;
end;

function TRubberbandLayer.GetHitTestCursor(const AHitTest: ILayerHitTest): TCursor;

  function SnapAngleTo45(Angle: integer): integer;
  begin
    Result := (((Angle + 45 div 2) div 45) * 45 + 360) mod 360;
  end;

  function AngleToDirection(Angle: integer): TResizeDirection;
  begin
    Result := TResizeDirection(SnapAngleTo45(Angle) div 45);
  end;

var
  HitTestVertex: ILayerHitTestVertex;
var
  Angle: integer;
  Direction: TResizeDirection;
  NewCursor: TCursor;
const
  VertexToAngle: array[0..7] of integer =
    //
    // 0     1     2
    //
    // 7           3
    //
    // 6     5     4
    //
    (135, 90, 45, 0, 315, 270, 225, 180);
begin
  Result := inherited GetHitTestCursor(AHitTest);

  if (AHitTest <> nil) then
  begin
    if Supports(AHitTest, ILayerHitTestVertex, HitTestVertex) then
    begin

      Angle := VertexToAngle[HitTestVertex.Vertex];

      // Call GetHandleCursor for backward compatibility in case a
      // derived class has overridden it. It will return Low(TCursor)
      // if GetHandleCursor has not been overridden.
      Result := GetHandleCursor(VertexToDragState[HitTestVertex.Vertex], Angle);

      if (Result = Low(TCursor)) then
      begin
        Direction := AngleToDirection(Angle);
        Result := DirectionCursors[Direction];
      end;

    end else
    if (Supports(AHitTest, ILayerHitTestMove)) then
    begin

      NewCursor := GetHandleCursor(dsMove, 0);

      if (NewCursor <> Low(TCursor)) then
        Result := NewCursor;

    end;
  end;
end;

function TRubberbandLayer.GetValidDragStates: TValidDragStates;
begin
  Result := [];

  if (rhCenter in FHandles) then
    Include(Result, dsMove);

  if (rhSides in FHandles) then
  begin
    if not(rhNotRightSide in FHandles) then
      Include(Result, dsSizeR);
    if not(rhNotBottomSide in FHandles) then
      Include(Result, dsSizeB);
    if not(rhNotLeftSide in FHandles) then
      Include(Result, dsSizeL);
    if not(rhNotTopSide in FHandles) then
      Include(Result, dsSizeT);
  end;

  if (rhCorners in FHandles) then
  begin
    if not(rhNotBRCorner in FHandles) then
      Include(Result, dsSizeBR);
    if not(rhNotBLCorner in FHandles) then
      Include(Result, dsSizeBL);
    if not(rhNotTRCorner in FHandles) then
      Include(Result, dsSizeTR);
    if not(rhNotTLCorner in FHandles) then
      Include(Result, dsSizeTL);
  end;

end;

function TRubberbandLayer.GetHandleCursor(DragState: TRBDragState; Angle: integer): TCursor;
(*
var
  Vertex: integer;
*)
begin
  Result := Low(TCursor);
(*
  if (DragState in [dsNone, dsMove]) then
    Vertex := -1
  else
  begin
    case Angle of
        0 .. 22: Vertex := 3;
       23 .. 57: Vertex := 2;
       58 ..112: Vertex := 1;
      113 ..157: Vertex := 0;
      158 ..202: Vertex := 7;
      203 ..247: Vertex := 6;
      248 ..292: Vertex := 5;
      293 ..337: Vertex := 4;
      338 ..360: Vertex := 3;
    else
      Vertex := -1
    end;
  end;
  Result := GetVertexCursor(Vertex);
*)
end;

function TRubberbandLayer.AllowMove: boolean;
begin
  Result := (dsMove in FValidDragStates);
end;

function TRubberbandLayer.ApplyOffset(const AHitTest: ILayerHitTest; AQuantize: boolean): boolean;

  // Move Left/Top relative to Right/Bottom
  procedure OffsetLeftTop(var LT: TFloat; RB: TFloat; Delta, MinSize, MaxSize: TFloat);
  begin
    LT := LT + Delta;

    if (RB - LT < MinSize) then
      LT := RB - MinSize;

    if (MaxSize >= MinSize) and (RB - LT > MaxSize) then
        LT := RB - MaxSize;

    if AQuantize then
      LT := Round(LT / Quantized) * Quantized;
  end;

  // Move Right/Bottom relative to Left/Top
  procedure OffsetRightBottom(LT: TFloat; var RB: TFloat; Delta, MinSize, MaxSize: TFloat);
  begin
    RB := RB + Delta;

    if (RB - LT < MinSize) then
      RB := LT + MinSize;

    if (MaxSize >= MinSize) and (RB - LT > MaxSize) then
        RB := LT + MaxSize;

    if AQuantize then
      RB := Round(RB / Quantized) * Quantized;
  end;

var
  Delta: TFloatPoint;
  ScaleX, ScaleY: TFloat;
  StartLocation: TFloatRect;
  NewLocation: TFloatRect;
  HitTestVertex: ILayerHitTestVertex;
  DragState: TRBDragState;
begin
  Result := False;

  Delta := FloatPoint(AHitTest.CurrentPosition - AHitTest.StartPosition);

  if Scaled then
  begin
    LayerCollection.GetViewportScale(ScaleX, ScaleY);

    Delta.X := Delta.X / ScaleX;
    Delta.Y := Delta.Y / ScaleY;
  end;

  // Apply delta/offset relative to start location
  StartLocation := AHitTest.StartLocation;

  (*
  ** Move layer
  *)
  if Supports(AHitTest, ILayerHitTestMove) then
  begin

    DragState := dsMove;

    if AQuantize then
    begin
      NewLocation.Left := Round((StartLocation.Left + Delta.X) / Quantized) * Quantized;
      NewLocation.Top := Round((StartLocation.Top + Delta.Y) / Quantized) * Quantized;
  end else
      NewLocation.TopLeft := StartLocation.TopLeft + Delta;

    // Set new loaction but keep old width/height
    NewLocation.Right := NewLocation.Left + StartLocation.Width;
    NewLocation.Bottom := NewLocation.Top + StartLocation.Height;

  end else
  (*
  ** Move handle
  *)
  if Supports(AHitTest, ILayerHitTestVertex, HitTestVertex) then
  begin

    DragState := VertexToDragState[HitTestVertex.Vertex];

    NewLocation := StartLocation;

    // Left handles
    if DragState in [dsSizeL, dsSizeTL, dsSizeBL] then
      OffsetLeftTop(NewLocation.Left, NewLocation.Right, Delta.X, MinWidth, MaxWidth)
    else
    // Right handles
    if DragState in [dsSizeR, dsSizeTR, dsSizeBR] then
      OffsetRightBottom(NewLocation.Left, NewLocation.Right, Delta.X, MinWidth, MaxWidth);

    // Top handles
    if DragState in [dsSizeT, dsSizeTL, dsSizeTR] then
      OffsetLeftTop(NewLocation.Top, NewLocation.Bottom, Delta.Y, MinHeight, MaxHeight)
    else
    // Bottom handles
    if DragState in [dsSizeB, dsSizeBL, dsSizeBR] then
      OffsetRightBottom(NewLocation.Top, NewLocation.Bottom, Delta.Y, MinHeight, MaxHeight);

  end else
    exit;

  if (roConstrained in FOptions) then
    DoConstrain(StartLocation, NewLocation, DragState, AHitTest.Shift);

  if (roProportional in FOptions) then
  begin
    case DragState of
      dsSizeB, dsSizeBR:
        NewLocation.Right := StartLocation.Left + StartLocation.Width * NewLocation.Height / StartLocation.Height;

      dsSizeT, dsSizeTL:
        NewLocation.Left := StartLocation.Right - StartLocation.Width * NewLocation.Height / StartLocation.Height;

      dsSizeR, dsSizeBL:
        NewLocation.Bottom := StartLocation.Top + StartLocation.Height * NewLocation.Width / StartLocation.Width;

      dsSizeL, dsSizeTR:
        NewLocation.Top := StartLocation.Bottom - StartLocation.Height * NewLocation.Width / StartLocation.Width;
    end;
  end;

  DoResizing(StartLocation, NewLocation, DragState, AHitTest.Shift);

  if (NewLocation <> Location) then
  begin
    Location := NewLocation;
    Result := True;
  end;
end;

procedure TRubberbandLayer.DoSetDragState(const Value: TRBDragState; const X, Y: Integer);
var
  HitTest: ILayerHitTest;
  Vertex: integer;
begin
  HitTest := nil;
  FDragState := Value;

  if (FDragState <> dsNone) then
  begin
    Vertex := DragStateToVertex[FDragState];
    if (Vertex <> -1) then
      HitTest := TLayerHitTestVertex.Create(GR32.Point(X, Y), Vertex, Vertices[Vertex])
    else
    if (FDragState = dsMove) then
      HitTest := TLayerHitTestMove.Create(GR32.Point(X, Y));
  end;

  inherited SetHitTest(HitTest);
end;

procedure TRubberbandLayer.SetDragState(const Value: TRBDragState; const X, Y: Integer);
begin
  // Indirection to avoid internal deprecated warnings
  DoSetDragState(Value, X, Y);
end;

procedure TRubberbandLayer.SetDragState(const Value: TRBDragState);
begin
  // Indirection to avoid internal deprecated warnings
  DoSetDragState(Value, 0, 0);
end;

function TRubberbandLayer.GetDragState(X, Y: Integer): TRBDragState;
var
  HitTest: ILayerHitTest;
  HitTestVertex: ILayerHitTestVertex;
begin
  HitTest := GetHitTest(GR32.Point(X, Y));

  if (HitTest = nil) then
    Result := dsNone
  else
  if (Supports(HitTest, ILayerHitTestVertex, HitTestVertex)) then
    Result := VertexToDragState[HitTestVertex.Vertex]
  else
  if (Supports(HitTest, ILayerHitTestMove)) then
    Result := dsMove
  else
    Result := dsNone
end;

function TRubberbandLayer.IsFrameVisible: boolean;
begin
  Result := (inherited IsFrameVisible) and (rhFrame in FHandles);
end;

function TRubberbandLayer.CanQuantize: boolean;
begin
  Result := (inherited CanQuantize) and (roQuantized in FOptions);
end;

function TRubberbandLayer.IsVertexVisible(VertexIndex: integer): boolean;
begin
  Result := (inherited IsVertexVisible(VertexIndex)) and (VertexToDragState[VertexIndex] in FValidDragStates);
end;

procedure TRubberbandLayer.DoSetLocation(const NewLocation: TFloatRect);
var
  Handles: TArrayOfFloatPoint;
begin
  inherited;
  SetLength(Handles, 8);

  Handles[0].X := Location.Left;
  Handles[0].Y := Location.Top;
  Handles[2].X := Location.Right;
  Handles[2].Y := Handles[0].Y;
  Handles[4].X := Handles[2].X;
  Handles[4].Y := Location.Bottom;
  Handles[6].X := Handles[0].X;
  Handles[6].Y := Handles[4].Y;

  Handles[1].X := (Handles[0].X + Handles[2].X) / 2;
  Handles[1].Y := Handles[0].Y;
  Handles[3].X := Handles[2].X;
  Handles[3].Y := (Handles[0].Y + Handles[4].Y) / 2;
  Handles[5].X := Handles[1].X;
  Handles[5].Y := Handles[4].Y;
  Handles[7].X := Handles[0].X;
  Handles[7].Y := Handles[3].Y;

  Vertices := Handles;
end;

procedure TRubberbandLayer.DoResizing(const OldLocation: TFloatRect; var NewLocation: TFloatRect; DragState: TRBDragState; Shift: TShiftState);
begin
  if Assigned(FOnResizing) then
    FOnResizing(Self, OldLocation, NewLocation, DragState, Shift);
end;

procedure TRubberbandLayer.DoConstrain(const OldLocation: TFloatRect; var NewLocation: TFloatRect; DragState: TRBDragState; Shift: TShiftState);
begin
  if Assigned(FOnConstrain) then
    FOnConstrain(Self, OldLocation, NewLocation, DragState, Shift);
end;

procedure TRubberbandLayer.SetHandles(Value: TRBHandles);
begin
  if Value <> FHandles then
  begin
    // Erase old
    UpdateVertices;

    FHandles := Value;
    FValidDragStates := GetValidDragStates;

    // Paint new
    UpdateVertices;
  end;
end;

procedure TRubberbandLayer.SetOptions(const Value: TRBOptions);
begin
  FOptions := Value;
end;

procedure TRubberbandLayer.DrawFrame(Buffer: TBitmap32; const R: TRect);
begin
  Buffer.SetStipple(FrameStipple);
  Buffer.StippleCounter := 0;
  Buffer.StippleStep := FrameStippleStep;
  Buffer.StippleCounter := FrameStippleCounter;
  Buffer.FrameRectTSP(R.Left, R.Top, R.Right, R.Bottom);
end;

procedure TRubberbandLayer.DoUpdateFrame(Buffer: TBitmap32; const R: TRect);
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

end.
