unit GR32_Image;

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
 * Mattias Andersson <mattias@centaurix.com>
 * Andre Beckedorf <Andre@metaException.de>
 * Andrew P. Rybin <aprybin@users.sourceforge.net>
 * Dieter Köhler <dieter.koehler@philo.de>
 * Michael Hansen <dyster_tid@hotmail.com>
 *
 * ***** END LICENSE BLOCK ***** *)

interface

// Define CONSOLIDATE_UPDATERECTS to consolidate potentially overlapping
// update areas into as few separate non-overlapping areas as possible.
{$define CONSOLIDATE_UPDATERECTS}

{$I GR32.inc}

uses
{$if defined(WINDOWS)}
  Windows,
{$ifend}
{$IFDEF FPC}
  LCLIntf, LCLType, LMessages, Types,
{$ELSE}
  Messages, {$IFDEF COMPILERXE2_UP}Types,{$ENDIF}
{$ENDIF}
  Graphics, Controls, Forms,
  Classes, SysUtils, GR32, GR32_Layers, GR32_RangeBars, GR32_Containers,
  GR32_RepaintOpt;

{$IFNDEF FPC}
// Animated zoom relies on amEasing which relies on the System.Diagnostics unit (TStopwatch)
{$define AnimatedZoom}
{$ENDIF}

const
  { Paint Stage Constants }
  PST_CUSTOM            = 1;   // Calls OnPaint with # of current stage in parameter
  PST_CLEAR_BUFFER      = 2;   // Clears the buffer
  PST_CLEAR_BACKGND     = 3;   // Clears a visible buffer area
  PST_DRAW_BITMAP       = 4;   // Draws a bitmap
  PST_DRAW_LAYERS       = 5;   // Draw layers (Parameter = Layer Mask)
  PST_CONTROL_FRAME     = 6;   // Draws a dotted frame around the control
  PST_BITMAP_FRAME      = 7;   // Draws a dotted frame around the scaled bitmap

type
  TPaintStageEvent = procedure(Sender: TObject; Buffer: TBitmap32; StageNum: Cardinal) of object;

  { TPaintStage }
  TPaintStageMask = set of (
    psmDesignTime,              // Stage is painted at design-time
    psmRunTime,                 // Stage is painted at run-time
    psmExport                   // Stage is painted when exporting the image via PaintTo
  );

  TPaintStage = record
  private
    function GetDesignTime: boolean;
    function GetRunTime: boolean;
    procedure SetDesignTime(const Value: boolean);
    procedure SetRunTime(const Value: boolean);
  public
    Mask: TPaintStageMask;
    Stage: Cardinal;             // a PST_* constant
    Parameter: Cardinal;         // an optional parameter

    // Backward compatibility
    property DsgnTime: boolean read GetDesignTime write SetDesignTime;
    property RunTime: boolean read GetRunTime write SetRunTime;
  end;
  PPaintStage = ^TPaintStage;

  { TPaintStages }
  TPaintStages = class
  private
    FItems: array of TPaintStage;
    function GetItem(Index: Integer): PPaintStage;
  public
    destructor Destroy; override;
    function  Add: PPaintStage;
    procedure Clear;
    function  Count: Integer;
    procedure Delete(Index: Integer);
    function  Insert(Index: Integer): PPaintStage;
    property Items[Index: Integer]: PPaintStage read GetItem; default;
  end;

  { Alignment of the bitmap in TCustomImage32 }
  TBitmapAlign = (baTopLeft, baCenter, baTile, baCustom);
  TScaleMode = (smNormal, smStretch, smScale, smResize, smOptimal, smOptimalScaled);
  TPaintBoxOptions = set of (pboWantArrowKeys, pboAutoFocus);

  TRepaintMode = (rmFull, rmDirect, rmOptimizer);

  { TCustomPaintBox32 }
  TCustomPaintBox32 = class(TCustomControl)
  strict private
    FBuffer: TBitmap32;
    FBufferOversize: Integer;
    FBufferValid: Boolean;
    FRepaintMode: TRepaintMode;
    FInvalidRects: TRectList;
    FUpdateRects: TRectList;
    FForceFullRepaint: Boolean;
    FRepaintOptimizer: TCustomRepaintOptimizer;
    FOptions: TPaintBoxOptions;
    FUpdateCount: Integer;
    FLockUpdateCount: Integer;
    FModified: boolean;
    FMouseInControl: Boolean;
    FOnGDIOverlay: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnChange: TNotifyEvent;
    procedure SetBufferOversize(Value: Integer);
{$IFDEF FPC}
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
    procedure WMGetDlgCode(var Msg: TLMessage); message LM_GETDLGCODE;
    procedure WMPaint(var Message: TLMPaint); message LM_PAINT;
    procedure CMMouseEnter(var Message: TLMessage); message LM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TLMessage); message LM_MOUSELEAVE;
{$ELSE}
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Msg: TWmGetDlgCode); message WM_GETDLGCODE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
{$ENDIF}
  protected
    procedure BitmapChangeHandler(Sender: TObject);
    procedure DirectAreaUpdateHandler(Sender: TObject; const Area: TRect; const Info: Cardinal);
    procedure OptimizedAreaUpdateHandler(Sender: TObject; const Area: TRect; const Info: Cardinal);
  protected
    // IUpdateRectNotification
    procedure AreaUpdated(const AArea: TRect; const AInfo: Cardinal);
  protected
    procedure CreateBuffer; virtual;
    function CreateRepaintOptimizer(ABuffer: TBitmap32; AInvalidRects: TRectList): TCustomRepaintOptimizer; virtual;
    procedure RepaintModeChanged; virtual;
    procedure SetRepaintMode(const Value: TRepaintMode); virtual;
    function  CustomRepaint: Boolean; virtual;
    function  InvalidRectsAvailable: Boolean; virtual;
    procedure DoPrepareInvalidRects; virtual;
    procedure DoPaintBuffer; virtual;
    procedure DoPaintGDIOverlay; virtual;
    procedure DoBufferResized(const OldWidth, OldHeight: Integer); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter; {$IFDEF FPC} override; {$ELSE} virtual; {$ENDIF}
    procedure MouseLeave; {$IFDEF FPC} override; {$ELSE} virtual; {$ENDIF}
    procedure AssignTo(Dest: TPersistent); override;
    procedure Loaded; override;
    procedure Paint; override;
    procedure ResetInvalidRects;
    procedure ResizeBuffer;
    procedure DoChanged; virtual;
    property  RepaintOptimizer: TCustomRepaintOptimizer read FRepaintOptimizer;
    property  BufferValid: Boolean read FBufferValid write FBufferValid;
    property  InvalidRects: TRectList read FInvalidRects;
    property  UpdateRects: TRectList read FUpdateRects;
    property UpdateCount: Integer read FUpdateCount;
    property LockUpdateCount: Integer read FLockUpdateCount;
    property Modified: boolean read FModified;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BeginUpdate; {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure EndUpdate; {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure Changed; {$IFDEF USEINLINING} inline; {$ENDIF}

    procedure BeginLockUpdate; {$IFDEF USEINLINING} inline; {$ENDIF}
    procedure EndLockUpdate; {$IFDEF USEINLINING} inline; {$ENDIF}

    function  GetViewportRect: TRect; virtual;
    procedure Flush; overload;
    procedure Flush(const SrcRect: TRect); overload;
    procedure Invalidate; override;
    procedure ForceFullInvalidate; virtual;
    procedure Resize; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    property Buffer: TBitmap32 read FBuffer;
    property BufferOversize: Integer read FBufferOversize write SetBufferOversize;
    property Options: TPaintBoxOptions read FOptions write FOptions default [];
    property MouseInControl: Boolean read FMouseInControl;
    property RepaintMode: TRepaintMode read FRepaintMode write SetRepaintMode default rmFull;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnGDIOverlay: TNotifyEvent read FOnGDIOverlay write FOnGDIOverlay;
  end;

  { TPaintBox32 }
  TPaintBox32 = class(TCustomPaintBox32)
  strict private
    FOnPaintBuffer: TNotifyEvent;
  protected
    procedure DoPaintBuffer; override;
  public
    property Canvas;
  published
    property Align;
    property Anchors;
    property AutoSize;
    property Constraints;
    property Cursor;
    property DragCursor;
    property DragMode;
    property Options;
    property ParentShowHint;
    property PopupMenu;
    property RepaintMode;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
{$IFNDEF PLATFORM_INDEPENDENT}
    property OnCanResize;
{$ENDIF}
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnGDIOverlay;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPaintBuffer: TNotifyEvent read FOnPaintBuffer write FOnPaintBuffer;
    property OnResize;
    property OnStartDrag;
  end;

  { TCustomImage32 }
  TImgMouseEvent = procedure(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer) of object;
  TImgMouseMoveEvent = procedure(Sender: TObject; Shift: TShiftState;
    X, Y: Integer; Layer: TCustomLayer) of object;
  TPaintStageHandler = procedure(Dest: TBitmap32; StageNum: Integer) of object;

  TBackgroundCheckerStyle = (bcsCustom, bcsNone, bcsLight, bcsMedium, bcsDark);
  TBackgroundFillStyle = (bfsColor, bfsCheckers, bfsPattern);

  TBackgroundOptions = class(TNotifiablePersistent)
  private type
    TCheckersColors = array[0..1] of TColor32;
  strict private
    FPatternBitmap: TBitmap32;
    FOuterBorderColor: TColor;
    FInnerBorderWidth: integer;
    FInnerBorderColor: TColor;
    FDropShadowBitmap: TBitmap32;
    FDropShadowOffset: integer;
    FDropShadowSize: integer;
    FDropShadowColor: TColor32;
    FCheckersColors: TCheckersColors;
    FCheckersStyle: TBackgroundCheckerStyle;
    FCheckersExponent: integer;
    FFillStyle: TBackgroundFillStyle;
  protected
    procedure SetFillStyle(const Value: TBackgroundFillStyle);
    procedure SetPatternBitmap(const Value: TBitmap32);
    procedure SetDropShadowBitmap(const Value: TBitmap32);
    procedure SetDropShadowColor(const Value: TColor32);
    procedure SetDropShadowOffset(const Value: integer);
    procedure SetDropShadowSize(const Value: integer);
    procedure SetInnerBorderColor(const Value: TColor);
    procedure SetInnerBorderWidth(const Value: integer);
    procedure SetOuterBorderColor(const Value: TColor);
    procedure SetCheckersStyle(const Value: TBackgroundCheckerStyle);
    function GetCheckersColor(const Index: Integer): TColor;
    procedure SetCheckersColor(Index: integer; const Value: TColor);
    procedure SetCheckersExponent(const Value: integer);

    function IsFillStyleStored: Boolean;
    function IsCheckersColorsStored(Index: integer): boolean;
    function IsDropShadowBitmapStored: boolean;
    function IsPatternBitmapStored: boolean;

    procedure CheckFillStyle;

    procedure ChangeHandler(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;

    property CheckersColors: TCheckersColors read FCheckersColors;
  published
    property PatternBitmap: TBitmap32 read FPatternBitmap write SetPatternBitmap stored IsPatternBitmapStored;

    property OuterBorderColor: TColor read FOuterBorderColor write SetOuterBorderColor default clNone;

    property InnerBorderWidth: integer read FInnerBorderWidth write SetInnerBorderWidth default 0;
    property InnerBorderColor: TColor read FInnerBorderColor write SetInnerBorderColor default clNone;

    property DropShadowColor: TColor32 read FDropShadowColor write SetDropShadowColor default 0;
    property DropShadowOffset: integer read FDropShadowOffset write SetDropShadowOffset default 0;
    property DropShadowSize: integer read FDropShadowSize write SetDropShadowSize default 0;
    property DropShadowBitmap: TBitmap32 read FDropShadowBitmap write SetDropShadowBitmap stored IsDropShadowBitmapStored;

    property CheckersStyle: TBackgroundCheckerStyle read FCheckersStyle write SetCheckersStyle default bcsNone;
    property CheckersColorOdd: TColor index 0 read GetCheckersColor write SetCheckersColor stored IsCheckersColorsStored;
    property CheckersColorEven: TColor index 1 read GetCheckersColor write SetCheckersColor stored IsCheckersColorsStored;
    property CheckersExponent: integer read FCheckersExponent write SetCheckersExponent default 3;

    // Last property! We need it to be set last when loading from the DFM so the
    // fill style rules isn't applied against incomplete property values.
    property FillStyle: TBackgroundFillStyle read FFillStyle write SetFillStyle stored IsFillStyleStored;
  end;

  TMouseShiftState = set of (mssShift, mssAlt, mssCtrl); // Order must be same as TShiftState

  TMousePanOptions = class(TNotifiablePersistent)
  strict private
    FPanCursor: TCursor;
    FEnabled: boolean;
    FMouseButton: TMouseButton;
    FShiftState: TMouseShiftState;
  protected
  public
    constructor Create;

    function MatchShiftState(AShiftState: TShiftState): boolean;
  published
    property Enabled: boolean read FEnabled write FEnabled default False;
    property MouseButton: TMouseButton read FMouseButton write FMouseButton default mbLeft;
    property ShiftState: TMouseShiftState read FShiftState write FShiftState default [];
    property PanCursor: TCursor read FPanCursor write FPanCursor default crSizeAll;
  end;

  TMouseZoomOptions = class(TNotifiablePersistent)
  strict private
    FEnabled: boolean;
    FInvert: boolean;
    FMaintainPivot: boolean;
    FMinScale: Single;
    FMaxScale: Single;
    FSteps: integer;
    FZoomFactor: Double;
    FShiftState: TMouseShiftState;
    FAnimate: boolean;
  protected
    procedure SetMaxScale(const Value: Single);
    procedure SetMinScale(const Value: Single);
    procedure SetSteps(const Value: integer);
    procedure SetZoomFactor(const Value: Double);

    function IsMaxScaleStored: Boolean;
    function IsMinScaleStored: Boolean;

    procedure UpdateZoomFactor;
  public
    constructor Create;

    function ScaleToLevel(AScale: Single): integer;
    function LevelToScale(ALevel: integer): Single;
    function MatchShiftState(AShiftState: TShiftState): boolean;
  published
    property Enabled: boolean read FEnabled write FEnabled default False;
    property Invert: boolean read FInvert write FInvert default False;
    property ShiftState: TMouseShiftState read FShiftState write FShiftState default [];
    property MaintainPivot: boolean read FMaintainPivot write FMaintainPivot default True;
    property MinScale: Single read FMinScale write SetMinScale stored IsMinScaleStored;
    property MaxScale: Single read FMaxScale write SetMaxScale stored IsMaxScaleStored;
    property Steps: integer read FSteps write SetSteps default 12;
    property ZoomFactor: Double read FZoomFactor write SetZoomFactor stored False;
    property Animate: boolean read FAnimate write FAnimate default False;
  end;

  TCustomImage32 = class(TCustomPaintBox32, IUpdateRectNotification)
  strict private
    FBitmap: TBitmap32;
    FBitmapAlign: TBitmapAlign;
    FLayers: TLayerCollection;
    FOffsetHorz: TFloat;
    FOffsetVert: TFloat;
    FPaintStages: TPaintStages;
    FPaintStageHandlers: array of TPaintStageHandler;
    FPaintStageNum: array of Integer;
    FScaleX: TFloat;
    FScaleY: TFloat;
    FScaleMode: TScaleMode;
    FBackgroundOptions: TBackgroundOptions;
    FMousePanOptions: TMousePanOptions;
    FMouseZoomOptions: TMouseZoomOptions;
    FIsMousePanning: boolean;
    FMousePanStartPos: TPoint;
    FPartialRepaintQueued: boolean;
    FOnBitmapResize: TNotifyEvent;
    FOnInitStages: TNotifyEvent;
    FOnMouseDown: TImgMouseEvent;
    FOnMouseMove: TImgMouseMoveEvent;
    FOnMouseUp: TImgMouseEvent;
    FOnPaintStage: TPaintStageEvent;
    FOnScaleChange: TNotifyEvent;
    procedure BackgroundOptionsChangeHandler(Sender: TObject);
    procedure BitmapResizeHandler(Sender: TObject);
    procedure BitmapChangeHandler(Sender: TObject);
    procedure BitmapAreaChangeHandler(Sender: TObject; const Area: TRect; const Info: Cardinal);
    procedure BitmapDirectAreaChangeHandler(Sender: TObject; const Area: TRect; const Info: Cardinal);
    procedure LayerCollectionChangeHandler(Sender: TObject);
    procedure LayerCollectionGDIUpdateHandler(Sender: TObject);
    procedure LayerCollectionGetViewportScaleHandler(Sender: TObject; out ScaleX, ScaleY: TFloat);
    procedure LayerCollectionGetViewportShiftHandler(Sender: TObject; out ShiftX, ShiftY: TFloat);
    function  GetOnPixelCombine: TPixelCombineEvent;
    procedure SetBitmap(Value: TBitmap32);
    procedure SetBitmapAlign(Value: TBitmapAlign);
    procedure SetLayers(Value: TLayerCollection);
    procedure SetOffsetHorz(Value: TFloat);
    procedure SetOffsetVert(Value: TFloat);
    procedure SetScale(Value: TFloat);
    procedure SetScaleX(Value: TFloat);
    procedure SetScaleY(Value: TFloat);
    procedure SetOnPixelCombine(Value: TPixelCombineEvent);
    procedure SetBackgroundOptions(const Value: TBackgroundOptions);
    procedure SetMousePanOptions(const Value: TMousePanOptions);
    procedure SetMouseZoomOptions(const Value: TMouseZoomOptions);
  protected
    CachedBitmapRect: TRect;
    CachedShiftX, CachedShiftY,
    CachedScaleX, CachedScaleY,
    CachedRecScaleX, CachedRecScaleY: TFloat;
    CacheValid: Boolean;
    OldSzX, OldSzY: Integer;
    PaintToMode: Boolean;
  protected
    procedure CreateBuffer; override;
    procedure RepaintModeChanged; override;
    procedure DoBitmapResized; virtual;
    procedure BitmapResized; virtual;
    procedure BitmapChanged(const Area: TRect); reintroduce; virtual;
    function CanMousePan: boolean; virtual;
    function CanMouseZoom: boolean; virtual;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure DoInitStages; virtual;
    procedure DoPaintBuffer; override;
    procedure DoPaintGDIOverlay; override;
    procedure DoScaleChange; virtual;
    procedure InitDefaultStages; virtual;
    procedure InvalidateCache;
    function InvalidRectsAvailable: Boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); overload; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); overload; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); overload; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer); reintroduce; overload; virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer); reintroduce; overload; virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer); reintroduce; overload; virtual;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure MouseLeave; override;
    procedure SetScaleMode(Value: TScaleMode); virtual;
    procedure SetXForm(ShiftX, ShiftY, ScaleX, ScaleY: TFloat);
    procedure DoZoom(APivot: TFloatPoint; AScale: TFloat);
    procedure DoSetZoom(const APivot: TFloatPoint; AScale: TFloat);
    procedure DoSetPivot(const APivot: TFloatPoint); virtual;
    procedure UpdateCache; virtual;
    function GetLayerCollectionClass: TLayerCollectionClass; virtual;
    function CreateLayerCollection: TLayerCollection; virtual;
    procedure Loaded; override;
    procedure DoChanged; override;
  protected
    // IUpdateRectNotification
    procedure AreaUpdated(const AArea: TRect; const AInfo: Cardinal);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function  BitmapToControl(const APoint: TPoint): TPoint; overload;
    function  BitmapToControl(const APoint: TFloatPoint): TFloatPoint; overload;
    function  ControlToBitmap(const APoint: TPoint): TPoint;  overload;
    function  ControlToBitmap(const ARect: TRect): TRect;  overload;
    function  ControlToBitmap(const APoint: TFloatPoint): TFloatPoint; overload;

    procedure Update(const Rect: TRect); reintroduce; overload; virtual; deprecated 'Use Invalidate(Rect) instead';
    procedure Invalidate; overload; override;
    procedure Invalidate(const Rect: TRect); reintroduce; overload; virtual;

    procedure ExecBitmapFrame(Dest: TBitmap32; StageNum: Integer); virtual;   // PST_BITMAP_FRAME
    procedure ExecClearBuffer(Dest: TBitmap32; StageNum: Integer); virtual;   // PST_CLEAR_BUFFER
    procedure ExecClearBackgnd(Dest: TBitmap32; StageNum: Integer); virtual;  // PST_CLEAR_BACKGND
    procedure ExecControlFrame(Dest: TBitmap32; StageNum: Integer); virtual;  // PST_CONTROL_FRAME
    procedure ExecCustom(Dest: TBitmap32; StageNum: Integer); virtual;        // PST_CUSTOM
    procedure ExecDrawBitmap(Dest: TBitmap32; StageNum: Integer); virtual;    // PST_DRAW_BITMAP
    procedure ExecDrawLayers(Dest: TBitmap32; StageNum: Integer); virtual;    // PST_DRAW_LAYERS

    function  GetBitmapRect: TRect; virtual;
    function  GetBitmapSize: TSize; virtual;

    procedure PaintTo(Dest: TBitmap32; DestRect: TRect); virtual;
    procedure Resize; override;
    procedure SetupBitmap(DoClear: Boolean = False; ClearColor: TColor32 = $FF000000); virtual;
    procedure Scroll(Dx, Dy: Integer); overload;
    procedure Scroll(Dx, Dy: Single); overload; virtual;

    property Bitmap: TBitmap32 read FBitmap write SetBitmap;
    property BitmapAlign: TBitmapAlign read FBitmapAlign write SetBitmapAlign;
    property Canvas;
    property Layers: TLayerCollection read FLayers write SetLayers;
    property OffsetHorz: TFloat read FOffsetHorz write SetOffsetHorz;
    property OffsetVert: TFloat read FOffsetVert write SetOffsetVert;
    property PaintStages: TPaintStages read FPaintStages;
    property Scale: TFloat read FScaleX write SetScale;
    property ScaleX: TFloat read FScaleX write SetScaleX;
    property ScaleY: TFloat read FScaleY write SetScaleY;
    property ScaleMode: TScaleMode read FScaleMode write SetScaleMode;
    property Background: TBackgroundOptions read FBackgroundOptions write SetBackgroundOptions;
    property MousePan: TMousePanOptions read FMousePanOptions write SetMousePanOptions;
    property MouseZoom: TMouseZoomOptions read FMouseZoomOptions write SetMouseZoomOptions;
    property IsMousePanning: boolean read FIsMousePanning;

    property OnBitmapResize: TNotifyEvent read FOnBitmapResize write FOnBitmapResize;
    property OnBitmapPixelCombine: TPixelCombineEvent read GetOnPixelCombine write SetOnPixelCombine;
    property OnInitStages: TNotifyEvent read FOnInitStages write FOnInitStages;
    property OnMouseDown: TImgMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TImgMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TImgMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnPaintStage: TPaintStageEvent read FOnPaintStage write FOnPaintStage;
    property OnScaleChange: TNotifyEvent read FOnScaleChange write FOnScaleChange;
  end;

  TImage32 = class(TCustomImage32)
  published
    property Align;
    property Anchors;
    property AutoSize;
    property Bitmap;
    property BitmapAlign;
    property Color;
    property Constraints;
    property Cursor;
    property DragCursor;
    property DragMode;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property RepaintMode;
    property Scale;
    property ScaleMode;
    property Background;
    property MousePan;
    property MouseZoom;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnBitmapResize;
{$IFNDEF PLATFORM_INDEPENDENT}
    property OnCanResize;
{$ENDIF}
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnGDIOverlay;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnInitStages;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPaintStage;
    property OnResize;
    property OnStartDrag;
  end;

  TCustomImgView32 = class;

  TScrollBarVisibility = (svAlways, svHidden, svAuto);

  { TIVScrollProperties }
  TIVScrollProperties = class(TArrowBarAccess)
  private
    function GetIncrement: Integer;
    function GetSize: Integer;
    function GetVisibility: TScrollbarVisibility;
    procedure SetIncrement(Value: Integer);
    procedure SetSize(Value: Integer);
    procedure SetVisibility(const Value: TScrollbarVisibility);
  protected
    ImgView: TCustomImgView32;
  published
    property Increment: Integer read GetIncrement write SetIncrement default 8;
    property Size: Integer read GetSize write SetSize default 0;
    property Visibility: TScrollBarVisibility read GetVisibility write SetVisibility default svAlways;
  end;

  TSizeGripStyle = (sgAuto, sgNone, sgAlways);

  { TCustomImgView32 }
  TCustomImgView32 = class(TCustomImage32)
  strict private
    FCentered: Boolean;
    FScrollBarSize: Integer;
    FScrollBarVisibility: TScrollBarVisibility;
    FScrollBars: TIVScrollProperties;
    FSizeGrip: TSizeGripStyle;
    FOnScroll: TNotifyEvent;
    FOverSize: Integer;
    procedure SetCentered(Value: Boolean);
    procedure SetScrollBars(Value: TIVScrollProperties);
    procedure SetSizeGrip(Value: TSizeGripStyle);
    procedure SetOverSize(const Value: Integer);
  protected
    FScrollLock: integer;
    HScroll: TCustomRangeBar;
    VScroll: TCustomRangeBar;
    procedure AlignAll;
    procedure BitmapResized; override;
    procedure DoDrawSizeGrip(R: TRect);
    procedure DoScaleChange; override;
    function CanMousePan: boolean; override;
    procedure DoScroll; virtual;
    function  GetScrollBarsVisible: Boolean;
    function  GetScrollBarSize: Integer;
    function  GetSizeGripRect: TRect;
    function  IsSizeGripVisible: Boolean;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer); override;
    procedure Paint; override;
    procedure Recenter;
    procedure SetScaleMode(Value: TScaleMode); override;
    procedure DoSetPivot(const APivot: TFloatPoint); override;
    procedure ScrollHandler(Sender: TObject); virtual;
    procedure ScrollChangingHandler(Sender: TObject; ANewPosition: Single; var Handled: boolean);
    procedure UpdateImage; virtual;
    procedure UpdateScrollBars; virtual;
    // Properties for use by TIVScrollProperties
    // TODO : Replace by interface: IScrollPropertiesOwner
    property ScrollBarSize: Integer read FScrollBarSize write FScrollBarSize;
    property ScrollBarVisibility: TScrollBarVisibility read FScrollBarVisibility write FScrollBarVisibility;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function  GetViewportRect: TRect; override;
    procedure Loaded; override;
    procedure Resize; override;
    procedure ScrollToCenter(X, Y: Integer);
    procedure Scroll(Dx, Dy: Single); override;
    property Centered: Boolean read FCentered write SetCentered default True;
    property ScrollBars: TIVScrollProperties read FScrollBars write SetScrollBars;
    property SizeGrip: TSizeGripStyle read FSizeGrip write SetSizeGrip default sgAuto;
    property OverSize: Integer read FOverSize write SetOverSize;
    property OnScroll: TNotifyEvent read FOnScroll write FOnScroll;
  end;

  TImgView32 = class(TCustomImgView32)
    property Align;
    property Anchors;
    property AutoSize;
    property Bitmap;
    property BitmapAlign;
    property Centered;
    property Color;
    property Constraints;
    property Cursor;
    property DragCursor;
    property DragMode;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property RepaintMode;
    property Scale;
    property ScaleMode;
    property Background;
    property MousePan;
    property MouseZoom;
    property ScrollBars;
    property ShowHint;
    property SizeGrip;
    property OverSize;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnBitmapResize;
{$IFNDEF PLATFORM_INDEPENDENT}
    property OnCanResize;
{$ENDIF}
    property OnClick;
    property OnChange;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnGDIOverlay;
    property OnInitStages; 
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaintStage;
    property OnResize;
    property OnScroll;
    property OnStartDrag;
  end;

  { TBitmap32Item }
  { A bitmap container designed to be inserted into TBitmap32Collection }
  TBitmap32Item = class(TCollectionItem)
  private
    FBitmap: TBitmap32;
    procedure SetBitmap(ABitmap: TBitmap32);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Bitmap: TBitmap32 read FBitmap write SetBitmap;
  end;

  TBitmap32ItemClass = class of TBitmap32Item;

  { TBitmap32Collection }
  { A collection of TBitmap32Item objects }
  TBitmap32Collection = class(TCollection)
  private
    FOwner: TPersistent;
    function  GetItem(Index: Integer): TBitmap32Item;
    procedure SetItem(Index: Integer; Value: TBitmap32Item);
  protected
    function  GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TBitmap32ItemClass);
    function Add: TBitmap32Item;
    property Items[Index: Integer]: TBitmap32Item read GetItem write SetItem; default;
  end;

  { TBitmap32List }
  { A component that stores TBitmap32Collection }
  TBitmap32List = class(TComponent)
  private
    FBitmap32Collection: TBitmap32Collection;
    procedure SetBitmap(Index: Integer; Value: TBitmap32);
    function GetBitmap(Index: Integer): TBitmap32;
    procedure SetBitmap32Collection(Value: TBitmap32Collection);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Bitmap[Index: Integer]: TBitmap32 read GetBitmap write SetBitmap; default;
  published
    property Bitmaps: TBitmap32Collection read FBitmap32Collection write SetBitmap32Collection;
  end;

var
  DefaultCheckersColors: array[TBackgroundCheckerStyle] of TBackgroundOptions.TCheckersColors =
    (($FFFFFFFF, $FF000000),
     ($FFFFFFFF, $FFFFFFFF),
     ($FFFFFFFF, $FFEBEBEB),
     ($FFFFFFFF, $FFD0D0D0),
     ($FFFFFFFF, $FFB0B0B0));

  // Maximum duration of animated zoom
  ZoomAnimateTime: integer = 300; // mS
  // Time between each zoom step. 1000 / ZoomAnimateDeltaTime = frame rate
  ZoomAnimateDeltaTime: integer = 5; // mS

implementation

uses
  Math, TypInfo,
{$if defined(WINDOWS)}
  MMSystem,
{$ifend}
{$if defined(AnimatedZoom)}
  amEasing,
{$ifend}
  GR32_MicroTiles, GR32_Backends, GR32_XPThemes, GR32_LowLevel,
  GR32_Resamplers, GR32_Backends_Generic;

type
  TLayerAccess = class(TCustomLayer);
  TLayerCollectionAccess = class(TLayerCollection);
  TRangeBarAccess = class(TRangeBar);

const
  DefaultRepaintOptimizerClass: TCustomRepaintOptimizerClass = TMicroTilesRepaintOptimizer;

resourcestring
  RCStrInvalidStageIndex = 'Invalid stage index';

{ TPaintStage }

function TPaintStage.GetDesignTime: boolean;
begin
  Result := (psmDesignTime in Mask);
end;

function TPaintStage.GetRunTime: boolean;
begin
  Result := (psmRunTime in Mask);
end;

procedure TPaintStage.SetDesignTime(const Value: boolean);
begin
  if (Value) then
    Include(Mask, psmDesignTime)
  else
    Exclude(Mask, psmDesignTime);
end;

procedure TPaintStage.SetRunTime(const Value: boolean);
begin
  if (Value) then
    Include(Mask, psmRunTime)
  else
    Exclude(Mask, psmRunTime);
end;

{ TPaintStages }

function TPaintStages.Add: PPaintStage;
var
  L: Integer;
begin
  L := Length(FItems);
  SetLength(FItems, L + 1);
  Result := @FItems[L];
  with Result^ do
  begin
    Mask := [psmRunTime, psmExport];
    Stage := 0;
    Parameter := 0;
  end;
end;

procedure TPaintStages.Clear;
begin
  FItems := nil;
end;

function TPaintStages.Count: Integer;
begin
  Result := Length(FItems);
end;

procedure TPaintStages.Delete(Index: Integer);
var
  LCount: Integer;
begin
  if (Index < 0) or (Index > High(FItems)) then
    raise EListError.Create(RCStrInvalidStageIndex);
  LCount := Length(FItems) - Index - 1;
  if LCount > 0 then
    Move(FItems[Index + 1], FItems[Index], LCount * SizeOf(TPaintStage));
  SetLength(FItems, High(FItems));
end;

destructor TPaintStages.Destroy;
begin
  Clear;
  inherited;
end;

function TPaintStages.GetItem(Index: Integer): PPaintStage;
begin
  Result := @FItems[Index];
end;

function TPaintStages.Insert(Index: Integer): PPaintStage;
var
  LCount: Integer;
begin
  if Index < 0 then
    Index := 0
  else
  if Index > Length(FItems) then
    Index := Length(FItems);

  LCount := Length(FItems) - Index;
  SetLength(FItems, Length(FItems) + 1);
  if LCount > 0 then
    Move(FItems[Index], FItems[Index + 1], LCount * SizeOf(TPaintStage));
  Result := @FItems[Index];
  with Result^ do
  begin
    Mask := [psmRunTime, psmExport];
    Stage := 0;
    Parameter := 0;
  end;
end;


{ TCustomPaintBox32 }

constructor TCustomPaintBox32.Create(AOwner: TComponent);
begin
  inherited;

  CreateBuffer;

  FBufferOversize := 40;
  FForceFullRepaint := True;
  FInvalidRects := TRectList.Create;
  FUpdateRects := TRectList.Create;
  FRepaintOptimizer := CreateRepaintOptimizer(Buffer, FInvalidRects);

  // RepaintModeChanged hooks up the bitmap event handlers according to the
  // repaint mode.
  RepaintModeChanged;

  { Setting a initial size here will cause the control to crash under LCL }
{$IFNDEF FPC}
  SetBounds(0, 0, 192, 192);
{$ENDIF}
end;

destructor TCustomPaintBox32.Destroy;
begin
  FreeAndNil(FRepaintOptimizer);
  FreeAndNil(FInvalidRects);
  FreeAndNil(FUpdateRects);
  FreeAndNil(FBuffer);
  inherited;
end;

procedure TCustomPaintBox32.CreateBuffer;
begin
  FBuffer := TBitmap32.Create;
end;

function TCustomPaintBox32.CreateRepaintOptimizer(ABuffer: TBitmap32;
  AInvalidRects: TRectList): TCustomRepaintOptimizer;
begin
  Result := DefaultRepaintOptimizerClass.Create(ABuffer, AInvalidRects);
end;

procedure TCustomPaintBox32.BeginUpdate;
begin
  // Defer OnChange notifications
  Inc(FUpdateCount);
end;

procedure TCustomPaintBox32.EndUpdate;
begin
  Assert(FUpdateCount > 0, 'Unpaired EndUpdate call');
  // Re-enable OnChange generation
  if (FUpdateCount = 1) then
  begin
    if (FModified) then
    begin
      DoChanged;
      FModified := False;
    end;
  end;

  Dec(FUpdateCount);
end;

procedure TCustomPaintBox32.BeginLockUpdate;
begin
  Inc(FLockUpdateCount);
end;

procedure TCustomPaintBox32.EndLockUpdate;
begin
  Assert(FLockUpdateCount > 0, 'Unpaired UnlockUpdate call');
  Dec(FLockUpdateCount);
end;

procedure TCustomPaintBox32.Changed;
begin
  if (FLockUpdateCount > 0) then
    exit;
  BeginUpdate;
  FModified := True;
  EndUpdate;
end;

procedure TCustomPaintBox32.DoChanged;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCustomPaintBox32.AreaUpdated(const AArea: TRect; const AInfo: Cardinal);
var
  UpdateRectSupport: IUpdateRectSupport;
begin
  if (Supports(FBuffer.Backend, IUpdateRectSupport, UpdateRectSupport)) then
    UpdateRectSupport.InvalidateRect(Self, AArea)
  else
    inherited Invalidate;
end;

procedure TCustomPaintBox32.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if Dest is TCustomPaintBox32 then
  begin
    FBuffer.Assign(TCustomPaintBox32(Dest).FBuffer);
    TCustomPaintBox32(Dest).FBufferOversize := FBufferOversize;
    TCustomPaintBox32(Dest).FBufferValid := FBufferValid;
    TCustomPaintBox32(Dest).FRepaintMode := FRepaintMode;
    TCustomPaintBox32(Dest).FInvalidRects := FInvalidRects;
    TCustomPaintBox32(Dest).FForceFullRepaint := FForceFullRepaint;
    TCustomPaintBox32(Dest).FOptions := FOptions;
    TCustomPaintBox32(Dest).FOnGDIOverlay := FOnGDIOverlay;
    TCustomPaintBox32(Dest).FOnMouseEnter := FOnMouseEnter;
    TCustomPaintBox32(Dest).FOnMouseLeave := FOnMouseLeave;
  end;
end;

procedure TCustomPaintBox32.CMMouseEnter(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
  inherited;
  MouseEnter;
end;

procedure TCustomPaintBox32.CMMouseLeave(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
  MouseLeave;
  inherited;
end;

procedure TCustomPaintBox32.DoBufferResized(const OldWidth, OldHeight: Integer);
begin
  if FRepaintOptimizer.Enabled then
    FRepaintOptimizer.BufferResizedHandler(FBuffer.Width, FBuffer.Height);
end;

function TCustomPaintBox32.CustomRepaint: Boolean;
begin
  Result := FRepaintOptimizer.Enabled and not FForceFullRepaint and
    FRepaintOptimizer.UpdatesAvailable;
end;

procedure TCustomPaintBox32.DoPrepareInvalidRects;
begin
  if FRepaintOptimizer.Enabled and not FForceFullRepaint then
    FRepaintOptimizer.PerformOptimization;
end;

function TCustomPaintBox32.InvalidRectsAvailable: Boolean;
begin
  Result := True;
end;

procedure TCustomPaintBox32.DoPaintBuffer;
begin
  // force full repaint, this is necessary when Buffer is invalid and was never painted
  // This will omit calculating the invalid rects, thus we paint everything.
  if FForceFullRepaint then
  begin
    FForceFullRepaint := False;
    FInvalidRects.Clear;
  end
  else
    DoPrepareInvalidRects;

  // descendants should override this method for painting operations,
  // not the Paint method!!!
  FBufferValid := True;
end;

procedure TCustomPaintBox32.DoPaintGDIOverlay;
begin
  if Assigned(FOnGDIOverlay) then
    FOnGDIOverlay(Self);
end;

procedure TCustomPaintBox32.Flush;
begin
  if (FBuffer.Handle <> 0) then
  begin
    Canvas.Lock;
    try
      FBuffer.Lock;
      try
        if (Canvas.Handle <> 0) then
          with GetViewportRect do
            BitBlt(Canvas.Handle, Left, Top, Right - Left, Bottom - Top,
              FBuffer.Handle, 0, 0, SRCCOPY);
      finally
        FBuffer.Unlock;
      end;
    finally
      Canvas.Unlock;
    end;
  end;
end;

procedure TCustomPaintBox32.Flush(const SrcRect: TRect);
var
  R: TRect;
begin
  if (FBuffer.Handle <> 0) then
  begin
    Canvas.Lock;
    try
      FBuffer.Lock;
      try
        R := GetViewPortRect;
        if (Canvas.Handle <> 0) then
          with SrcRect do
            BitBlt(Canvas.Handle, Left + R.Left, Top + R.Top, Right - Left,
              Bottom - Top, FBuffer.Handle, Left, Top, SRCCOPY);
      finally
        FBuffer.Unlock;
      end;
    finally
      Canvas.Unlock;
    end;
  end;
end;

function TCustomPaintBox32.GetViewportRect: TRect;
begin
  // returns position of the buffered area within the control bounds
  // by default, the whole control is buffered
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := Width;
  Result.Bottom := Height;
end;

procedure TCustomPaintBox32.Invalidate;
begin
  FBufferValid := False;
{$if defined(COMPILERFPC) and defined(WINDOWS)}
  // LCL TWinControl.Invalidate doesn't take csOpaque in account when calling InvalidateRect.
  if (HandleAllocated) then
    InvalidateRect(Handle, nil, not(csOpaque in ControlStyle));
{$else}
  inherited;
{$ifend}
end;

procedure TCustomPaintBox32.ForceFullInvalidate;
begin
  if FRepaintOptimizer.Enabled then
    FRepaintOptimizer.Reset;
  FForceFullRepaint := True;
  Invalidate;
end;

procedure TCustomPaintBox32.Loaded;
begin
  ResizeBuffer;
  FBufferValid := False;
  inherited;
end;

procedure TCustomPaintBox32.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (pboAutoFocus in Options) and CanFocus then
    SetFocus;
  inherited;
end;

procedure TCustomPaintBox32.MouseEnter;
begin
  FMouseInControl := True;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TCustomPaintBox32.MouseLeave;
begin
  FMouseInControl := False;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TCustomPaintBox32.Paint;
var
  PaintSupport: IPaintSupport;
begin
  if (Parent = nil) then
    Exit;

  if FRepaintOptimizer.Enabled then
    FRepaintOptimizer.BeginPaint;

  PaintSupport := FBuffer.Backend as IPaintSupport;

  if not FBufferValid then
  begin
    PaintSupport.ImageNeeded;
    DoPaintBuffer;
    PaintSupport.CheckPixmap;
  end;

  FBuffer.Lock;
  try
    PaintSupport.DoPaint(FBuffer, FUpdateRects, Canvas, Self);
  finally
    FBuffer.Unlock;
  end;

{-$define DEBUG_UPDATERECTS}
{$ifdef DEBUG_UPDATERECTS} // See issue # 202
  Canvas.Brush.Color := clRed;
  Canvas.Brush.Style := bsSolid;
  for var i := 0 to FUpdateRects.Count-1 do
    Canvas.FrameRect(FUpdateRects[i]^);
{$endif DEBUG_UPDATERECTS}

  DoPaintGDIOverlay;

  if FRepaintOptimizer.Enabled then
    FRepaintOptimizer.EndPaint;

  ResetInvalidRects;
  FForceFullRepaint := False;
end;

procedure TCustomPaintBox32.ResetInvalidRects;
begin
  FInvalidRects.Clear;
end;

procedure TCustomPaintBox32.Resize;
begin
  if (not (csLoading in ComponentState)) then
    ResizeBuffer;
  BufferValid := False;
  inherited;
end;

procedure TCustomPaintBox32.ResizeBuffer;
var
  NewWidth, NewHeight, W, H: Integer;
  OldWidth, OldHeight: Integer;
begin
  // get the viewport parameters
  with GetViewportRect do
  begin
    NewWidth := Right - Left;
    NewHeight := Bottom - Top;
  end;
  if NewWidth < 0 then
    NewWidth := 0;
  if NewHeight < 0 then
    NewHeight := 0;

  W := FBuffer.Width;

  if NewWidth > W then
    W := NewWidth + FBufferOversize
  else
  if NewWidth < W - FBufferOversize then
    W := NewWidth;

  if W < 1 then
    W := 1;

  H := FBuffer.Height;

  if NewHeight > H then
    H := NewHeight + FBufferOversize
  else
  if NewHeight < H - FBufferOversize then
    H := NewHeight;

  if H < 1 then
    H := 1;

  if (W <> FBuffer.Width) or (H <> FBuffer.Height) then
  begin
    FBuffer.Lock;
    OldWidth := Buffer.Width;
    OldHeight := Buffer.Height;
    FBuffer.SetSize(W, H);
    FBuffer.Unlock;

    DoBufferResized(OldWidth, OldHeight);
    ForceFullInvalidate;
  end;
end;

procedure TCustomPaintBox32.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if (not (csLoading in ComponentState)) then
    ResizeBuffer;
  FBufferValid := False;
end;

procedure TCustomPaintBox32.SetBufferOversize(Value: Integer);
begin
  if (Value < 0) then
    Value := 0;

  if (Value <> FBufferOversize) then
  begin
    FBufferOversize := Value;
    ResizeBuffer;
    FBufferValid := False
  end;
end;

procedure TCustomPaintBox32.WMEraseBkgnd(var Message: {$IFDEF FPC}TLmEraseBkgnd{$ELSE}TWmEraseBkgnd{$ENDIF});
begin
  Message.Result := 1;
end;

procedure TCustomPaintBox32.WMGetDlgCode(var Msg: {$IFDEF FPC}TLMessage{$ELSE}TWmGetDlgCode{$ENDIF});
begin
  if (pboWantArrowKeys in Options) then
    Msg.Result:= Msg.Result or DLGC_WANTARROWS
  else
    Msg.Result:= Msg.Result and not DLGC_WANTARROWS;
end;

procedure TCustomPaintBox32.WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF});
var
  FullRepaint: boolean;
  UpdateRectSupport: IUpdateRectSupport;
  i: integer;
  Tiles: TMicroTiles;
begin
  // Update the InvalidRects
  if CustomRepaint then
    DoPrepareInvalidRects;

  // Get a list of update rects
  if (Supports(FBuffer.Backend, IUpdateRectSupport, UpdateRectSupport)) then
  begin
    FullRepaint := False;
    UpdateRectSupport.GetUpdateRects(Self, FUpdateRects, FInvalidRects.Count, FullRepaint);
  end else
    FullRepaint := True;

  if (not FullRepaint) then
  begin
    // Merge FInvalidRects into FUpdateRects
    for i := 0 to FInvalidRects.Count-1 do
      FUpdateRects.Add(FInvalidRects[i]^);

    // Consolidate potentially overlapping areas into as few separate
    // non-overlapping areas as possible.
{$ifdef CONSOLIDATE_UPDATERECTS} // See issue # 202
    MicroTilesCreate(Tiles);
    MicroTilesSetSize(Tiles, ClientRect);
    for i := 0 to FUpdateRects.Count-1 do
      MicroTilesAddRect(Tiles, FUpdateRects[i]^, True);
    FUpdateRects.Count := 0;
    MicroTilesCalcRects(Tiles, FUpdateRects, False, True);
    MicroTilesDestroy(Tiles);
{$endif CONSOLIDATE_UPDATERECTS}
  end;

  FullRepaint := FullRepaint or ((FUpdateRects.Count = 1) and (GR32.EqualRect(FUpdateRects[0]^, ClientRect)));
  if (FullRepaint) then
    FUpdateRects.Count := 0;

{$IFDEF FPC}
  { On FPC we need to specify the name of the ancestor here }
  inherited WMPaint(Message);
{$ELSE}
  inherited;
{$ENDIF}

  FUpdateRects.Count := 0;
end;

procedure TCustomPaintBox32.BitmapChangeHandler(Sender: TObject);
begin
  FRepaintOptimizer.Reset;
  // Request that everything be repainted
  inherited Invalidate;;
end;

procedure TCustomPaintBox32.DirectAreaUpdateHandler(Sender: TObject; const Area: TRect; const Info: Cardinal);
begin
  // Request that the area be repainted...
  AreaUpdated(Area, Info);

  if not(csCustomPaint in ControlState) then
    // ...and process pending updates
    Update;
end;

procedure TCustomPaintBox32.OptimizedAreaUpdateHandler(Sender: TObject; const Area: TRect; const Info: Cardinal);
var
  UpdateRectNotification: IUpdateRectNotification;
begin
  // Add the area to the optimizer
  if (FRepaintOptimizer.Enabled) and (Supports(FRepaintOptimizer, IUpdateRectNotification, UpdateRectNotification)) then
    UpdateRectNotification.AreaUpdated(Area, Info);

  // Request that the area be repainted
  AreaUpdated(Area, Info);
end;

procedure TCustomPaintBox32.RepaintModeChanged;
begin
  // Setup event handler on change of area
  if (FBuffer <> nil) then
  begin
    case FRepaintMode of
      rmOptimizer:
        begin
          FBuffer.OnAreaChanged := OptimizedAreaUpdateHandler;
          FBuffer.OnChange := nil;
        end;

      rmDirect:
        begin
          FBuffer.OnAreaChanged := DirectAreaUpdateHandler;
          FBuffer.OnChange := nil;
        end;

      rmFull:
        begin
          FBuffer.OnAreaChanged := nil;
          FBuffer.OnChange := BitmapChangeHandler;
        end
    end;
  end;
end;

procedure TCustomPaintBox32.SetRepaintMode(const Value: TRepaintMode);
begin
  if (Value <> FRepaintMode) then
  begin
    FRepaintMode := Value;

    if (FRepaintOptimizer <> nil) then
      FRepaintOptimizer.Enabled := (FRepaintMode = rmOptimizer);

    // Update buffer event handlers according to repaint mode
    RepaintModeChanged;

    Invalidate;
  end;
end;


{ TPaintBox32 }

procedure TPaintBox32.DoPaintBuffer;
begin
  if Assigned(FOnPaintBuffer) then
    FOnPaintBuffer(Self);
  inherited;
end;


{ TBackgroundOptions }

procedure TBackgroundOptions.ChangeHandler(Sender: TObject);
begin
  CheckFillStyle;
  Changed;
end;

constructor TBackgroundOptions.Create;
begin
  inherited Create;

  FPatternBitmap := TBitmap32.Create(TMemoryBackend);
  FPatternBitmap.DrawMode := dmOpaque;
  FPatternBitmap.OnChange := ChangeHandler;

  FDropShadowBitmap := TBitmap32.Create(TMemoryBackend);
  FDropShadowBitmap.DrawMode := dmBlend;
  FDropShadowBitmap.OnChange := ChangeHandler;

  FOuterBorderColor := clNone;
  FInnerBorderColor := clNone;
  SetCheckersStyle(bcsNone); // We need to go via the property setter
  FCheckersExponent := 3;
end;

destructor TBackgroundOptions.Destroy;
begin
  FPatternBitmap.Free;
  FDropShadowBitmap.Free;
  inherited;
end;

procedure TBackgroundOptions.CheckFillStyle;
begin
  case FFillStyle of
    bfsColor:
      if (not FPatternBitmap.Empty) then
        FFillStyle := bfsPattern;

    bfsCheckers:
      if (not FPatternBitmap.Empty) then
        FFillStyle := bfsPattern
      else
      if (FCheckersStyle = bcsNone) then
        FFillStyle := bfsColor
      else
      if (FInnerBorderColor <> clNone) and (FInnerBorderWidth <> 0) then
        FFillStyle := bfsColor
      else
      if (FOuterBorderColor <> clNone) then
        FFillStyle := bfsColor
      else
      if (not FDropShadowBitmap.Empty) or (FDropShadowSize <> 0) then
        FFillStyle := bfsColor;

    bfsPattern:
      if (FPatternBitmap.Empty) then
        FFillStyle := bfsColor;
  end;
end;

function TBackgroundOptions.GetCheckersColor(const Index: Integer): TColor;
begin
  Result := WinColor(FCheckersColors[Index]);
end;

function TBackgroundOptions.IsCheckersColorsStored(Index: integer): boolean;
begin
  Result := (FCheckersStyle = bcsCustom);
end;

function TBackgroundOptions.IsDropShadowBitmapStored: boolean;
begin
  Result := (not FDropShadowBitmap.Empty);
end;

function TBackgroundOptions.IsFillStyleStored: Boolean;
begin
  case FFillStyle of
    bfsColor:
      Result := (FCheckersStyle <> bcsNone) and
        ((FInnerBorderColor = clNone) or (FInnerBorderWidth = 0)) and
        (FOuterBorderColor = clNone) and
        (FDropShadowBitmap.Empty) and (FDropShadowSize = 0);

    bfsCheckers:
      Result := True;

    bfsPattern:
      Result := False;
  else
    Result := True;
  end;
end;

function TBackgroundOptions.IsPatternBitmapStored: boolean;
begin
  Result := (not FPatternBitmap.Empty);
end;

procedure TBackgroundOptions.SetPatternBitmap(const Value: TBitmap32);
begin
  FPatternBitmap.Assign(Value);
end;

procedure TBackgroundOptions.SetCheckersColor(Index: integer; const Value: TColor);
begin
  if (FCheckersStyle <> bcsCustom) or (Color32(Value) <> FCheckersColors[Index]) then
  begin
    FCheckersColors[Index] := Color32(Value);
    FCheckersStyle := bcsCustom;
    Changed;
  end;
end;

procedure TBackgroundOptions.SetCheckersExponent(const Value: integer);
begin
  if (Value <> FCheckersExponent) then
  begin;
    // There's no technical reason to limit the size, but there's also no
    // practical reason to allow larger values.
    FCheckersExponent := Min(10, Max(0, Value));
    Changed;
  end;
end;

procedure TBackgroundOptions.SetCheckersStyle(const Value: TBackgroundCheckerStyle);
begin
  if (FCheckersStyle <> Value) then
  begin
    FCheckersStyle := Value;

    if (FCheckersStyle <> bcsCustom) then
      FCheckersColors := DefaultCheckersColors[FCheckersStyle];

    CheckFillStyle;

    Changed;
  end;
end;

procedure TBackgroundOptions.SetDropShadowBitmap(const Value: TBitmap32);
begin
  FDropShadowBitmap.Assign(Value);
end;

procedure TBackgroundOptions.SetDropShadowColor(const Value: TColor32);
begin
  if (Value <> FDropShadowColor) then
  begin
    FDropShadowColor := Value;
    CheckFillStyle;
    Changed;
  end;
end;

procedure TBackgroundOptions.SetDropShadowOffset(const Value: integer);
begin
  if (Value <> FDropShadowOffset) then
  begin
    FDropShadowOffset := Max(0, Value);
    Changed;
  end;
end;

procedure TBackgroundOptions.SetDropShadowSize(const Value: integer);
begin
  if (Value <> FDropShadowSize) then
  begin
    FDropShadowSize := Max(0, Value);
    Changed;
  end;
end;

procedure TBackgroundOptions.SetFillStyle(const Value: TBackgroundFillStyle);
begin
  if (Value <> FFillStyle) then
  begin
    FFillStyle := Value;
    CheckFillStyle;
    Changed;
  end;
end;

procedure TBackgroundOptions.SetInnerBorderColor(const Value: TColor);
begin
  if (Value <> FInnerBorderColor) then
  begin
    FInnerBorderColor := Value;
    CheckFillStyle;
    Changed;
  end;
end;

procedure TBackgroundOptions.SetInnerBorderWidth(const Value: integer);
begin
  if (Value <> FInnerBorderWidth) then
  begin
    FInnerBorderWidth := Max(0, Value);
    CheckFillStyle;
    Changed;
  end;
end;

procedure TBackgroundOptions.SetOuterBorderColor(const Value: TColor);
begin
  if (Value <> FOuterBorderColor) then
  begin
    FOuterBorderColor := Value;
    CheckFillStyle;
    Changed;
  end;
end;

{ TCustomImage32 }

constructor TCustomImage32.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents, csDoubleClicks, csReplicatable, csOpaque];

  FLayers := CreateLayerCollection;
  FLayers.Subscribe(Self);

  RepaintOptimizer.RegisterLayerCollection(FLayers);

  FPaintStages := TPaintStages.Create;
  FScaleX := 1;
  FScaleY := 1;
  SetXForm(0, 0, 1, 1);

  FBackgroundOptions := TBackgroundOptions.Create;
  FBackgroundOptions.OnChange := BackgroundOptionsChangeHandler;

  FMousePanOptions := TMousePanOptions.Create;
  FMouseZoomOptions := TMouseZoomOptions.Create;

  InitDefaultStages;
end;

destructor TCustomImage32.Destroy;
begin
  BeginLockUpdate; // Block further notifications

  Bitmap.OnResize := nil;

  FreeAndNil(FPaintStages);
  RepaintOptimizer.UnregisterLayerCollection(FLayers);
  FLayers.Unsubscribe(Self);
  FreeAndNil(FLayers);
  FreeAndNil(FBitmap);
  FreeAndNil(FBackgroundOptions);
  FreeAndNil(FMousePanOptions);
  FreeAndNil(FMouseZoomOptions);
  inherited;
end;

procedure TCustomImage32.CreateBuffer;
begin
  inherited;

  FBitmap := TBitmap32.Create;
  FBitmap.OnResize := BitmapResizeHandler;
end;

procedure TCustomImage32.RepaintModeChanged;
begin
  // Beware! This is called from TCustomPaintBox32.Create

  // Note: We don't really need to call inherited here since we don't want the
  // paintbox buffer event handlers set. However, since we're supressing the
  // buffer change events in derived classes with BeginUpdate/EndUpdate there's
  // no harm in doing it.
  inherited;

  if (FBitmap <> nil) then
  begin
    case RepaintMode of
      rmOptimizer:
        begin
          FBitmap.OnAreaChanged := BitmapAreaChangeHandler;
          FBitmap.OnChange := nil;
        end;

      rmDirect:
        begin
          FBitmap.OnAreaChanged := BitmapDirectAreaChangeHandler;
          FBitmap.OnChange := nil;
        end;

      rmFull:
        begin
          FBitmap.OnAreaChanged := nil;
          FBitmap.OnChange := BitmapChangeHandler;
        end;
    end;
  end;
end;

function TCustomImage32.GetLayerCollectionClass: TLayerCollectionClass;
begin
  Result := TLayerCollection;
end;

function TCustomImage32.CreateLayerCollection: TLayerCollection;
begin
  Result := GetLayerCollectionClass.Create(Self);

  TLayerCollectionAccess(Result).OnChange := LayerCollectionChangeHandler;
  TLayerCollectionAccess(Result).OnGDIUpdate := LayerCollectionGDIUpdateHandler;
  TLayerCollectionAccess(Result).OnGetViewportScale := LayerCollectionGetViewportScaleHandler;
  TLayerCollectionAccess(Result).OnGetViewportShift := LayerCollectionGetViewportShiftHandler;
end;

procedure TCustomImage32.AreaUpdated(const AArea: TRect; const AInfo: Cardinal);
var
  UpdateRectSupport: IUpdateRectSupport;
begin
  if (Supports(Bitmap.Backend, IUpdateRectSupport, UpdateRectSupport)) then
  begin
    FPartialRepaintQueued := True;
    UpdateRectSupport.InvalidateRect(Self, AArea);
  end else
    Invalidate;

  BufferValid := False;
end;

procedure TCustomImage32.DoChanged;
begin
  // If partial repaints hasn't been queued then we need to do a full repaint
  if (not FPartialRepaintQueued) then
    Invalidate;

  inherited;
end;

procedure TCustomImage32.DoBitmapResized;
begin
  if Assigned(FOnBitmapResize) then
    FOnBitmapResize(Self);
end;

procedure TCustomImage32.BitmapResized;
var
  W, H: Integer;
begin
  if AutoSize then
  begin
    W := Bitmap.Width;
    H := Bitmap.Height;

    if (ScaleMode = smScale) then
    begin
      W := Round(W * Scale);
      H := Round(H * Scale);
    end;

    if AutoSize and (W > 0) and (H > 0) then
      SetBounds(Left, Top, W, H);
  end;

  if (UpdateCount = 0) then
    DoBitmapResized;

  InvalidateCache;
  ForceFullInvalidate;
end;

procedure TCustomImage32.BitmapChanged(const Area: TRect);
begin
  Changed;
end;

function TCustomImage32.BitmapToControl(const APoint: TPoint): TPoint;
begin
  // convert coordinates from bitmap's ref. frame to control's ref. frame
  UpdateCache;
  with APoint do
  begin
    Result.X := Trunc(X * CachedScaleX + CachedShiftX);
    Result.Y := Trunc(Y * CachedScaleY + CachedShiftY);
  end;
end;

function TCustomImage32.BitmapToControl(const APoint: TFloatPoint): TFloatPoint;
begin
  // subpixel precision version
  UpdateCache;
  with APoint do
  begin
    Result.X := X * CachedScaleX + CachedShiftX;
    Result.Y := Y * CachedScaleY + CachedShiftY;
  end;
end;

procedure TCustomImage32.BackgroundOptionsChangeHandler(Sender: TObject);
begin
  Invalidate;
end;

procedure TCustomImage32.BitmapResizeHandler(Sender: TObject);
begin
  BitmapResized;
end;

procedure TCustomImage32.BitmapChangeHandler(Sender: TObject);
begin
  RepaintOptimizer.Reset;
  BitmapChanged(Bitmap.Boundsrect);
end;

procedure TCustomImage32.BitmapAreaChangeHandler(Sender: TObject;
  const Area: TRect; const Info: Cardinal);
var
  NewInfo: Cardinal;
  T, R: TRect;
  Width, Tx, Ty, I, J: Integer;
  OffsetX, OffsetY: Integer;
  WidthX, WidthY: Integer;
  UpdateRectNotification: IUpdateRectNotification;
begin
  if Sender = FBitmap then
  begin
    T := Area;

    UpdateCache; // Ensure CachedScaleXY is up to date
    NewInfo := Info;
    if (NewInfo and AREAINFO_LINE <> 0) then
    begin
      if (T.Left = T.Right) and (T.Top = T.Bottom) then
        Exit; // Zero length line

      // Unpack line width from Info param
      Width := integer(NewInfo and (not AREAINFO_MASK));

      // Add line and resampler width and scale value to viewport
      Width := Ceil((Width + FBitmap.Resampler.Width) * CachedScaleX);

      // Pack width into Info param again
      NewInfo := AREAINFO_LINE or Width;
    end else
    if (T.Left = T.Right) or (T.Top = T.Bottom) then
      Exit; // Empty rect

    // Make sure rect is positive (i.e. dX >= 0)
    if (T.Left > T.Right) then
    begin
      Swap(T.Left, T.Right);
      Swap(T.Top, T.Bottom);
    end;

    // Translate the coordinates from bitmap to viewport
    T.TopLeft := BitmapToControl(T.TopLeft);
    T.BottomRight := BitmapToControl(T.BottomRight);

    if (NewInfo and AREAINFO_LINE <> 0) then
    begin
      // Line coordinates specify the center of the pixel.
      // For example the rect (0, 0, 0, 1) is a one pixel long line while (0, 0, 0, 0) is empty.
      OffsetX := Round(CachedScaleX / 2);
      OffsetY := Round(CachedScaleY / 2);
      GR32.OffsetRect(T, OffsetX, OffsetY);
    end else
    begin
      // Rect coordinates specify the pixel corners.
      // It is assumed that (Top, Left) specify the top/left corner of the top/left pixel and
      // that (Right, Bottom) specify the bottom/right corner of the bottom/right pixel.
      // For example the rect (0, 0, 1, 1) covers just one pixel while (0, 0, 0, 1) is empty.
      Dec(T.Right);
      Dec(T.Bottom);

      WidthX := Ceil(FBitmap.Resampler.Width * CachedScaleX);
      WidthY := Ceil(FBitmap.Resampler.Width * CachedScaleY);

      InflateArea(T, WidthX, WidthY);
    end;

    if (RepaintOptimizer.Enabled) and (Supports(RepaintOptimizer, IUpdateRectNotification, UpdateRectNotification)) then
    begin
      if FBitmapAlign <> baTile then
        UpdateRectNotification.AreaUpdated(T, NewInfo)
      else
      begin
        with CachedBitmapRect do
        begin
          Tx := Buffer.Width div Right;
          Ty := Buffer.Height div Bottom;
          for J := 0 to Ty do
            for I := 0 to Tx do
            begin
              R := T;
              GR32.OffsetRect(R, Right * I, Bottom * J);
              UpdateRectNotification.AreaUpdated(R, NewInfo);
            end;
        end;
      end;
    end;
  end;

  BitmapChanged(Area);
end;

procedure TCustomImage32.BitmapDirectAreaChangeHandler(Sender: TObject;
  const Area: TRect; const Info: Cardinal);
var
  T, R: TRect;
  Width, Tx, Ty, I, J: Integer;
begin
  if Sender = FBitmap then
  begin
    T := Area;
    Width := Trunc(FBitmap.Resampler.Width) + 1;
    InflateArea(T, Width, Width);
    T.TopLeft := BitmapToControl(T.TopLeft);
    T.BottomRight := BitmapToControl(T.BottomRight);

    if FBitmapAlign <> baTile then
      InvalidRects.Add(T)
    else
    begin
      with CachedBitmapRect do
      begin
        Tx := Buffer.Width div Right;
        Ty := Buffer.Height div Bottom;
        for J := 0 to Ty do
          for I := 0 to Tx do
          begin
            R := T;
            GR32.OffsetRect(R, Right * I, Bottom * J);
            InvalidRects.Add(R);
          end;
      end;
    end;
  end;

  Changed;
  if not(csCustomPaint in ControlState) then
    Update;
end;

function TCustomImage32.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
var
  W, H: Integer;
begin
  Result := True;

  InvalidateCache;

  W := Bitmap.Width;
  H := Bitmap.Height;

  if (ScaleMode = smScale) then
  begin
    W := Round(W * Scale);
    H := Round(H * Scale);
  end;

  if not (csDesigning in ComponentState) or (W > 0) and (H > 0) then
  begin
    if Align in [alNone, alLeft, alRight] then
      NewWidth := W;

    if Align in [alNone, alTop, alBottom] then
      NewHeight := H;
  end;
end;

function TCustomImage32.CanMousePan: boolean;
begin
  Result := (BitmapAlign = baCustom) and (FMousePanOptions.Enabled);
end;

function TCustomImage32.CanMouseZoom: boolean;
begin
  Result := (ScaleMode in [smScale, smOptimalScaled]) and (FMouseZoomOptions.Enabled);
end;

function TCustomImage32.ControlToBitmap(const ARect: TRect): TRect;
begin
  // Top/Left rounded down, Bottom/Right rounded up
  // It is assumed that ARect.Top<=ARect.Bottom and ARect.Left<=ARect.Right
  UpdateCache;
  with ARect do
  begin
    if (CachedRecScaleX = 0) then
    begin
      Result.Left := High(Result.Left);
      Result.Right := High(Result.Right);
    end else
    begin
      Result.Left := Floor((Left - CachedShiftX) * CachedRecScaleX);
      Result.Right := Ceil((Right - CachedShiftX) * CachedRecScaleX);
    end;

    if (CachedRecScaleY = 0) then
    begin
      Result.Top := High(Result.Top);
      Result.Bottom := High(Result.Bottom);
    end else
    begin
      Result.Top := Floor((Top - CachedShiftY) * CachedRecScaleY);
      Result.Bottom := Ceil((Bottom - CachedShiftY) * CachedRecScaleY);
    end;
  end;
end;

function TCustomImage32.ControlToBitmap(const APoint: TPoint): TPoint;
begin
  // convert point coords from control's ref. frame to bitmap's ref. frame
  // the coordinates are not clipped to bitmap image boundary
  UpdateCache;
  with APoint do
  begin
    if (CachedRecScaleX = 0) then
      Result.X := High(Result.X)
    else
      Result.X := Floor((X - CachedShiftX) * CachedRecScaleX);

    if (CachedRecScaleY = 0) then
      Result.Y := High(Result.Y)
    else
      Result.Y := Floor((Y - CachedShiftY) * CachedRecScaleY);
  end;
end;

function TCustomImage32.ControlToBitmap(const APoint: TFloatPoint): TFloatPoint;
begin
  // subpixel precision version
  UpdateCache;
  with APoint do
  begin
    if (CachedRecScaleX = 0) then
      Result.X := MaxInt
    else
      Result.X := (X - CachedShiftX) * CachedRecScaleX;

    if (CachedRecScaleY = 0) then
      Result.Y := MaxInt
    else
      Result.Y := (Y - CachedShiftY) * CachedRecScaleY;
  end;
end;

procedure TCustomImage32.DoInitStages;
begin
  if Assigned(FOnInitStages) then
    FOnInitStages(Self);
end;

function TCustomImage32.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
var
  r: TRect;
  Pivot: TFloatPoint;
  NewScale: TFloat;
  ZoomIn: boolean;
  ZoomFactor: TFloat;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);

  if (not Result) and (CanMouseZoom) and (FMouseZoomOptions.MatchShiftState(Shift)) then
  begin
{$ifndef FPC} // On FPC the mouse position is in client coordinates. WTF?
    MousePos := ScreenToClient(MousePos);
{$endif FPC}

    if (FMouseZoomOptions.MaintainPivot) and (not ClientRect.Contains(MousePos)) then
      // We can't (or rather, won't) maintain a pivot outside the viewport
      exit;

    r := GetBitmapRect;
    Pivot.X := (MousePos.X - r.Left) / ScaleX;
    Pivot.Y := (MousePos.Y - r.Top) / ScaleY;

    ZoomIn := (WheelDelta > 0) xor (FMouseZoomOptions.Invert);
    WheelDelta := Abs(WheelDelta);

     // WheelDelta is expressed as 120 per step
    ZoomFactor := Power(FMouseZoomOptions.ZoomFactor, WheelDelta div 120);

    if (not ZoomIn) then
      ZoomFactor := 1 / ZoomFactor;

    NewScale := Scale * ZoomFactor;

    DoZoom(Pivot, NewScale);

    Result := True;
  end;
end;

procedure TCustomImage32.DoPaintBuffer;
var
  PaintStageHandlerCount: Integer;
  I, J: Integer;
  PaintStageMask: TPaintStageMask;
begin
  if RepaintOptimizer.Enabled then
    RepaintOptimizer.BeginPaintBuffer;

  UpdateCache;

  SetLength(FPaintStageHandlers, FPaintStages.Count);
  SetLength(FPaintStageNum, FPaintStages.Count);
  PaintStageHandlerCount := 0;

  if (csDesigning in ComponentState) then
    PaintStageMask := [psmDesignTime]
  else
    PaintStageMask := [psmRunTime];

  // compile list of paintstage handler methods
  for I := 0 to FPaintStages.Count - 1 do
  begin
    if (FPaintStages[I].Mask * PaintStageMask <> []) then
    begin
      FPaintStageNum[PaintStageHandlerCount] := I;
      case FPaintStages[I].Stage of
        PST_CUSTOM: FPaintStageHandlers[PaintStageHandlerCount] := ExecCustom;
        PST_CLEAR_BUFFER: FPaintStageHandlers[PaintStageHandlerCount] := ExecClearBuffer;
        PST_CLEAR_BACKGND: FPaintStageHandlers[PaintStageHandlerCount] := ExecClearBackgnd;
        PST_DRAW_BITMAP: FPaintStageHandlers[PaintStageHandlerCount] := ExecDrawBitmap;
        PST_DRAW_LAYERS: FPaintStageHandlers[PaintStageHandlerCount] := ExecDrawLayers;
        PST_CONTROL_FRAME: FPaintStageHandlers[PaintStageHandlerCount] := ExecControlFrame;
        PST_BITMAP_FRAME: FPaintStageHandlers[PaintStageHandlerCount] := ExecBitmapFrame;
      else
        Dec(PaintStageHandlerCount); // this should not happen .
      end;
      Inc(PaintStageHandlerCount);
    end;
  end;

  Buffer.BeginLockUpdate;
  if InvalidRects.Count = 0 then
  begin
    Buffer.ClipRect := GetViewportRect;
    for I := 0 to PaintStageHandlerCount - 1 do
      FPaintStageHandlers[I](Buffer, FPaintStageNum[I]);
  end
  else
  begin
    for J := 0 to InvalidRects.Count - 1 do
    begin
      Buffer.ClipRect := InvalidRects[J]^;
      for I := 0 to PaintStageHandlerCount - 1 do
        FPaintStageHandlers[I](Buffer, FPaintStageNum[I]);
    end;

    Buffer.ClipRect := GetViewportRect;
  end;
  Buffer.EndLockUpdate;

  if RepaintOptimizer.Enabled then
    RepaintOptimizer.EndPaintBuffer;

  // avoid calling inherited, we have a totally different behaviour here...
  BufferValid := True;
  FPartialRepaintQueued := False;
end;

procedure TCustomImage32.DoPaintGDIOverlay;
var
  I: Integer;
begin
  for I := 0 to Layers.Count - 1 do
    if (Layers[I].LayerOptions and LOB_GDI_OVERLAY) <> 0 then
      TLayerAccess(Layers[I]).PaintGDI(Canvas);
  inherited;
end;

procedure TCustomImage32.DoScaleChange;
begin
  if Assigned(FOnScaleChange) then
    FOnScaleChange(Self);
end;

procedure TCustomImage32.DoSetPivot(const APivot: TFloatPoint);
begin
  OffsetHorz := APivot.X;
  OffsetVert := APivot.Y;
end;

procedure TCustomImage32.DoSetZoom(const APivot: TFloatPoint; AScale: TFloat);
var
  DeltaScale: TFloat;
  NewOffset: TFloatPoint;
begin
  if (AScale = Scale) then
    exit;

  DeltaScale := Scale;
  NewOffset.X := OffsetHorz;
  NewOffset.Y := OffsetVert;

  BeginUpdate;
  try
    Scale := AScale;

    if (FMouseZoomOptions.MaintainPivot) and (BitmapAlign = baCustom) then
    begin
      DeltaScale := DeltaScale - Scale;

      NewOffset.X := NewOffset.X + DeltaScale * APivot.X;
      NewOffset.Y := NewOffset.Y + DeltaScale * APivot.Y;

      DoSetPivot(NewOffset);
    end;

    Changed;
  finally
    EndUpdate;
  end;
end;

procedure TCustomImage32.DoZoom(APivot: TFloatPoint; AScale: TFloat);
{$if defined(AnimatedZoom)}
var
  StartValue, DeltaValue: TFloat;
const
  MinZoomDelta = 0.01;
{$ifend}
begin
  AScale := Constrain(AScale, FMouseZoomOptions.MinScale, FMouseZoomOptions.MaxScale);
  if (AScale = Scale) then
    exit;

{$if defined(AnimatedZoom)}
  if (FMouseZoomOptions.Animate) and (Showing) then
  begin
    StartValue := Scale;
    DeltaValue := AScale-StartValue;

    // Ease between old and new scale
    AnimatedTween(TEaseCubic.EaseInOut, ZoomAnimateTime,
      procedure(Value: Double; var Continue: boolean)
      var
        NewValue: Single;
      begin
        NewValue := StartValue + Value*DeltaValue;

        if (Abs(StartValue-NewValue) >= MinZoomDelta) and
          (Abs(AScale-NewValue) >= MinZoomDelta) and
          (Scale <> NewValue) then
        begin
          DoSetZoom(APivot, NewValue);

          // Paint immediately or user will not see animation
          Repaint;
        end;
      end, ZoomAnimateDeltaTime);
  end;
{$ifend}
  DoSetZoom(APivot, AScale);

  ForceFullInvalidate;
end;

procedure TCustomImage32.ExecBitmapFrame(Dest: TBitmap32; StageNum: Integer);
begin
  Dest.Canvas.DrawFocusRect(CachedBitmapRect);
end;

procedure TCustomImage32.ExecClearBackgnd(Dest: TBitmap32; StageNum: Integer);
var
  OuterBorder: integer;
  InnerBorder: integer;
  Width: integer;
  OddRow, EvenRow: TArrayOfColor32;
  ColorEven, ColorOdd: PColor32;
  X, Y: integer;
  i: Integer;
  Parity: integer;
  ViewportRect: TRect;
  r: TRect;
  Tile: TRect;
  C: TColor32;
  TileX, TileY: integer;
  DrawFancyStuff: boolean;
  DrawBitmapBackground: boolean;
begin
  ViewportRect := GetViewportRect;

  if (not Bitmap.Empty) and (Bitmap.DrawMode = dmOpaque) then
  begin
    // No need to draw background if bitmap covers everything
    if (BitmapAlign = baTile) or (CachedBitmapRect.Contains(ViewportRect)) then
      exit;
  end;


  // Background (from inside out/top down):
  // - Checkers
  // - white border
  // - 1px black/dark border
  // - bump map/solid color
  // - alpha drop shadow

  if (FBackgroundOptions.OuterBorderColor <> clNone) then
    OuterBorder := 1
  else
    OuterBorder := 0;

  if (FBackgroundOptions.InnerBorderWidth > 0) and (FBackgroundOptions.InnerBorderColor <> clNone) then
    InnerBorder := FBackgroundOptions.InnerBorderWidth
  else
    InnerBorder := 0;

  // If the bitmap is empty or if we're tiling it, or if the borders and dropshadow
  // is disabled, then we only need to do a simple clear of the whole background.
  DrawFancyStuff := (not Bitmap.Empty) and (BitmapAlign <> baTile) and
    ((not FBackgroundOptions.DropShadowBitmap.Empty) or (FBackgroundOptions.DropShadowSize <> 0) or
     (OuterBorder <> 0) or (InnerBorder <> 0));

     // Do we need to clear the area below the bitmap?
  DrawBitmapBackground := (not Bitmap.Empty) and (BitmapAlign <> baTile) and (Bitmap.DrawMode <> dmOpaque);

  r := CachedBitmapRect;
  if (DrawFancyStuff) then
    GR32.InflateRect(r, OuterBorder+InnerBorder, OuterBorder+InnerBorder);

  (*
  ** Background (pattern or solid color)
  *)
  if (FBackgroundOptions.FillStyle = bfsPattern) then
  begin
    Assert(not FBackgroundOptions.PatternBitmap.Empty);

    TileX := (ViewportRect.Width + FBackgroundOptions.PatternBitmap.Width - 1) div FBackgroundOptions.PatternBitmap.Width;
    TileY := (ViewportRect.Height + FBackgroundOptions.PatternBitmap.Height - 1) div FBackgroundOptions.PatternBitmap.Height;
    for Y := 0 to TileY-1 do
      for X := 0 to TileX-1 do
      begin
        Tile := Rect(0, 0, FBackgroundOptions.PatternBitmap.Width, FBackgroundOptions.PatternBitmap.Height);
        GR32.OffsetRect(Tile, X * FBackgroundOptions.PatternBitmap.Width, Y * FBackgroundOptions.PatternBitmap.Height);

        if (DrawBitmapBackground) and (CachedBitmapRect.Contains(Tile)) then
          // Tile would have been obscured by bitmap/checkers
          continue;

        BlockTransfer(Dest,
          Tile.Left, Tile.Top, Dest.ClipRect,
          FBackgroundOptions.PatternBitmap, FBackgroundOptions.PatternBitmap.BoundsRect, dmOpaque);
      end;

    // CheckersStyle=bcsNone doesn't clear the area under the bitmap so we need to do it here
    if (DrawBitmapBackground) and (FBackgroundOptions.CheckersStyle = bcsNone) then
    begin
      C := Color32(Color);
      Dest.FillRectS(CachedBitmapRect, C);
    end;
  end else
  if (FBackgroundOptions.FillStyle = bfsColor) then
  begin
    C := Color32(Color);

    if InvalidRects.Count > 0 then
    begin
      for i := 0 to InvalidRects.Count-1 do
      begin
        if (DrawBitmapBackground) and (FBackgroundOptions.CheckersStyle <> bcsNone) and (CachedBitmapRect.Contains(InvalidRects[i]^)) then
          continue;

        with InvalidRects[i]^ do
          Dest.FillRectS(Left, Top, Right, Bottom, C);
      end;
    end else
    if (DrawBitmapBackground) and (FBackgroundOptions.CheckersStyle <> bcsNone) then
    begin
      Dest.FillRectS(Rect(ViewportRect.Left, ViewportRect.Top, ViewportRect.Right, r.Top), C);
      Dest.FillRectS(Rect(ViewportRect.Left, r.Top, r.Left, r.Bottom), C);
      Dest.FillRectS(Rect(r.Right, r.Top, ViewportRect.Right, r.Bottom), C);
      Dest.FillRectS(Rect(ViewportRect.Left, r.Bottom, ViewportRect.Right, ViewportRect.Bottom), C);
    end else
      Dest.Clear(C);
  end;

  if (DrawFancyStuff) then
  begin
    (*
    ** Drop shadow
    *)
    if (not FBackgroundOptions.DropShadowBitmap.Empty) then
    begin
      (*
      The drop shadow tile is partitioned into 5 segments

                                       +
        +-------+                     1|#
        |       |#
        |       |#   ->               2|#
        |       |#
        +-------+#           3    4   5|
         #########           +-   -   -+
                              #   #     #

      *)

      X := FBackgroundOptions.DropShadowBitmap.Width div 2;
      Y := FBackgroundOptions.DropShadowBitmap.Height div 2;
      // Segment 1
      FBackgroundOptions.DropShadowBitmap.DrawTo(Dest,
        r.Right, r.Top+FBackgroundOptions.DropShadowOffset,
        Rect(X, 0, FBackgroundOptions.DropShadowBitmap.Width-1, Y));

      // Segment 2 (stretched)
      FBackgroundOptions.DropShadowBitmap.DrawTo(Dest,
        Rect(r.Right, r.Top+FBackgroundOptions.DropShadowOffset+Y, r.Right+X, r.Bottom),
        Rect(X, Y, FBackgroundOptions.DropShadowBitmap.Width, Y+1));

      // Segment 3
      FBackgroundOptions.DropShadowBitmap.DrawTo(Dest,
        r.Left+FBackgroundOptions.DropShadowOffset, r.Bottom,
        Rect(0, Y, X, FBackgroundOptions.DropShadowBitmap.Height-1));

      // Segment 4 (stretched)
      FBackgroundOptions.DropShadowBitmap.DrawTo(Dest,
        Rect(r.Left+FBackgroundOptions.DropShadowOffset+X, r.Bottom, r.Right, r.Bottom+Y),
        Rect(X, Y, X+1, FBackgroundOptions.DropShadowBitmap.Height));

      // Segment 5
      FBackgroundOptions.DropShadowBitmap.DrawTo(Dest,
        r.Right, r.Bottom,
        Rect(X, Y, FBackgroundOptions.DropShadowBitmap.Width-1, FBackgroundOptions.DropShadowBitmap.Height-1));
    end else
    if (FBackgroundOptions.DropShadowSize <> 0) then
    begin
      // Note: Transparent
      Dest.FillRectTS(Rect(r.Right, r.Top+FBackgroundOptions.DropShadowOffset, r.Right+FBackgroundOptions.DropShadowSize, r.Bottom), FBackgroundOptions.DropShadowColor);
      Dest.FillRectTS(Rect(r.Left+FBackgroundOptions.DropShadowOffset, r.Bottom, r.Right+FBackgroundOptions.DropShadowSize, r.Bottom+FBackgroundOptions.DropShadowSize), FBackgroundOptions.DropShadowColor);
    end;

    (*
    ** Outer dark border
    *)
    if (OuterBorder <> 0) then
    begin
      Dest.FrameRectS(r, Color32(FBackgroundOptions.OuterBorderColor));
      GR32.InflateRect(r, -OuterBorder, -OuterBorder);
    end;

    (*
    ** Inner light border
    *)
    if (InnerBorder <> 0) then
    begin
      C := Color32(FBackgroundOptions.InnerBorderColor);
      if (InnerBorder > 1) then
      begin
        Dest.FillRectS(Rect(r.Left, r.Top, r.Right, CachedBitmapRect.Top), C);
        Dest.FillRectS(Rect(r.Left, CachedBitmapRect.Top, CachedBitmapRect.Left, CachedBitmapRect.Bottom), C);
        Dest.FillRectS(Rect(CachedBitmapRect.Right, CachedBitmapRect.Top, r.Right, CachedBitmapRect.Bottom), C);
        Dest.FillRectS(Rect(r.Left, CachedBitmapRect.Bottom, r.Right, r.Bottom), C);
      end else
        Dest.FrameRectS(r, C);
    end;
  end;

  (*
  ** Checkers
  *)
  if (FBackgroundOptions.CheckersStyle <> bcsNone) and
    ((DrawBitmapBackground) or
     ((FBackgroundOptions.FillStyle = bfsCheckers) and
      ((Bitmap.Empty) or (Bitmap.DrawMode = dmOpaque)))) then
  begin
    if (FBackgroundOptions.FillStyle = bfsCheckers) then
      // Fill the whole viewport
      r := Dest.ClipRect
    else
      // Fill the area under the bitmap
      GR32.IntersectRect(r, CachedBitmapRect, Dest.ClipRect);

    Width := r.Width;

    if (Width > 0) then
    begin
      if (FBackgroundOptions.CheckersStyle <> bcsCustom) or (FBackgroundOptions.CheckersColors[0] <> FBackgroundOptions.CheckersColors[1]) then
      begin
        SetLength(OddRow, Width);
        SetLength(EvenRow, Width);

        ColorEven := @EvenRow[0];
        ColorOdd := @OddRow[0];
        for X := 0 to Width-1 do
        begin
          Parity := ((r.Left+X) shr FBackgroundOptions.CheckersExponent) and $1;
          ColorEven^ := FBackgroundOptions.CheckersColors[Parity];
          ColorOdd^ := FBackgroundOptions.CheckersColors[1-Parity];
          inc(ColorEven);
          inc(ColorOdd);
        end;

        // Note: For ((DrawMode<>dmOpaque) and (FillStyle=bfsCheckers)) we should
        // exclude filling the area covered by the bitmap. For simplicity we're
        // not doing that.

        for Y := r.Top to r.Bottom-1 do
        begin
          Parity := (Y shr FBackgroundOptions.CheckersExponent) and $1;
          if (Parity = 0) then
            MoveLongword(EvenRow[0], Dest.PixelPtr[r.Left, Y]^, Width)
          else
            MoveLongword(OddRow[0], Dest.PixelPtr[r.Left, Y]^, Width);
        end;
      end else
        // Odd color = Even color -> Just clear with the color
        Dest.FillRectS(r, FBackgroundOptions.CheckersColors[0]);
    end;
  end;
end;

procedure TCustomImage32.ExecClearBuffer(Dest: TBitmap32; StageNum: Integer);
begin
  // By default ExecClearBuffer is never called because the PST_CLEAR_BUFFER
  // paint stage isn't used by default.

  // We skip the clear if Image.Bitmap.DrawMode=dmOpaque since the bitmap will
  // cover the area we cleared anyway.
  if (Bitmap.Empty) or (Bitmap.DrawMode <> dmOpaque) then
    Dest.Clear(Color32(Color));
end;

procedure TCustomImage32.ExecControlFrame(Dest: TBitmap32; StageNum: Integer);
begin
  DrawFocusRect(Dest.Handle, Rect(0, 0, Width, Height));
end;

procedure TCustomImage32.ExecCustom(Dest: TBitmap32; StageNum: Integer);
begin
  if Assigned(FOnPaintStage) then
    FOnPaintStage(Self, Dest, StageNum);
end;

procedure TCustomImage32.ExecDrawBitmap(Dest: TBitmap32; StageNum: Integer);
var
  I, J, Tx, Ty: Integer;
  R: TRect;
begin
  if Bitmap.Empty or GR32.IsRectEmpty(CachedBitmapRect) then
    Exit;

  Bitmap.Lock;
  try
    if (BitmapAlign <> baTile) then
      Bitmap.DrawTo(Dest, CachedBitmapRect)
    else
    with CachedBitmapRect do
    begin
      Tx := Dest.Width div Right;
      Ty := Dest.Height div Bottom;
      for J := 0 to Ty do
        for I := 0 to Tx do
        begin
          R := CachedBitmapRect;
          GR32.OffsetRect(R, Right * I, Bottom * J);
          Bitmap.DrawTo(Dest, R);
        end;
    end;
  finally
    Bitmap.Unlock;
  end;
end;

procedure TCustomImage32.ExecDrawLayers(Dest: TBitmap32; StageNum: Integer);
var
  I: Integer;
  Mask: Cardinal;
begin
  Mask := PaintStages[StageNum]^.Parameter;
  for I := 0 to Layers.Count - 1 do
    if (Layers.Items[I].LayerOptions and Mask) <> 0 then
      TLayerAccess(Layers.Items[I]).DoPaint(Dest);
end;

function TCustomImage32.GetBitmapRect: TRect;
var
  Size: TSize;
begin
  if Bitmap.Empty then
    with Result do
    begin
      Left := 0;
      Right := 0;
      Top := 0;
      Bottom := 0;
    end
  else
  begin
    Size := GetBitmapSize;
    Result := Rect(0, 0, Size.Cx, Size.Cy);
    if BitmapAlign = baCenter then
      GR32.OffsetRect(Result, (Width - Size.Cx) div 2, (Height - Size.Cy) div 2)
    else
    if BitmapAlign = baCustom then
      GR32.OffsetRect(Result, Round(OffsetHorz), Round(OffsetVert));
  end;
end;

function TCustomImage32.GetBitmapSize: TSize;
var
  Mode: TScaleMode;
  ViewportWidth, ViewportHeight: Integer;
  RScaleX, RScaleY: TFloat;
begin
  begin
    if Bitmap.Empty or (Width = 0) or (Height = 0) then
    begin
      Result.Cx := 0;
      Result.Cy := 0;
      Exit;
    end;

    with GetViewportRect do
    begin
      ViewportWidth := Right - Left;
      ViewportHeight := Bottom - Top;
    end;

    // check for optimal modes as these are compounds of the other modes.
    case ScaleMode of
      smOptimal:
        if (Bitmap.Width > ViewportWidth) or (Bitmap.Height > ViewportHeight) then
          Mode := smResize
        else
          Mode := smNormal;

      smOptimalScaled:
        if (Round(Bitmap.Width * ScaleX) > ViewportWidth) or
          (Round(Bitmap.Height * ScaleY) > ViewportHeight) then
          Mode := smResize
        else
          Mode := smScale;
    else
      Mode := ScaleMode;
    end;

    case Mode of
      smNormal:
        begin
          Result.Cx := Bitmap.Width;
          Result.Cy := Bitmap.Height;
        end;

      smStretch:
        begin
          Result.Cx := ViewportWidth;
          Result.Cy := ViewportHeight;
        end;

      smResize:
        begin
          Result.Cx := Bitmap.Width;
          Result.Cy := Bitmap.Height;
          RScaleX := ViewportWidth / Result.Cx;
          RScaleY := ViewportHeight / Result.Cy;
          if (RScaleX >= RScaleY) then
          begin
            Result.Cx := Round(Result.Cx * RScaleY);
            Result.Cy := ViewportHeight;
          end
          else
          begin
            Result.Cx := ViewportWidth;
            Result.Cy := Round(Result.Cy * RScaleX);
          end;
        end;
    else // smScale
      begin
        Result.Cx := Round(Bitmap.Width * ScaleX);
        Result.Cy := Round(Bitmap.Height * ScaleY);
      end;
    end;

    if (Result.Cx <= 0) then
      Result.Cx := 0;
    if (Result.Cy <= 0) then
      Result.Cy := 0;
  end;
end;

function TCustomImage32.GetOnPixelCombine: TPixelCombineEvent;
begin
  Result := FBitmap.OnPixelCombine;
end;

procedure TCustomImage32.InitDefaultStages;
begin
  // clear buffer
  (* Not used. PST_CLEAR_BACKGND is used instead.
  with PaintStages.Add^ do
  begin
    Mask := [];
    Stage := PST_CLEAR_BUFFER;
  end;
  *)

  // background
  with PaintStages.Add^ do
  begin
    Mask := [psmRunTime, psmDesignTime]; // See issue #247
    Stage := PST_CLEAR_BACKGND;
  end;

  // control frame
  with PaintStages.Add^ do
  begin
    Mask := [psmDesignTime];
    Stage := PST_CONTROL_FRAME;
  end;

  // bitmap
  with PaintStages.Add^ do
  begin
    Mask := [psmRunTime, psmDesignTime, psmExport];
    Stage := PST_DRAW_BITMAP;
  end;

  // bitmap frame
  with PaintStages.Add^ do
  begin
    Mask := [psmDesignTime];
    Stage := PST_BITMAP_FRAME;
  end;

  // layers
  with PaintStages.Add^ do
  begin
    Mask := [psmRunTime, psmDesignTime, psmExport];
    Stage := PST_DRAW_LAYERS;
    Parameter := LOB_VISIBLE;
  end;
end;

procedure TCustomImage32.Invalidate;
begin
  BufferValid := False;
  CacheValid := False;
  inherited;
end;

procedure TCustomImage32.Update(const Rect: TRect);
begin
  Invalidate(Rect);
end;

procedure TCustomImage32.Invalidate(const Rect: TRect);
var
  UpdateRectNotification: IUpdateRectNotification;
begin
  if (RepaintOptimizer.Enabled) and (Supports(RepaintOptimizer, IUpdateRectNotification, UpdateRectNotification)) then
    UpdateRectNotification.AreaUpdated(Rect, AREAINFO_RECT);
end;

procedure TCustomImage32.InvalidateCache;
begin
  if RepaintOptimizer.Enabled and CacheValid then
    RepaintOptimizer.Reset;

  CacheValid := False;
end;

function TCustomImage32.InvalidRectsAvailable: Boolean;
begin
  // avoid calling inherited, we have a totally different behaviour here...
  DoPrepareInvalidRects;
  Result := (InvalidRects.Count > 0);
end;

procedure TCustomImage32.LayerCollectionChangeHandler(Sender: TObject);
begin
  Changed;
end;

procedure TCustomImage32.LayerCollectionGDIUpdateHandler(Sender: TObject);
begin
  Paint;
end;

procedure TCustomImage32.LayerCollectionGetViewportScaleHandler(Sender: TObject;
  out ScaleX, ScaleY: TFloat);
begin
  UpdateCache;
  ScaleX := CachedScaleX;
  ScaleY := CachedScaleY;
end;

procedure TCustomImage32.LayerCollectionGetViewportShiftHandler(Sender: TObject;
  out ShiftX, ShiftY: TFloat);
begin
  UpdateCache;
  ShiftX := CachedShiftX;
  ShiftY := CachedShiftY;
end;

procedure TCustomImage32.Loaded;
begin
  inherited;
  DoInitStages;
end;

// PanDetect is an adaption of DragDetectPlus from the Drag and Drop Component Suite.
// The following assumptions are made:
// - The Position parameter is in screen coordinates.
// - The mouse has already been captured.
// - Only the left mouse button is handled.
{$if defined(WINDOWS)}
function PanDetect(Handle: THandle; Position: TPoint): boolean;
var
  DragRect: TRect;
  Msg: TMsg;
  StartTime: DWORD;
const
  PM_QS_INPUT = QS_INPUT shl 16;
  PM_QS_KEY = QS_KEY shl 16;
  PM_QS_MOUSEMOVE = QS_MOUSEMOVE shl 16;
  PM_QS_MOUSEBUTTON = QS_MOUSEBUTTON shl 16;
begin
  Result := False;

  // Check mouse state, and punt if none of the mouse buttons are down.
  if ((GetKeyState(VK_LBUTTON) AND $8000) = 0) then
    exit;

  // Calculate the drag rect.
  // If the mouse leaves this rect, while the mouse button is pressed, a drag is
  // detected.
  DragRect.TopLeft := Position;
  DragRect.BottomRight := Position;
  GR32.InflateRect(DragRect, GetSystemMetrics(SM_CXDRAG), GetSystemMetrics(SM_CYDRAG));

  StartTime := TimeGetTime;

  // Abort if we haven't captured the mouse.
  if (GetCapture <> Handle) then
    exit;

  while (not Result) do
  begin
    // Wait for mouse or keyboard events.
    // - but do not eat mouse button messages (so we don't break popup menus etc).
    if (PeekMessage(Msg, 0, WM_LBUTTONDOWN, WM_LBUTTONUP, PM_NOREMOVE)) then
    begin
      // Mouse button was changed - bail out.
      exit;
    end;

    while (not PeekMessage(Msg, 0, WM_LBUTTONDOWN, WM_LBUTTONUP, PM_NOREMOVE)) and
     (not PeekMessage(Msg, 0, 0, 0, PM_REMOVE or PM_QS_KEY or PM_QS_MOUSEMOVE)) and
      (GetCapture = Handle) do
    begin
      // If there are no events for 500mS start drag without further ado.
      if (MsgWaitForMultipleObjects(0, nil^, False, 500, QS_INPUT) = WAIT_TIMEOUT) then
      begin
        Result := True;
        exit;
      end;
    end;

    // Bail out if someone else has captured the mouse.
    if (GetCapture <> Handle) then
      break;

    case (Msg.message) of
      // Mouse was moved.
      WM_MOUSEMOVE:
        // Start drag if mouse has moved outside the drag rect and the minimum
        // time has elapsed.
        // Note that we ignore time warp (wrap around) and that Msg.Time
        // might be smaller than StartTime.
        Result := (not GR32.PtInRect(DragRect, Msg.pt)) and (Msg.time >= StartTime + DWORD(100));

      // [Esc] cancels drag detection.
      WM_KEYDOWN:
        if (Msg.wParam = VK_ESCAPE) then
          break;

      // Some operation cancelled our mouse capture.
      WM_CANCELMODE:
        break;

      // Application is shutting down.
      WM_QUIT:
        begin
          // Put quit message back in queue and abort.
          PostQuitMessage(Msg.wParam);
          exit;
        end;
    end;
  end;
end;
{$ifend}

procedure TCustomImage32.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Layer: TCustomLayer;
begin
  inherited;

  if TabStop and CanFocus then
    SetFocus;

  BeginUpdate;
  try
    if Layers.MouseEvents then
      Layer := TLayerCollectionAccess(Layers).MouseDown(Button, Shift, X, Y)
    else
      Layer := nil;

    // lock the capture only if mbLeft was pushed or any mouse listener was activated
    if (Button = mbLeft) or (TLayerCollectionAccess(Layers).MouseListener <> nil) then
      MouseCapture := True;

    MouseDown(Button, Shift, X, Y, Layer);

    if (Layer = nil) and (CanMousePan) and (Button = FMousePanOptions.MouseButton) and (FMousePanOptions.MatchShiftState(Shift)) then
    begin
      // Wait a moment, looking for a mouse-up, before we decide that this
      // is a drag and not a click. Note that we cannot use the Windows DragDetect
      // function as it eats the mouse-up event and thus break the OnClick generation.
{$if defined(WINDOWS)}
      if (not PanDetect(WindowHandle, ClientToScreen(Point(X, Y)))) then
        exit;
{$ifend}

      FIsMousePanning := True;
      if (FMousePanOptions.PanCursor <> crDefault) then
        Screen.Cursor := FMousePanOptions.PanCursor;
      // Avoid OnClick event when pan finishes
      ControlState := ControlState - [csClicked];

      // Remember start point
      FMousePanStartPos.X := X;
      FMousePanStartPos.Y := Y;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TCustomImage32.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Layer: TCustomLayer;
  Delta: TPoint;
begin
  inherited;

  // If we're panning then calculate how far mouse has moved since last and
  // scroll image the same amount.
  if (FIsMousePanning) then
  begin
    Delta.X := FMousePanStartPos.X - X;
    Delta.Y := FMousePanStartPos.Y - Y;

    FMousePanStartPos.X := X;
    FMousePanStartPos.Y := Y;

    if (Delta.X <> 0) or (Delta.Y <> 0) then
      Scroll(Delta.X, Delta.Y);

    if (FMousePanOptions.PanCursor <> crDefault) then
      Screen.Cursor := FMousePanOptions.PanCursor;
  end else
  begin
    BeginUpdate;
    try
      if Layers.MouseEvents then
        Layer := TLayerCollectionAccess(Layers).MouseMove(Shift, X, Y)
      else
        Layer := nil;

      MouseMove(Shift, X, Y, Layer);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TCustomImage32.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Layer: TCustomLayer;
  MouseListener: TCustomLayer;
begin
  MouseListener := TLayerCollectionAccess(Layers).MouseListener;

  BeginUpdate;
  try
    if Layers.MouseEvents then
      Layer := TLayerCollectionAccess(Layers).MouseUp(Button, Shift, X, Y)
    else
      Layer := nil;

    // unlock the capture using same criteria as was used to acquire it
    if (Button = mbLeft) or ((MouseListener <> nil) and (TLayerCollectionAccess(Layers).MouseListener = nil)) then
      MouseCapture := False;

    MouseUp(Button, Shift, X, Y, Layer);

    if (FIsMousePanning) and (Button = FMousePanOptions.MouseButton) then
    begin
      FIsMousePanning := False;
      if (FMousePanOptions.PanCursor <> crDefault) then
        Screen.Cursor := crDefault;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TCustomImage32.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, Button, Shift, X, Y, Layer);
end;

procedure TCustomImage32.MouseMove(Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, Shift, X, Y, Layer);
end;

procedure TCustomImage32.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer; Layer: TCustomLayer);
begin
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, Button, Shift, X, Y, Layer);
end;

procedure TCustomImage32.MouseLeave;
begin
  if (Layers.MouseEvents) and (Layers.MouseListener = nil) then
    Screen.Cursor := crDefault;
  inherited;
end;

procedure TCustomImage32.PaintTo(Dest: TBitmap32; DestRect: TRect);
var
  OldRepaintMode: TRepaintMode;
  I: Integer;
begin
  if (Dest = nil) then
    exit;

  OldRepaintMode := RepaintMode;
  RepaintMode := rmFull;

  CachedBitmapRect := DestRect;

  if (CachedBitmapRect.Right <= CachedBitmapRect.Left) or (CachedBitmapRect.Bottom <= CachedBitmapRect.Top) or Bitmap.Empty then
    SetXForm(0, 0, 1, 1)
  else
    SetXForm(CachedBitmapRect.Left, CachedBitmapRect.Top, CachedBitmapRect.Width / Bitmap.Width, CachedBitmapRect.Height / Bitmap.Height);

  CacheValid := True;

  //
  // By default neither PST_CLEAR_BUFFER/ExecClearBuffer nor
  // PST_CLEAR_BACKGND/ExecClearBackgnd is called to clear the
  // destination bitmap.
  //
  // This means that we are painting the bitmap and layers onto whatever is
  // already on the destination bitmap. This makes it possible to produce a
  // flattened semitransparent bitmap.
  // If an flattened opaque bitmap is desired then:
  //
  // - The destination bitmap can be made opaque before PaintTo is called
  //   (i.e. cleared with the desired background color).
  //
  // - A opaque Image.Bitmap can be used (i.e. all pixels have Alpha=255).
  //
  // - Image.Bitmap.DrawMode can be set to dmOpaque (the default).
  //
  // See issue #248
  //

  PaintToMode := True;
  try
    for I := 0 to FPaintStages.Count - 1 do
      if (psmExport in FPaintStages[I].Mask) then
        case FPaintStages[I].Stage of
          PST_CUSTOM: ExecCustom(Dest, I);
          PST_CLEAR_BUFFER: ExecClearBuffer(Dest, I);
          PST_CLEAR_BACKGND: ExecClearBackgnd(Dest, I);
          PST_DRAW_BITMAP: ExecDrawBitmap(Dest, I);
          PST_DRAW_LAYERS: ExecDrawLayers(Dest, I);
          PST_CONTROL_FRAME: ExecControlFrame(Dest, I);
          PST_BITMAP_FRAME: ExecBitmapFrame(Dest, I);
        end;
  finally
    PaintToMode := False;
  end;
  CacheValid := False;

  RepaintMode := OldRepaintMode;
end;

procedure TCustomImage32.Resize;
begin
  InvalidateCache;
  inherited;
end;

procedure TCustomImage32.Scroll(Dx, Dy: Single);
begin
  if (IsZero(Dx)) and (IsZero(Dy)) then
    Exit;

  BeginUpdate;
  try

    OffsetHorz := OffsetHorz - Dx;
    OffsetVert := OffsetVert - Dy;

  finally
    EndUpdate;
  end;
end;

procedure TCustomImage32.Scroll(Dx, Dy: Integer);
begin
  if (Dx <> 0) or (Dy <> 0) then
{$ifndef FPC} // FPC chokes on the float conversion with an exception
  Scroll(Single(Dx), Single(Dy));
{$else FPC}
  Scroll(Dx * 1.0, Dy * 1.0);
{$endif FPC}
end;

procedure TCustomImage32.SetBackgroundOptions(const Value: TBackgroundOptions);
begin
  FBackgroundOptions.Assign(Value);
end;

procedure TCustomImage32.SetBitmap(Value: TBitmap32);
begin
  InvalidateCache;
  FBitmap.Assign(Value);
end;

procedure TCustomImage32.SetBitmapAlign(Value: TBitmapAlign);
begin
  InvalidateCache;
  FBitmapAlign := Value;
  Changed;
end;

procedure TCustomImage32.SetLayers(Value: TLayerCollection);
begin
  FLayers.Assign(Value);
end;

procedure TCustomImage32.SetOffsetHorz(Value: TFloat);
begin
  if Value <> FOffsetHorz then
  begin
    InvalidateCache;
    FOffsetHorz := Value;
    Changed;
  end;
end;

procedure TCustomImage32.SetOffsetVert(Value: TFloat);
begin
  if Value <> FOffsetVert then
  begin
    FOffsetVert := Value;
    InvalidateCache;
    Changed;
  end;
end;

procedure TCustomImage32.SetOnPixelCombine(Value: TPixelCombineEvent);
begin
  FBitmap.OnPixelCombine := Value;
  Changed;
end;

procedure TCustomImage32.SetMousePanOptions(const Value: TMousePanOptions);
begin
  FMousePanOptions.Assign(Value);
end;

procedure TCustomImage32.SetMouseZoomOptions(const Value: TMouseZoomOptions);
begin
  FMouseZoomOptions.Assign(Value);
end;

procedure TCustomImage32.SetScale(Value: TFloat);
begin
  if Value < 0.001 then
    Value := 0.001;

  if Value <> FScaleX then
  begin
    InvalidateCache;
    FScaleX := Value;
    FScaleY := Value;
    CachedScaleX := FScaleX;
    CachedScaleY := FScaleY;
    CachedRecScaleX := 1 / Value;
    CachedRecScaleY := 1 / Value;
    DoScaleChange;
    Changed;
  end;
end;

procedure TCustomImage32.SetScaleX(Value: TFloat);
begin
  if Value < 0.001 then
    Value := 0.001;

  if Value <> FScaleX then
  begin
    InvalidateCache;
    FScaleX := Value;
    CachedScaleX := Value;
    CachedRecScaleX := 1 / Value;
    DoScaleChange;
    Changed;
  end;
end;

procedure TCustomImage32.SetScaleY(Value: TFloat);
begin
  if Value < 0.001 then
    Value := 0.001;

  if Value <> FScaleY then
  begin
    InvalidateCache;
    FScaleY := Value;
    CachedScaleY := Value;
    CachedRecScaleY := 1 / Value;
    DoScaleChange;
    Changed;
  end;
end;

procedure TCustomImage32.SetScaleMode(Value: TScaleMode);
begin
  if Value <> FScaleMode then
  begin
    InvalidateCache;
    FScaleMode := Value;
    Changed;
  end;
end;

procedure TCustomImage32.SetupBitmap(DoClear: Boolean = False; ClearColor: TColor32 = $FF000000);
begin
  BeginUpdate;
  try
    FBitmap.BeginUpdate;
    with GetViewPortRect do
      FBitmap.SetSize(Right - Left, Bottom - Top);
    if DoClear then
      FBitmap.Clear(ClearColor);
    FBitmap.EndUpdate;
    InvalidateCache;
    Changed;
  finally
    EndUpdate;
  end;
end;

procedure TCustomImage32.SetXForm(ShiftX, ShiftY, ScaleX, ScaleY: TFloat);
begin
  CachedShiftX := ShiftX;
  CachedShiftY := ShiftY;
  CachedScaleX := ScaleX;
  CachedScaleY := ScaleY;
  if (ScaleX <> 0) then
    CachedRecScaleX := 1 / ScaleX
  else
    CachedRecScaleX := 0;

  if (ScaleY <> 0) then
    CachedRecScaleY := 1 / ScaleY
  else
    CachedRecScaleY := 0;
end;

procedure TCustomImage32.UpdateCache;
begin
  if CacheValid then
    Exit;

  CachedBitmapRect := GetBitmapRect;

  if Bitmap.Empty then
    SetXForm(0, 0, 1, 1)
  else
    SetXForm(
      CachedBitmapRect.Left, CachedBitmapRect.Top,
      (CachedBitmapRect.Right - CachedBitmapRect.Left) / Bitmap.Width,
      (CachedBitmapRect.Bottom - CachedBitmapRect.Top) / Bitmap.Height
    );

  CacheValid := True;
end;


{ TIVScrollProperties }

function TIVScrollProperties.GetIncrement: Integer;
begin
  Result := Round(TCustomRangeBar(Master).Increment);
end;

function TIVScrollProperties.GetSize: Integer;
begin
  Result := ImgView.ScrollBarSize;
end;

function TIVScrollProperties.GetVisibility: TScrollbarVisibility;
begin
  Result := ImgView.ScrollBarVisibility;
end;

procedure TIVScrollProperties.SetIncrement(Value: Integer);
begin
  TCustomRangeBar(Master).Increment := Value;
  TCustomRangeBar(Slave).Increment := Value;
end;

procedure TIVScrollProperties.SetSize(Value: Integer);
begin
  ImgView.ScrollBarSize := Value;
  ImgView.AlignAll;
  ImgView.UpdateImage;
end;

procedure TIVScrollProperties.SetVisibility(const Value: TScrollbarVisibility);
begin
  if Value <> ImgView.ScrollBarVisibility then
  begin
    ImgView.ScrollBarVisibility := Value;
    ImgView.Resize;
  end;
end;

{ TCustomImgView32 }

procedure TCustomImgView32.AlignAll;
var
  ScrollbarVisible: Boolean;
  ViewPort: TRect;
  NeedResize: boolean;
begin
  if (Width <= 0) or (Height <= 0) then
    Exit;

  NeedResize := False;
  ViewPort := GetViewportRect;
  ScrollbarVisible := GetScrollBarsVisible;

  if (HScroll <> nil) then
  begin
    NeedResize := (HScroll.Visible <> ScrollbarVisible);

    HScroll.BoundsRect := Rect(ViewPort.Left, ViewPort.Bottom, ViewPort.Right, Self.Height);
    HScroll.Visible := ScrollbarVisible;
  end;

  if (VScroll <> nil) then
  begin
    NeedResize := NeedResize or (VScroll.Visible <> ScrollbarVisible);

    VScroll.BoundsRect := Rect(ViewPort.Right, ViewPort.Top, Self.Width, ViewPort.Bottom);
    VScroll.Visible := ScrollbarVisible;
  end;

  if (NeedResize) then
  begin
    // Scrollbars has been shown or hidden. Buffer must resize to align with new viewport.
    // This will automatically lead to the viewport being redrawn.
    ResizeBuffer;
    BufferValid := False
  end;
end;

procedure TCustomImgView32.BitmapResized;
begin
  inherited;
  UpdateScrollBars;
  if Centered then
    ScrollToCenter(Bitmap.Width div 2, Bitmap.Height div 2)
  else
  begin
    HScroll.Position := 0;
    VScroll.Position := 0;
    UpdateImage;
  end;
end;

function TCustomImgView32.CanMousePan: boolean;
begin
  Result := (inherited CanMousePan) and
    (ScaleMode in [smScale,smNormal]) and
    ((HScroll.Range > (TRangeBarAccess(HScroll).EffectiveWindow + VScroll.Width)) or
     (VScroll.Range > (TRangeBarAccess(VScroll).EffectiveWindow + HScroll.Height)));
end;

constructor TCustomImgView32.Create(AOwner: TComponent);
begin
  inherited;

  FScrollBarSize := GetSystemMetrics(SM_CYHSCROLL);

  HScroll := TCustomRangeBar.Create(Self);
  VScroll := TCustomRangeBar.Create(Self);

  with HScroll do
  begin
    HScroll.Parent := Self;
    BorderStyle := bsNone;
    Centered := True;
    OnUserChange := ScrollHandler;
    OnUserChanging := ScrollChangingHandler;
  end;

  with VScroll do
  begin
    Parent := Self;
    BorderStyle := bsNone;
    Centered := True;
    Kind := sbVertical;
    OnUserChange := ScrollHandler;
    OnUserChanging := ScrollChangingHandler;
  end;

  FCentered := True;
  ScaleMode := smScale;
  BitmapAlign := baCustom;
  with GetViewportRect do
  begin
    OldSzX := Right - Left;
    OldSzY := Bottom - Top;
  end;

  FScrollBars := TIVScrollProperties.Create;
  FScrollBars.ImgView := Self;
  FScrollBars.Master := HScroll;
  FScrollBars.Slave := VScroll;

  AlignAll;
end;

destructor TCustomImgView32.Destroy;
begin
  FreeAndNil(FScrollBars);
  inherited;
end;

procedure TCustomImgView32.DoDrawSizeGrip(R: TRect);
begin
{$IFDEF Windows}
  if USE_THEMES then
  begin
    Canvas.Brush.Color := clBtnFace;
    Canvas.FillRect(R);
    DrawThemeBackground(SCROLLBAR_THEME, Canvas.Handle, SBP_SIZEBOX, SZB_RIGHTALIGN, R, nil);
  end
  else
    DrawFrameControl(Canvas.Handle, R, DFC_SCROLL, DFCS_SCROLLSIZEGRIP)
{$ENDIF}
end;

procedure TCustomImgView32.DoScaleChange;
begin
  inherited;
  InvalidateCache;
  UpdateScrollBars;
  UpdateImage;
  Invalidate;
end;

procedure TCustomImgView32.DoScroll;
begin
  if Assigned(FOnScroll) then
    FOnScroll(Self);
end;

procedure TCustomImgView32.DoSetPivot(const APivot: TFloatPoint);
begin
  inherited;

  HScroll.Position := Round(Oversize * Scale)-APivot.X;
  VScroll.Position := Round(Oversize * Scale)-APivot.Y;
end;

function TCustomImgView32.GetScrollBarSize: Integer;
begin
  if GetScrollBarsVisible then
  begin
    Result := FScrollBarSize;
    if (Result = 0) then
      Result := GetSystemMetrics(SM_CYHSCROLL);
  end
  else
    Result := 0;
end;

function TCustomImgView32.GetScrollBarsVisible: Boolean;
begin
  if AutoSize then
  begin
    Result := False;
    Exit;
  end;

  Result := True;
  if (FScrollBars <> nil) and (HScroll <> nil) and (VScroll <> nil) then
    case FScrollBars.Visibility of
      svAlways:
        Result := True;

      svHidden:
        Result := False;

      svAuto:
        Result := (BitmapAlign = baCustom) and (ScaleMode in [smScale,smNormal]) and
                  ((HScroll.Range > (TRangeBarAccess(HScroll).EffectiveWindow + VScroll.Width)) or
                   (VScroll.Range > (TRangeBarAccess(VScroll).EffectiveWindow + HScroll.Height)));
    end;
end;

function TCustomImgView32.GetSizeGripRect: TRect;
var
  Sz: Integer;
begin
  Sz := GetScrollBarSize;

  if (Parent = nil) then
    Result := BoundsRect
  else
    Result := ClientRect;

  with Result do
  begin
    Left := Right - Sz;
    Top := Bottom - Sz;
  end;
end;

function TCustomImgView32.GetViewportRect: TRect;
var
  Sz: Integer;
begin
  Result := Rect(0, 0, Width, Height);
  Sz := GetScrollBarSize;
  Dec(Result.Right, Sz);
  Dec(Result.Bottom, Sz);
end;

function TCustomImgView32.IsSizeGripVisible: Boolean;
var
  P: TWinControl;
begin
  case SizeGrip of
    sgAuto:
      begin
        Result := False;
        if (Align <> alClient) then
          Exit;
        P := Parent;
        while True do
        begin
          if P is TCustomForm then
          begin
            Result := True;
            Break;
          end else
          if (P = nil) or (P.Align <> alClient) then
            Exit;

          P := P.Parent;
        end;
      end;

    sgNone:
      Result := False

  else { sgAlways }
    Result := True;
  end;
end;

procedure TCustomImgView32.Loaded;
begin
  AlignAll;
  Invalidate;
  UpdateScrollBars;
  if Centered then
    ScrollToCenter(Bitmap.Width div 2, Bitmap.Height div 2);
  inherited;
end;

procedure TCustomImgView32.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
{$IFNDEF PLATFORM_INDEPENDENT}
var
  P: TPoint;
  Action: Cardinal;
  Msg: TMessage;
{$ENDIF}
begin
{$IFNDEF PLATFORM_INDEPENDENT}
  P.X := X;
  P.Y := Y;

  if IsSizeGripVisible and (Owner is TCustomForm) and GR32.PtInRect(GetSizeGripRect, P) then
  begin
    Action := HTBOTTOMRIGHT;
    Application.ProcessMessages;
    Msg.Msg := WM_NCLBUTTONDOWN;
    Msg.WParam := Action;
    SetCaptureControl(nil);
    SendMessage(TCustomForm(Owner).Handle, Msg.Msg, Msg.wParam, Msg.lParam);
    Exit;
  end;
{$ENDIF}

  inherited;
end;

procedure TCustomImgView32.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  inherited;

  if IsSizeGripVisible then
  begin
    P.X := X;
    P.Y := Y;

    if GR32.PtInRect(GetSizeGripRect, P) then
      Screen.Cursor := crSizeNWSE;
  end;
end;

procedure TCustomImgView32.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer; Layer: TCustomLayer);
begin
  inherited;
end;

procedure TCustomImgView32.Paint;
begin
  if (Parent = nil) then
    Exit;

  if IsSizeGripVisible then
    DoDrawSizeGrip(GetSizeGripRect)
  else
  begin
    Canvas.Brush.Color := clBtnFace;
    Canvas.FillRect(GetSizeGripRect);
  end;
  inherited;
end;

procedure TCustomImgView32.Resize;
begin
  AlignAll;

  if (Parent <> nil) then
  begin
    if IsSizeGripVisible then
      DoDrawSizeGrip(GetSizeGripRect)
    else
    begin
      Canvas.Brush.Color := clBtnFace;
      Canvas.FillRect(GetSizeGripRect);
    end;
  end;

  InvalidateCache;
  UpdateScrollBars;
  UpdateImage;
  Invalidate;
  inherited;
end;

procedure TCustomImgView32.Scroll(Dx, Dy: Single);
begin
  if (IsZero(Dx)) and (IsZero(Dy)) then
    Exit;

  BeginUpdate;
  try

    Inc(FScrollLock);
    HScroll.Position := HScroll.Position + Dx;
    VScroll.Position := VScroll.Position + Dy;
    Dec(FScrollLock);

  finally
    EndUpdate;
  end;

  UpdateImage;
end;

procedure TCustomImgView32.ScrollHandler(Sender: TObject);
begin
  if (FScrollLock > 0) then
    Exit;

  if (Sender = HScroll) then
    HScroll.Repaint
  else
  if (Sender = VScroll) then
    VScroll.Repaint;

  UpdateImage;
  DoScroll;
  Repaint;
end;

procedure TCustomImgView32.ScrollChangingHandler(Sender: TObject; ANewPosition: Single; var Handled: boolean);
begin
  if (Sender = HScroll) then
    Scroll(ANewPosition - HScroll.Position, 0)
  else
  if (Sender = VScroll) then
    Scroll(0, ANewPosition - VScroll.Position);

  Handled := True;

  DoScroll;
end;

procedure TCustomImgView32.ScrollToCenter(X, Y: Integer);
var
  ScaledDOversize: Integer;
begin
  Inc(FScrollLock);
  AlignAll;

  ScaledDOversize := Round(FOversize * Scale);
  with GetViewportRect do
  begin
    HScroll.Position := X * Scale - (Right - Left) * 0.5 + ScaledDOversize;
    VScroll.Position := Y * Scale - (Bottom - Top) * 0.5 + ScaledDOversize;
  end;
  Dec(FScrollLock);
  UpdateImage;
end;

procedure TCustomImgView32.Recenter;
begin
  InvalidateCache;
  HScroll.Centered := FCentered;
  VScroll.Centered := FCentered;
  UpdateScrollBars;
  UpdateImage;
  if FCentered then
    ScrollToCenter(Bitmap.Width div 2, Bitmap.Height div 2)
  else
    ScrollToCenter(0, 0);
end;

procedure TCustomImgView32.SetCentered(Value: Boolean);
begin
  FCentered := Value;
  Recenter;
end;

procedure TCustomImgView32.SetOverSize(const Value: Integer);
begin
  if Value <> FOverSize then
  begin
    FOverSize := Value;
    Invalidate;
  end;
end;

procedure TCustomImgView32.SetScrollBars(Value: TIVScrollProperties);
begin
  FScrollBars.Assign(Value);
end;

procedure TCustomImgView32.SetSizeGrip(Value: TSizeGripStyle);
begin
  if Value <> FSizeGrip then
  begin
    FSizeGrip := Value;
    Invalidate;
  end;
end;

procedure TCustomImgView32.UpdateImage;
var
  Sz: TSize;
  W, H: Integer;
  ScaledOversize: Integer;
begin
  Sz := GetBitmapSize;
  ScaledOversize := Round(FOversize * Scale);

  with GetViewportRect do
  begin
    W := Right - Left;
    H := Bottom - Top;
  end;

  BeginUpdate;
  try
    if Centered then
    begin
      if W > Sz.Cx + 2 * ScaledOversize then // Viewport is bigger than scaled Bitmap
        OffsetHorz := (W - Sz.Cx) * 0.5
      else
        OffsetHorz := -HScroll.Position + ScaledOversize;

      if H > Sz.Cy + 2 * ScaledOversize then // Viewport is bigger than scaled Bitmap
        OffsetVert := (H - Sz.Cy) * 0.5
      else
        OffsetVert := -VScroll.Position + ScaledOversize;
    end else
    begin
      OffsetHorz := -HScroll.Position + ScaledOversize;
      OffsetVert := -VScroll.Position + ScaledOversize;
    end;

    InvalidateCache;
  finally
    EndUpdate;
  end;
end;

procedure TCustomImgView32.UpdateScrollBars;
var
  Sz: TSize;
  ScaledDOversize: Integer;
begin
  if (HScroll <> nil) and (VScroll <> nil) then
  begin
    Sz := GetBitmapSize;
    ScaledDOversize := Round(2 * FOversize * Scale);

    HScroll.Range := Sz.Cx + ScaledDOversize;
    VScroll.Range := Sz.Cy + ScaledDOversize;

    // call AlignAll for Visibility svAuto, because the ranges of the scrollbars
    // may have just changed, thus we need to update the visibility of the scrollbars:
    if (FScrollBarVisibility = svAuto) then
      AlignAll;
  end;
end;

procedure TCustomImgView32.SetScaleMode(Value: TScaleMode);
begin
  inherited;
  Recenter;
end;

{ TBitmap32Item }

procedure TBitmap32Item.AssignTo(Dest: TPersistent);
begin
  if Dest is TBitmap32Item then
    TBitmap32Item(Dest).Bitmap.Assign(Bitmap)
  else
    inherited;
end;

constructor TBitmap32Item.Create(Collection: TCollection);
begin
  inherited;
  FBitmap := TBitmap32.Create;
end;

destructor TBitmap32Item.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

procedure TBitmap32Item.SetBitmap(ABitmap: TBitmap32);
begin
  FBitmap.Assign(ABitmap)
end;




{ TBitmap32Collection }

function TBitmap32Collection.Add: TBitmap32Item;
begin
  Result := TBitmap32Item(inherited Add);
end;

constructor TBitmap32Collection.Create(AOwner: TPersistent; ItemClass: TBitmap32ItemClass);
begin
  inherited Create(ItemClass);
  FOwner := AOwner;
end;

function TBitmap32Collection.GetItem(Index: Integer): TBitmap32Item;
begin
  Result := TBitmap32Item(inherited GetItem(Index));
end;

function TBitmap32Collection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TBitmap32Collection.SetItem(Index: Integer; Value: TBitmap32Item);
begin
  inherited SetItem(Index, Value);
end;




{ TBitmap32List }

constructor TBitmap32List.Create(AOwner: TComponent);
begin
  inherited;
  FBitmap32Collection := TBitmap32Collection.Create(Self, TBitmap32Item);
end;

destructor TBitmap32List.Destroy;
begin
  FBitmap32Collection.Free;
  inherited;
end;

function TBitmap32List.GetBitmap(Index: Integer): TBitmap32;
begin
  Result := FBitmap32Collection.Items[Index].Bitmap;
end;

procedure TBitmap32List.SetBitmap(Index: Integer; Value: TBitmap32);
begin
  FBitmap32Collection.Items[Index].Bitmap := Value;
end;

procedure TBitmap32List.SetBitmap32Collection(Value: TBitmap32Collection);
begin
  FBitmap32Collection := Value;
end;

{ TMousePanOptions }

constructor TMousePanOptions.Create;
begin
  inherited Create;
  FMouseButton := mbLeft;
  FPanCursor := crSizeAll;
end;

function TMousePanOptions.MatchShiftState(AShiftState: TShiftState): boolean;
begin
{$ifdef FPC}
  {-$push}
  {-$packset 4} // The FPC TShiftState does not fit in a word
  Result := (DWord(AShiftState * [ssShift, ssAlt, ssCtrl]) = DWord(Byte(FShiftState)));
  {-$pop}
{$else FPC}
  Result := (Word(AShiftState * [ssShift, ssAlt, ssCtrl]) = Word(Byte(FShiftState)));
{$endif FPC}
end;

{ TMouseZoomOptions }

constructor TMouseZoomOptions.Create;
begin
  inherited Create;
  FSteps := 12;
  FMinScale := 0.0625;
  FMaxScale := 128.0;
  FMaintainPivot := True;
  UpdateZoomFactor;
end;

procedure TMouseZoomOptions.SetSteps(const Value: integer);
begin
  FSteps := Min(100, Max(2, Value));
  UpdateZoomFactor;
end;

procedure TMouseZoomOptions.SetZoomFactor(const Value: Double);
begin
  FZoomFactor := Min(100, Max(1.001, Value));

  // Calculate steps and recalculate FZoomFactor
  Steps := Ceil(Ln(FMaxScale / FMinScale) / Ln(FZoomFactor)) + 1;
end;

procedure TMouseZoomOptions.UpdateZoomFactor;
begin
  FZoomFactor := Power(FMaxScale / FMinScale, 1/(FSteps-1));
end;

function TMouseZoomOptions.IsMaxScaleStored: Boolean;
begin
  Result := (FMinScale <> 0.0625);
end;

function TMouseZoomOptions.IsMinScaleStored: Boolean;
begin
  Result := (FMaxScale <> 128.0);
end;

function TMouseZoomOptions.LevelToScale(ALevel: integer): Single;
var
  LogMinZoom: Double;
  LogMaxZoom: Double;
  LogZoom: Double;
begin
  ALevel := Max(0, Min(ALevel, FSteps-1));
  // Work in log space...
  LogMinZoom := Ln(FMinScale);
  LogMaxZoom := Ln(FMaxScale);
  // Linear interpolation in log space:
  LogZoom := LogMinZoom + (LogMaxZoom-LogMinZoom) / (FSteps-1) * ALevel;
  // Back to linear space
  Result := Exp(LogZoom);
end;

function TMouseZoomOptions.MatchShiftState(AShiftState: TShiftState): boolean;
begin
  {$ifdef FPC}
    {-$push}
    {-$packset 4} // The FPC TShiftState does not fit in a word
    Result := (DWord(AShiftState * [ssShift, ssAlt, ssCtrl]) = DWord(Byte(FShiftState)));
    {-$pop}
  {$else FPC}
    Result := (Word(AShiftState * [ssShift, ssAlt, ssCtrl]) = Word(Byte(FShiftState)));
  {$endif FPC}
end;

function TMouseZoomOptions.ScaleToLevel(AScale: Single): integer;
var
  LogMinZoom: Double;
  LogMaxZoom: Double;
  Step: Double;
begin
  AScale := Max(FMinScale, Min(AScale, FMaxScale));
  // Work in log space...
  LogMinZoom := Ln(FMinScale);
  LogMaxZoom := Ln(FMaxScale);
  // Linear interpolation in log space:
  Step := (FSteps-1) / (LogMaxZoom-LogMinZoom) * (Ln(AScale)-LogMinZoom);
  // Back to linear space
  Result := Ceil(Step);
  Result := Max(0, Min(Result, FSteps-1));
end;

procedure TMouseZoomOptions.SetMaxScale(const Value: Single);
begin
  FMaxScale := Max(FMinScale+0.001, Value);
  UpdateZoomFactor;
end;

procedure TMouseZoomOptions.SetMinScale(const Value: Single);
begin
  FMinScale := Min(FMaxScale-0.002, Max(0.001, Value));
  UpdateZoomFactor;
end;

end.
