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

{$I GR32.inc}

uses
{$IFDEF FPC}
  LCLIntf, LCLType, LMessages, Types,
{$ELSE}
  Windows, Messages,
{$ENDIF}
  Graphics, Controls, Forms,
  Classes, SysUtils, GR32, GR32_Layers, GR32_RangeBars, GR32_LowLevel,
  GR32_System, GR32_Containers, GR32_RepaintOpt;

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
  PPaintStage = ^TPaintStage;
  TPaintStage = record
    DsgnTime: Boolean;
    RunTime: Boolean;
    Stage: Cardinal;             // a PST_* constant
    Parameter: Cardinal;         // an optional parameter
  end;

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
  private
    FBuffer: TBitmap32;
    FBufferOversize: Integer;
    FBufferValid: Boolean;
    FRepaintMode: TRepaintMode;
    FInvalidRects: TRectList;
    FForceFullRepaint: Boolean;
    FRepaintOptimizer: TCustomRepaintOptimizer;
    FOptions: TPaintBoxOptions;
    FOnGDIOverlay: TNotifyEvent;
    FMouseInControl: Boolean;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    procedure SetBufferOversize(Value: Integer);
{$IFDEF FPC}
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
    procedure WMGetDlgCode(var Msg: TLMessage); message LM_GETDLGCODE;
    procedure WMPaint(var Message: TLMPaint); message LM_PAINT;
    procedure CMMouseEnter(var Message: TLMessage); message LM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TLMessage); message LM_MOUSELEAVE;
    procedure CMInvalidate(var Message: TLMessage); message CM_INVALIDATE;
{$ELSE}
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Msg: TWmGetDlgCode); message WM_GETDLGCODE;
    procedure WMPaint(var Message: TMessage); message WM_PAINT;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMInvalidate(var Message: TMessage); message CM_INVALIDATE;
{$ENDIF}
    procedure DirectAreaUpdateHandler(Sender: TObject; const Area: TRect; const Info: Cardinal);
  protected
    procedure SetRepaintMode(const Value: TRepaintMode); virtual;
    function  CustomRepaint: Boolean; virtual;
    function  InvalidRectsAvailable: Boolean; virtual;
    procedure DoPrepareInvalidRects; virtual;
    procedure DoPaintBuffer; virtual;
    procedure DoPaintGDIOverlay; virtual;
    procedure DoBufferResized(const OldWidth, OldHeight: Integer); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter; virtual;
    procedure MouseLeave; virtual;
    procedure Paint; override;
    procedure ResetInvalidRects;
    procedure ResizeBuffer;
    property  RepaintOptimizer: TCustomRepaintOptimizer read FRepaintOptimizer;
    property  BufferValid: Boolean read FBufferValid write FBufferValid;
    property  InvalidRects: TRectList read FInvalidRects;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function  GetViewportRect: TRect; virtual;
    procedure Flush; overload;
    procedure Flush(const SrcRect: TRect); overload;
    procedure Invalidate; override;
    procedure ForceFullInvalidate; virtual;
    procedure Loaded; override;
    procedure Resize; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure AssignTo(Dest: TPersistent); override;
    property Buffer: TBitmap32 read FBuffer;
    property BufferOversize: Integer read FBufferOversize write SetBufferOversize;
    property Options: TPaintBoxOptions read FOptions write FOptions default [];
    property MouseInControl: Boolean read FMouseInControl;
    property RepaintMode: TRepaintMode read FRepaintMode write SetRepaintMode default rmFull;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnGDIOverlay: TNotifyEvent read FOnGDIOverlay write FOnGDIOverlay;
  end;

  { TPaintBox32 }
  TPaintBox32 = class(TCustomPaintBox32)
  private
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

  TCustomImage32 = class(TCustomPaintBox32)
  private
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
    FUpdateCount: Integer;
    FOnBitmapResize: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FOnInitStages: TNotifyEvent;
    FOnMouseDown: TImgMouseEvent;
    FOnMouseMove: TImgMouseMoveEvent;
    FOnMouseUp: TImgMouseEvent;
    FOnPaintStage: TPaintStageEvent;
    FOnScaleChange: TNotifyEvent;
    procedure BitmapResizeHandler(Sender: TObject);
    procedure BitmapChangeHandler(Sender: TObject);
    procedure BitmapAreaChangeHandler(Sender: TObject; const Area: TRect; const Info: Cardinal);
    procedure BitmapDirectAreaChangeHandler(Sender: TObject; const Area: TRect; const Info: Cardinal);
    procedure LayerCollectionChangeHandler(Sender: TObject);
    procedure LayerCollectionGDIUpdateHandler(Sender: TObject);
    procedure LayerCollectionGetViewportScaleHandler(Sender: TObject; var ScaleX, ScaleY: TFloat);
    procedure LayerCollectionGetViewportShiftHandler(Sender: TObject; var ShiftX, ShiftY: TFloat);
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
  protected
    CachedBitmapRect: TRect;
    CachedShiftX, CachedShiftY,
    CachedScaleX, CachedScaleY,
    CachedRecScaleX, CachedRecScaleY: TFloat;
    CacheValid: Boolean;
    OldSzX, OldSzY: Integer;
    PaintToMode: Boolean;
    procedure BitmapResized; virtual;
    procedure BitmapChanged(const Area: TRect); reintroduce; virtual;
    function  CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure DoInitStages; virtual;
    procedure DoPaintBuffer; override;
    procedure DoPaintGDIOverlay; override;
    procedure DoScaleChange; virtual;
    procedure InitDefaultStages; virtual;
    procedure InvalidateCache;
    function  InvalidRectsAvailable: Boolean; override;
    procedure DblClick; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); overload; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); overload; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); overload; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer); reintroduce; overload; virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer); reintroduce; overload; virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer); reintroduce; overload; virtual;
    procedure MouseLeave; override;
    procedure SetRepaintMode(const Value: TRepaintMode); override;
    procedure SetScaleMode(Value: TScaleMode); virtual;
    procedure SetXForm(ShiftX, ShiftY, ScaleX, ScaleY: TFloat);
    procedure UpdateCache; virtual;
    property  UpdateCount: Integer read FUpdateCount;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate; virtual;
    function  BitmapToControl(const APoint: TPoint): TPoint; overload;
    function  BitmapToControl(const APoint: TFloatPoint): TFloatPoint; overload;
    procedure Changed; virtual;
    procedure Update(const Rect: TRect); reintroduce; overload; virtual;
    function  ControlToBitmap(const APoint: TPoint): TPoint;  overload;
    function  ControlToBitmap(const APoint: TFloatPoint): TFloatPoint; overload;
    procedure EndUpdate; virtual;
    procedure ExecBitmapFrame(Dest: TBitmap32; StageNum: Integer); virtual;   // PST_BITMAP_FRAME
    procedure ExecClearBuffer(Dest: TBitmap32; StageNum: Integer); virtual;   // PST_CLEAR_BUFFER
    procedure ExecClearBackgnd(Dest: TBitmap32; StageNum: Integer); virtual;  // PST_CLEAR_BACKGND
    procedure ExecControlFrame(Dest: TBitmap32; StageNum: Integer); virtual;  // PST_CONTROL_FRAME
    procedure ExecCustom(Dest: TBitmap32; StageNum: Integer); virtual;        // PST_CUSTOM
    procedure ExecDrawBitmap(Dest: TBitmap32; StageNum: Integer); virtual;    // PST_DRAW_BITMAP
    procedure ExecDrawLayers(Dest: TBitmap32; StageNum: Integer); virtual;    // PST_DRAW_LAYERS
    function  GetBitmapRect: TRect; virtual;
    function  GetBitmapSize: TSize; virtual;
    procedure Invalidate; override;
    procedure Loaded; override;
    procedure PaintTo(Dest: TBitmap32; DestRect: TRect); virtual;
    procedure Resize; override;
    procedure SetupBitmap(DoClear: Boolean = False; ClearColor: TColor32 = $FF000000); virtual;
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
    property OnBitmapResize: TNotifyEvent read FOnBitmapResize write FOnBitmapResize;
    property OnBitmapPixelCombine: TPixelCombineEvent read GetOnPixelCombine write SetOnPixelCombine;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
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
    property ShowHint;
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
  private
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
    DisableScrollUpdate: Boolean;
    HScroll: TCustomRangeBar;
    VScroll: TCustomRangeBar;
    procedure AlignAll;
    procedure BitmapResized; override;
    procedure DoDrawSizeGrip(R: TRect);
    procedure DoScaleChange; override;
    procedure DoScroll; virtual;
    function  GetScrollBarsVisible: Boolean;
    function  GetScrollBarSize: Integer;
    function  GetSizeGripRect: TRect;
    function  IsSizeGripVisible: Boolean;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure Recenter;
    procedure SetScaleMode(Value: TScaleMode); override;
    procedure ScrollHandler(Sender: TObject); virtual;
    procedure UpdateImage; virtual;
    procedure UpdateScrollBars; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function  GetViewportRect: TRect; override;
    procedure Loaded; override;
    procedure Resize; override;
    procedure ScrollToCenter(X, Y: Integer);
    procedure Scroll(Dx, Dy: Integer);
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

implementation

uses
  TypInfo, GR32_MicroTiles, GR32_Backends, GR32_XPThemes;

type
  TBitmap32Access = class(TBitmap32);
  TLayerAccess = class(TCustomLayer);
  TLayerCollectionAccess = class(TLayerCollection);
  TRangeBarAccess = class(TRangeBar);

const
  DefaultRepaintOptimizerClass: TCustomRepaintOptimizerClass = TMicroTilesRepaintOptimizer;

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
    DsgnTime := False;
    RunTime := True;
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
  Count: Integer;
begin
  if (Index < 0) or (Index > High(FItems)) then
    raise EListError.Create('Invalid stage index');
  Count := Length(FItems) - Index - 1;
  if Count > 0 then
    Move(FItems[Index + 1], FItems[Index], Count * SizeOf(TPaintStage));
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
  Count: Integer;
begin
  if Index < 0 then Index := 0
  else if Index > Length(FItems) then Index := Length(FItems);
  Count := Length(FItems) - Index;
  SetLength(FItems, Length(FItems) + 1);
  if Count > 0 then
    Move(FItems[Index], FItems[Index + 1], Count * SizeOf(TPaintStage));
  Result := @FItems[Index];
  with Result^ do
  begin
    DsgnTime := False;
    RunTime := True;
    Stage := 0;
    Parameter := 0;
  end;
end;


{ TCustomPaintBox32 }

{$IFDEF FPC}
procedure TCustomPaintBox32.CMInvalidate(var Message: TLMessage);
begin
  if CustomRepaint and HandleAllocated then
    PostMessage(Handle, LM_PAINT, 0, 0)
  else
    inherited;
end;
{$ELSE}

procedure TCustomPaintBox32.CMInvalidate(var Message: TMessage);
begin
  if CustomRepaint and HandleAllocated then
    // we might have invalid rects, so just go ahead without invalidating
    // the whole client area...
    PostMessage(Handle, WM_PAINT, 0, 0)
  else
    // no invalid rects, so just invalidate the whole client area...
    inherited;
end;
{$ENDIF}

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

constructor TCustomPaintBox32.Create(AOwner: TComponent);
begin
  inherited;
  FBuffer := TBitmap32.Create;
  FBufferOversize := 40;
  FForceFullRepaint := True;
  FInvalidRects := TRectList.Create;
  FRepaintOptimizer := DefaultRepaintOptimizerClass.Create(Buffer, InvalidRects);

  { Setting a initial size here will cause the control to crash under LCL }
{$IFNDEF FPC}
  Height := 192;
  Width := 192;
{$ENDIF}
end;

destructor TCustomPaintBox32.Destroy;
begin
  FRepaintOptimizer.Free;
  FInvalidRects.Free;
  FBuffer.Free;
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
  if Assigned(FOnGDIOverlay) then FOnGDIOverlay(Self);
end;

procedure TCustomPaintBox32.Flush;
begin
  if (Canvas.Handle <> 0) and (FBuffer.Handle <> 0) then
  begin
    Canvas.Lock;
    try
      FBuffer.Lock;
      try
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
  if (Canvas.Handle <> 0) and (FBuffer.Handle <> 0) then
  begin
    Canvas.Lock;
    try
      FBuffer.Lock;
      try
        R := GetViewPortRect;
        with SrcRect do
          BitBlt(Canvas.Handle, Left + R.Left, Top + R.Top, Right - Left, Bottom - Top,
            FBuffer.Handle, Left, Top, SRCCOPY);
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
  with Result do
  begin
    // by default, the whole control is buffered
    Left := 0;
    Top := 0;
    Right := Width;
    Bottom := Height;
  end;
end;

procedure TCustomPaintBox32.Invalidate;
begin
  FBufferValid := False;
  inherited;
end;

procedure TCustomPaintBox32.ForceFullInvalidate;
begin
  if FRepaintOptimizer.Enabled then FRepaintOptimizer.Reset;
  FForceFullRepaint := True;
  Invalidate;
end;

procedure TCustomPaintBox32.Loaded;
begin
  FBufferValid := False;
  inherited;
end;

procedure TCustomPaintBox32.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (pboAutoFocus in Options) and CanFocus then SetFocus;
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
begin
  if not Assigned(Parent) then
    Exit;

  if FRepaintOptimizer.Enabled then
  begin
    FRepaintOptimizer.BeginPaint;
  end;

  if not FBufferValid then
  begin
    (FBuffer.Backend as IPaintSupport).ImageNeeded;
    DoPaintBuffer;
    (FBuffer.Backend as IPaintSupport).CheckPixmap;
  end;

  FBuffer.Lock;
  with Canvas do
  try
    (FBuffer.Backend as IPaintSupport).DoPaint(FBuffer, FInvalidRects, Canvas, Self);
  finally
    FBuffer.Unlock;
  end;

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
  if NewWidth < 0 then NewWidth := 0;
  if NewHeight < 0 then NewHeight := 0;

  W := FBuffer.Width;

  if NewWidth > W then
    W := NewWidth + FBufferOversize
  else if NewWidth < W - FBufferOversize then
    W := NewWidth;

  if W < 1 then W := 1;

  H := FBuffer.Height;

  if NewHeight > H then
    H := NewHeight + FBufferOversize
  else if NewHeight < H - FBufferOversize then
    H := NewHeight;

  if H < 1 then H := 1;

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
  if csDesigning in ComponentState then ResizeBuffer;
  FBufferValid := False;
end;

procedure TCustomPaintBox32.SetBufferOversize(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if Value <> FBufferOversize then
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
  with Msg do if pboWantArrowKeys in Options then
    Result:= Result or DLGC_WANTARROWS
  else
    Result:= Result and not DLGC_WANTARROWS;
end;

procedure TCustomPaintBox32.WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TMessage{$ENDIF});
begin
  if CustomRepaint then
  begin
    if InvalidRectsAvailable then
      // BeginPaint deeper might set invalid clipping, so we call Paint here
      // to force repaint of our invalid rects...
      Paint
    else
      // no invalid rects available? Invalidate the whole client area
      InvalidateRect(Handle, nil, False);
  end;
  
  {$IFDEF FPC}
  { On FPC we need to specify the name of the ancestor here }
  inherited WMPaint(Message);
  {$ELSE}
  inherited;
  {$ENDIF}
end;

procedure TCustomPaintBox32.DirectAreaUpdateHandler(Sender: TObject;
  const Area: TRect; const Info: Cardinal);
begin
  FInvalidRects.Add(Area);
  if not(csCustomPaint in ControlState) then Repaint;
end;

procedure TCustomPaintBox32.SetRepaintMode(const Value: TRepaintMode);
begin
  if Assigned(FRepaintOptimizer) then
  begin
    // setup event handler on change of area
    if (Value = rmOptimizer) and not(Self is TCustomImage32) then
      FBuffer.OnAreaChanged := FRepaintOptimizer.AreaUpdateHandler
    else if Value = rmDirect then
      FBuffer.OnAreaChanged := DirectAreaUpdateHandler
    else
      FBuffer.OnAreaChanged := nil;

    FRepaintOptimizer.Enabled := Value = rmOptimizer;

    FRepaintMode := Value;
    Invalidate;
  end;
end;


{ TPaintBox32 }

procedure TPaintBox32.DoPaintBuffer;
begin
  if Assigned(FOnPaintBuffer) then FOnPaintBuffer(Self);
  inherited;
end;

{ TCustomImage32 }

procedure TCustomImage32.BeginUpdate;
begin
  // disable OnChange & OnChanging generation
  Inc(FUpdateCount);
end;

procedure TCustomImage32.BitmapResized;
var
  W, H: Integer;
begin
  if AutoSize then
  begin
    W := Bitmap.Width;
    H := Bitmap.Height;
    if ScaleMode = smScale then
    begin
      W := Round(W * Scale);
      H := Round(H * Scale);
    end;
    if AutoSize and (W > 0) and (H > 0) then SetBounds(Left, Top, W, H);
  end;

  if (FUpdateCount = 0) and Assigned(FOnBitmapResize) then FOnBitmapResize(Self);
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

function TCustomImage32.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
var
  W, H: Integer;
begin
  InvalidateCache;
  Result := True;
  W := Bitmap.Width;
  H := Bitmap.Height;
  if ScaleMode = smScale then
  begin
    W := Round(W * Scale);
    H := Round(H * Scale);
  end;
  if not (csDesigning in ComponentState) or (W > 0) and (H > 0) then
  begin
    if Align in [alNone, alLeft, alRight] then NewWidth := W;
    if Align in [alNone, alTop, alBottom] then NewHeight := H;
  end;
end;

procedure TCustomImage32.Changed;
begin
  if FUpdateCount = 0 then
  begin
    Invalidate;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TCustomImage32.Update(const Rect: TRect);
begin
  if FRepaintOptimizer.Enabled then
    FRepaintOptimizer.AreaUpdateHandler(Self, Rect, AREAINFO_RECT);
end;

procedure TCustomImage32.BitmapResizeHandler(Sender: TObject);
begin
  BitmapResized;
end;

procedure TCustomImage32.BitmapChangeHandler(Sender: TObject);
begin
  FRepaintOptimizer.Reset;
  BitmapChanged(Bitmap.Boundsrect);
end;

procedure TCustomImage32.BitmapAreaChangeHandler(Sender: TObject; const Area: TRect; const Info: Cardinal);
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
      FRepaintOptimizer.AreaUpdateHandler(Self, T, AREAINFO_RECT)
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
            OffsetRect(R, Right * I, Bottom * J);
            FRepaintOptimizer.AreaUpdateHandler(Self, R, AREAINFO_RECT);
          end;
      end;
    end;
  end;

  BitmapChanged(Area);
end;

procedure TCustomImage32.BitmapDirectAreaChangeHandler(Sender: TObject; const Area: TRect; const Info: Cardinal);
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
            OffsetRect(R, Right * I, Bottom * J);
            InvalidRects.Add(R);
          end;
      end;
    end;
  end;

  if FUpdateCount = 0 then
  begin
    if not(csCustomPaint in ControlState) then Repaint;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TCustomImage32.LayerCollectionChangeHandler(Sender: TObject);
begin
  Changed;
end;

procedure TCustomImage32.LayerCollectionGDIUpdateHandler(Sender: TObject);
begin
  Paint;
end;

procedure TCustomImage32.LayerCollectionGetViewportScaleHandler(Sender: TObject; var ScaleX, ScaleY: TFloat);
begin
  UpdateCache;
  ScaleX := CachedScaleX;
  ScaleY := CachedScaleY;
end;

procedure TCustomImage32.LayerCollectionGetViewportShiftHandler(Sender: TObject; var ShiftX, ShiftY: TFloat);
begin
  UpdateCache;
  ShiftX := CachedShiftX;
  ShiftY := CachedShiftY;
end;

function TCustomImage32.ControlToBitmap(const APoint: TPoint): TPoint;
begin
  // convert point coords from control's ref. frame to bitmap's ref. frame
  // the coordinates are not clipped to bitmap image boundary
  UpdateCache;
  with APoint do
  begin
    Result.X := Trunc((X - CachedShiftX) * CachedRecScaleX);
    Result.Y := Trunc((Y - CachedShiftY) * CachedRecScaleY);
  end;
end;

function TCustomImage32.ControlToBitmap(const APoint: TFloatPoint): TFloatPoint;
begin
  // subpixel precision version
  UpdateCache;
  with APoint do
  begin
    Result.X := (X - CachedShiftX) * CachedRecScaleX;
    Result.Y := (Y - CachedShiftY) * CachedRecScaleY;
  end;
end;


constructor TCustomImage32.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csDoubleClicks, csReplicatable, csOpaque];
  FBitmap := TBitmap32.Create;
  FBitmap.OnResize := BitmapResizeHandler;

  FLayers := TLayerCollection.Create(Self);
  with TLayerCollectionAccess(FLayers) do
  begin
    OnChange := LayerCollectionChangeHandler;
    OnGDIUpdate := LayerCollectionGDIUpdateHandler;
    OnGetViewportScale := LayerCollectionGetViewportScaleHandler;
    OnGetViewportShift := LayerCollectionGetViewportShiftHandler;
  end;

  FRepaintOptimizer.RegisterLayerCollection(FLayers);
  RepaintMode := rmFull;

  FPaintStages := TPaintStages.Create;
  FScaleX := 1;
  FScaleY := 1;
  SetXForm(0, 0, 1, 1);

  InitDefaultStages;
end;

procedure TCustomImage32.DblClick;
begin
  Layers.MouseListener := nil;
  MouseUp(mbLeft, [], 0, 0);
  inherited;
end;

destructor TCustomImage32.Destroy;
begin
  BeginUpdate;
  FPaintStages.Free;
  FRepaintOptimizer.UnregisterLayerCollection(FLayers);
  FLayers.Free;
  FBitmap.Free;
  inherited;
end;

procedure TCustomImage32.DoInitStages;
begin
  if Assigned(FOnInitStages) then FOnInitStages(Self);
end;

procedure TCustomImage32.DoPaintBuffer;
var
  PaintStageHandlerCount: Integer;
  I, J: Integer;
  DT, RT: Boolean;
begin
  if FRepaintOptimizer.Enabled then
    FRepaintOptimizer.BeginPaintBuffer;

  UpdateCache;

  SetLength(FPaintStageHandlers, FPaintStages.Count);
  SetLength(FPaintStageNum, FPaintStages.Count);
  PaintStageHandlerCount := 0;

  DT := csDesigning in ComponentState;
  RT := not DT;

  // compile list of paintstage handler methods
  for I := 0 to FPaintStages.Count - 1 do
  begin
    with FPaintStages[I]^ do
      if (DsgnTime and DT) or (RunTime and RT) then
      begin
        FPaintStageNum[PaintStageHandlerCount] := I;
        case Stage of
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

  Buffer.BeginUpdate;
  if FInvalidRects.Count = 0 then
  begin
    Buffer.ClipRect := GetViewportRect;
    for I := 0 to PaintStageHandlerCount - 1 do
      FPaintStageHandlers[I](Buffer, FPaintStageNum[I]);
  end
  else
  begin
    for J := 0 to FInvalidRects.Count - 1 do
    begin
      Buffer.ClipRect := FInvalidRects[J]^;
      for I := 0 to PaintStageHandlerCount - 1 do
        FPaintStageHandlers[I](Buffer, FPaintStageNum[I]);
    end;

    Buffer.ClipRect := GetViewportRect;
  end;
  Buffer.EndUpdate;

  if FRepaintOptimizer.Enabled then
    FRepaintOptimizer.EndPaintBuffer;

  // avoid calling inherited, we have a totally different behaviour here...
  FBufferValid := True;
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
  if Assigned(FOnScaleChange) then FOnScaleChange(Self);
end;

procedure TCustomImage32.EndUpdate;
begin
  // re-enable OnChange & OnChanging generation
  Dec(FUpdateCount);
  Assert(FUpdateCount >= 0, 'Unpaired EndUpdate call');
end;

procedure TCustomImage32.ExecBitmapFrame(Dest: TBitmap32; StageNum: Integer);
begin
  Dest.Canvas.DrawFocusRect(CachedBitmapRect);
end;

procedure TCustomImage32.ExecClearBackgnd(Dest: TBitmap32; StageNum: Integer);
var
  C: TColor32;
  I: Integer;
begin
  C := Color32(Color);
  if FInvalidRects.Count > 0 then
  begin
    for I := 0 to FInvalidRects.Count - 1 do
      with FInvalidRects[I]^ do
        Dest.FillRectS(Left, Top, Right, Bottom, C);
  end
  else
  begin
  if ((Bitmap.Empty) or (Bitmap.DrawMode <> dmOpaque)) and assigned(Dest) then
    Dest.Clear(C)
  else
    with CachedBitmapRect do
    begin
      if (Left > 0) or (Right < Width) or (Top > 0) or (Bottom < Height) and
        not (BitmapAlign = baTile) then
      begin
        // clean only the part of the buffer lying around image edges
        Dest.FillRectS(0, 0, Width, Top, C);          // top
        Dest.FillRectS(0, Bottom, Width, Height, C);  // bottom
        Dest.FillRectS(0, Top, Left, Bottom, C);      // left
        Dest.FillRectS(Right, Top, Width, Bottom, C); // right
        end;
      end;
    end;
end;

procedure TCustomImage32.ExecClearBuffer(Dest: TBitmap32; StageNum: Integer);
begin
  Dest.Clear(Color32(Color));
end;

procedure TCustomImage32.ExecControlFrame(Dest: TBitmap32; StageNum: Integer);
begin
  DrawFocusRect(Dest.Handle, Rect(0, 0, Width, Height));
end;

procedure TCustomImage32.ExecCustom(Dest: TBitmap32; StageNum: Integer);
begin
  if Assigned(FOnPaintStage) then FOnPaintStage(Self, Dest, StageNum);
end;

procedure TCustomImage32.ExecDrawBitmap(Dest: TBitmap32; StageNum: Integer);
var
  I, J, Tx, Ty: Integer;
  R: TRect;
begin
  if Bitmap.Empty or IsRectEmpty(CachedBitmapRect) then Exit;
  Bitmap.Lock;
  try
    if BitmapAlign <> baTile then Bitmap.DrawTo(Dest, CachedBitmapRect)
    else with CachedBitmapRect do
    begin
      Tx := Dest.Width div Right;
      Ty := Dest.Height div Bottom;
      for J := 0 to Ty do
        for I := 0 to Tx do
        begin
          R := CachedBitmapRect;
          OffsetRect(R, Right * I, Bottom * J);
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
    with Size do
    begin
      Result := Rect(0, 0, Cx, Cy);
      if BitmapAlign = baCenter then
        OffsetRect(Result, (Width - Cx) div 2, (Height - Cy) div 2)
      else if BitmapAlign = baCustom then
        OffsetRect(Result, Round(OffsetHorz), Round(OffsetVert));
    end;
  end;
end;

function TCustomImage32.GetBitmapSize: TSize;
var
  Mode: TScaleMode;
  ViewportWidth, ViewportHeight: Integer;
  RScaleX, RScaleY: TFloat;
begin
  with Result do
  begin
    if Bitmap.Empty or (Width = 0) or (Height = 0) then
    begin
      Cx := 0;
      Cy := 0;
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
          Cx := Bitmap.Width;
          Cy := Bitmap.Height;
        end;
      smStretch:
        begin
          Cx := ViewportWidth;
          Cy := ViewportHeight;
        end;
      smResize:
        begin
          Cx := Bitmap.Width;
          Cy := Bitmap.Height;
          RScaleX := ViewportWidth / Cx;
          RScaleY := ViewportHeight / Cy;
          if RScaleX >= RScaleY then
          begin
            Cx := Round(Cx * RScaleY);
            Cy := ViewportHeight;
          end
          else
          begin
            Cx := ViewportWidth;
            Cy := Round(Cy * RScaleX);
          end;
        end;
    else // smScale
      begin
        Cx := Round(Bitmap.Width * ScaleX);
        Cy := Round(Bitmap.Height * ScaleY);
      end;
    end;
    if Cx <= 0 then Cx := 0;
    if Cy <= 0 then Cy := 0;
  end;
end;

function TCustomImage32.GetOnPixelCombine: TPixelCombineEvent;
begin
  Result := FBitmap.OnPixelCombine;
end;

procedure TCustomImage32.InitDefaultStages;
begin
  // background
  with PaintStages.Add^ do
  begin
    DsgnTime := True;
    RunTime := True;
    Stage := PST_CLEAR_BACKGND;
  end;

  // control frame
  with PaintStages.Add^ do
  begin
    DsgnTime := True;
    RunTime := False;
    Stage := PST_CONTROL_FRAME;
  end;

  // bitmap
  with PaintStages.Add^ do
  begin
    DsgnTime := True;
    RunTime := True;
    Stage := PST_DRAW_BITMAP;
  end;

  // bitmap frame
  with PaintStages.Add^ do
  begin
    DsgnTime := True;
    RunTime := False;
    Stage := PST_BITMAP_FRAME;
  end;

  // layers
  with PaintStages.Add^ do
  begin
    DsgnTime := True;
    RunTime := True;
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

procedure TCustomImage32.InvalidateCache;
begin
  if FRepaintOptimizer.Enabled then FRepaintOptimizer.Reset;
  CacheValid := False;
end;

procedure TCustomImage32.Loaded;
begin
  inherited;
  DoInitStages;
end;

procedure TCustomImage32.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Layer: TCustomLayer;
begin
  inherited;

  if TabStop and CanFocus then SetFocus;
  
  if Layers.MouseEvents then
    Layer := TLayerCollectionAccess(Layers).MouseDown(Button, Shift, X, Y)
  else
    Layer := nil;

  // lock the capture only if mbLeft was pushed or any mouse listener was activated
  if (Button = mbLeft) or (TLayerCollectionAccess(Layers).MouseListener <> nil) then
    MouseCapture := True;

  MouseDown(Button, Shift, X, Y, Layer);
end;

procedure TCustomImage32.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Layer: TCustomLayer;
begin
  inherited;
  if Layers.MouseEvents then
    Layer := TLayerCollectionAccess(Layers).MouseMove(Shift, X, Y)
  else
    Layer := nil;

  MouseMove(Shift, X, Y, Layer);
end;

procedure TCustomImage32.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Layer: TCustomLayer;
begin
  if Layers.MouseEvents then
    Layer := TLayerCollectionAccess(Layers).MouseUp(Button, Shift, X, Y)
  else
    Layer := nil;

  // unlock the capture only if mbLeft was raised and there is no active mouse listeners
  if (Button = mbLeft) and (TLayerCollectionAccess(Layers).MouseListener = nil) then
    MouseCapture := False;

  MouseUp(Button, Shift, X, Y, Layer);
end;

procedure TCustomImage32.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if Assigned(FOnMouseDown) then FOnMouseDown(Self, Button, Shift, X, Y, Layer);
end;

procedure TCustomImage32.MouseMove(Shift: TShiftState; X, Y: Integer;
  Layer: TCustomLayer);
begin
  if Assigned(FOnMouseMove) then FOnMouseMove(Self, Shift, X, Y, Layer);
end;

procedure TCustomImage32.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer; Layer: TCustomLayer);
begin
  if Assigned(FOnMouseUp) then FOnMouseUp(Self, Button, Shift, X, Y, Layer);
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
  if not assigned(Dest)then exit;
  OldRepaintMode := RepaintMode;
  RepaintMode := rmFull;

  CachedBitmapRect := DestRect;

  with CachedBitmapRect do
  begin
    if (Right - Left <= 0) or (Bottom - Top <= 0) or Bitmap.Empty then
      SetXForm(0, 0, 1, 1)
    else
      SetXForm(Left, Top, (Right - Left) / Bitmap.Width, (Bottom - Top) / Bitmap.Height);
  end;
  CacheValid := True;

  PaintToMode := True;
  try
    for I := 0 to FPaintStages.Count - 1 do
      with FPaintStages[I]^ do
        if RunTime then
          case Stage of
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

procedure TCustomImage32.SetScale(Value: TFloat);
begin
  if Value < 0.001 then Value := 0.001;
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
  if Value < 0.001 then Value := 0.001;
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
  if Value < 0.001 then Value := 0.001;
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
  FBitmap.BeginUpdate;
  with GetViewPortRect do
    FBitmap.SetSize(Right - Left, Bottom - Top);
  if DoClear then FBitmap.Clear(ClearColor);
  FBitmap.EndUpdate;
  InvalidateCache;
  Changed;
end;

procedure TCustomImage32.SetXForm(ShiftX, ShiftY, ScaleX, ScaleY: TFloat);
begin
  CachedShiftX := ShiftX;
  CachedShiftY := ShiftY;
  CachedScaleX := ScaleX;
  CachedScaleY := ScaleY;
  CachedRecScaleX := 1 / ScaleX;
  CachedRecScaleY := 1 / ScaleY;
end;

procedure TCustomImage32.UpdateCache;
begin
  if CacheValid then Exit;
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

function TCustomImage32.InvalidRectsAvailable: Boolean;
begin
  // avoid calling inherited, we have a totally different behaviour here...
  DoPrepareInvalidRects;
  Result := FInvalidRects.Count > 0;
end;

procedure TCustomImage32.SetRepaintMode(const Value: TRepaintMode);
begin
  inherited;

  case Value of
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
  else
    FBitmap.OnAreaChanged := nil;
    FBitmap.OnChange := BitmapChangeHandler;
  end;
end;

{ TIVScrollProperties }

function TIVScrollProperties.GetIncrement: Integer;
begin
  Result := Round(TCustomRangeBar(Master).Increment);
end;

function TIVScrollProperties.GetSize: Integer;
begin
  Result := ImgView.FScrollBarSize;
end;

function TIVScrollProperties.GetVisibility: TScrollbarVisibility;
begin
  Result := ImgView.FScrollBarVisibility;
end;

procedure TIVScrollProperties.SetIncrement(Value: Integer);
begin
  TCustomRangeBar(Master).Increment := Value;
  TCustomRangeBar(Slave).Increment := Value;
end;

procedure TIVScrollProperties.SetSize(Value: Integer);
begin
  ImgView.FScrollBarSize := Value;
  ImgView.AlignAll;
  ImgView.UpdateImage;
end;

procedure TIVScrollProperties.SetVisibility(const Value: TScrollbarVisibility);
begin
  if Value <> ImgView.FScrollBarVisibility then
  begin
    ImgView.FScrollBarVisibility := Value;
    ImgView.Resize;
  end;
end;

{ TCustomImgView32 }

procedure TCustomImgView32.AlignAll;
var
  ScrollbarVisible: Boolean;
begin
  if (Width > 0) and (Height > 0) then
  with GetViewportRect do
  begin
    ScrollbarVisible := GetScrollBarsVisible;

    if Assigned(HScroll) then
    begin
      HScroll.BoundsRect := Rect(Left, Bottom, Right, Height);
      HScroll.Visible := ScrollbarVisible;
      HScroll.Repaint;
    end;

    if Assigned(VScroll) then
    begin
      VScroll.BoundsRect := Rect(Right, Top, Width, Bottom);
      VScroll.Visible := ScrollbarVisible;
      VScroll.Repaint;
    end;
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
  end;

  with VScroll do
  begin
    Parent := Self;
    BorderStyle := bsNone;
    Centered := True;
    Kind := sbVertical;
    OnUserChange := ScrollHandler;
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
  if Assigned(FOnScroll) then FOnScroll(Self);
end;

function TCustomImgView32.GetScrollBarSize: Integer;
begin
  if GetScrollBarsVisible then
  begin
    Result := FScrollBarSize;
    if Result = 0 then Result := GetSystemMetrics(SM_CYHSCROLL);
  end
  else
    Result := 0;
end;

function TCustomImgView32.GetScrollBarsVisible: Boolean;
begin
  Result := True;
  if Assigned(FScrollBars) and Assigned(HScroll) and Assigned(VScroll) then
  case FScrollBars.Visibility of
    svAlways:
      Result := True;
    svHidden:
      Result := False;
    svAuto:
      Result := (HScroll.Range > (TRangeBarAccess(HScroll).EffectiveWindow + VScroll.Width)) or
                (VScroll.Range > (TRangeBarAccess(VScroll).EffectiveWindow + HScroll.Height));
  end;
end;

function TCustomImgView32.GetSizeGripRect: TRect;
var
  Sz: Integer;
begin
  Sz := GetScrollBarSize;

  if not Assigned(Parent) then
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
        if Align <> alClient then Exit;
        P := Parent;
        while True do
        begin
          if P is TCustomForm then
          begin
            Result := True;
            Break;
          end
          else if not Assigned(P) or (P.Align <> alClient) then Exit;
          P := P.Parent;
        end;
      end;

    sgNone: Result := False

  else { sgAlways }
    Result := True;
  end;
end;

procedure TCustomImgView32.Loaded;
begin
  AlignAll;
  Invalidate;
  UpdateScrollBars;
  if Centered then with Bitmap do ScrollToCenter(Width div 2, Height div 2);
  inherited;
end;

procedure TCustomImgView32.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
{$IFNDEF PLATFORM_INDEPENDENT}
var
  Action: Cardinal;
  Msg: TMessage;
  P: TPoint;
{$ENDIF}
begin
{$IFNDEF PLATFORM_INDEPENDENT}
  if IsSizeGripVisible and (Owner is TCustomForm) then
  begin
    P.X := X; P.Y := Y;
    if PtInRect(GetSizeGripRect, P) then
    begin
      Action := HTBOTTOMRIGHT;
      Application.ProcessMessages;
      Msg.Msg := WM_NCLBUTTONDOWN;
      Msg.WParam := Action;
      SetCaptureControl(nil);
      with Msg do SendMessage(TCustomForm(Owner).Handle, Msg, wParam, lParam);
      Exit;
    end;
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
    P.X := X; P.Y := Y;
    if PtInRect(GetSizeGripRect, P) then Screen.Cursor := crSizeNWSE;
  end;
end;

procedure TCustomImgView32.Paint;
begin
  if not Assigned(Parent) then
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

  if Assigned(Parent) then
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

procedure TCustomImgView32.Scroll(Dx, Dy: Integer);
begin
  DisableScrollUpdate := True;
  HScroll.Position := HScroll.Position + Dx;
  VScroll.Position := VScroll.Position + Dy;
  DisableScrollUpdate := False;
  UpdateImage;
end;

procedure TCustomImgView32.ScrollHandler(Sender: TObject);
begin
  if DisableScrollUpdate then Exit;
  if Sender = HScroll then HScroll.Repaint;
  if Sender = VScroll then VScroll.Repaint;
  UpdateImage;
  DoScroll;
  Repaint;
end;

procedure TCustomImgView32.ScrollToCenter(X, Y: Integer);
var
  ScaledDOversize: Integer;
begin
  DisableScrollUpdate := True;
  AlignAll;

  ScaledDOversize := Round(FOversize * Scale);
  with GetViewportRect do
  begin
    HScroll.Position := X * Scale - (Right - Left) / 2 + ScaledDOversize;
    VScroll.Position := Y * Scale - (Bottom - Top) / 2 + ScaledDOversize;
  end;
  DisableScrollUpdate := False;
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
    with Bitmap do
      ScrollToCenter(Width div 2, Height div 2)
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
  if not Centered then
  begin
    OffsetHorz := -HScroll.Position + ScaledOversize;
    OffsetVert := -VScroll.Position + ScaledOversize;
  end
  else
  begin
    if W > Sz.Cx + 2 * ScaledOversize then // Viewport is bigger than scaled Bitmap
      OffsetHorz := (W - Sz.Cx) / 2
    else
      OffsetHorz := -HScroll.Position + ScaledOversize;

    if H > Sz.Cy + 2 * ScaledOversize then // Viewport is bigger than scaled Bitmap
      OffsetVert := (H - Sz.Cy) / 2
    else
      OffsetVert := -VScroll.Position + ScaledOversize;
  end;
  InvalidateCache;
  EndUpdate;    
  Changed;
end;

procedure TCustomImgView32.UpdateScrollBars;
var
  Sz: TSize;
  ScaledDOversize: Integer;
begin
  if Assigned(HScroll) and Assigned(VScroll) then
  begin
    Sz := GetBitmapSize;
    ScaledDOversize := Round(2 * FOversize * Scale);

    HScroll.Range := Sz.Cx + ScaledDOversize;
    VScroll.Range := Sz.Cy + ScaledDOversize;

    // call AlignAll for Visibility svAuto, because the ranges of the scrollbars
    // may have just changed, thus we need to update the visibility of the scrollbars:
    if FScrollBarVisibility = svAuto then AlignAll;
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

end.
