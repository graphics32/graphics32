unit GR32_Image;

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
 * Mattias Andersson <mattias@centaurix.com>
 * Andre Beckedorf <Andre@metaException.de>
 * Andrew P. Rybin <aprybin@users.sourceforge.net>
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
{$IFDEF CLX}
  Qt, Types, QControls, QGraphics, QForms, QConsts,
  {$IFDEF LINUX}Libc,{$ENDIF}
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
{$ELSE}
  Windows, Messages, Controls, Graphics, Forms,
{$ENDIF}
  Classes, SysUtils, GR32, GR32_Layers, GR32_RangeBars;

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

  TBitmap32Access = class(TBitmap32);

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
  TScaleMode = (smNormal, smStretch, smScale, smResize);
  TPaintBoxOptions = set of (pboWantArrowKeys, pboAutoFocus);

  { TCustomPaintBox32 }
  TCustomPaintBox32 = class(TCustomControl)
  private
    FBuffer: TBitmap32;
    FBufferOversize: Integer;
    FBufferValid: Boolean;
    FOptions: TPaintBoxOptions;
    FOnGDIOverlay: TNotifyEvent;
    FMouseInControl: Boolean;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    procedure SetBufferOversize(Value: Integer);
{$IFNDEF CLX}
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Msg: TWmGetDlgCode); message WM_GETDLGCODE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
{$ENDIF}
  protected
    procedure DoPaintBuffer; virtual;
    procedure DoPaintGDIOverlay; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
{$IFDEF CLX}
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
{$ELSE}
    procedure MouseEnter; virtual;
    procedure MouseLeave; virtual;
{$ENDIF}
    procedure Paint; override;
    procedure ResizeBuffer;
    property  BufferValid: Boolean read FBufferValid write FBufferValid;
{$IFDEF CLX}
    function WidgetFlags: Integer; override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function  GetViewportRect: TRect; virtual;
    procedure Flush; overload;
    procedure Flush(const SrcRect: TRect); overload;
    procedure Invalidate; override;
    procedure Loaded; override;
    procedure Resize; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property Buffer: TBitmap32 read FBuffer;
    property BufferOversize: Integer read FBufferOversize write SetBufferOversize;
    property Options: TPaintBoxOptions read FOptions write FOptions default [];
    property MouseInControl: Boolean read FMouseInControl;
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
{$IFNDEF CLX}
    property AutoSize;
{$ENDIF}
    property Constraints;
    property Cursor;
{$IFNDEF CLX}
    property DragCursor;
{$ENDIF}
    property Options;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
{$IFNDEF CLX}
    property OnCanResize;
{$ENDIF}
    property OnClick;
{$IFDEF DELPHI5}
    property OnContextPopup;
{$ENDIF}
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

  TCustomImage32 = class(TCustomPaintBox32)
  private
    FBitmap: TBitmap32;
    FBitmapAlign: TBitmapAlign;
    FLayers: TLayerCollection;
    FOffsetHorz: Single;
    FOffsetVert: Single;
    FPaintStages: TPaintStages;
    FScale: Single;
    FScaleMode: TScaleMode;
    FUpdateCount: Integer;
    FOnBitmapResize: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FOnInitStages: TNotifyEvent;
    FOnMouseDown: TImgMouseEvent;
    FOnMouseMove: TImgMouseMoveEvent;
    FOnMouseUp: TImgMouseEvent;
    FOnPaintStage: TPaintStageEvent;
    procedure ResizedHandler(Sender: TObject);
    procedure ChangedHandler(Sender: TObject);
    function  GetOnPixelCombine: TPixelCombineEvent;
    procedure GDIUpdateHandler(Sender: TObject);
    procedure SetBitmap(Value: TBitmap32); {$IFDEF CLX}reintroduce;{$ENDIF}
    procedure SetBitmapAlign(Value: TBitmapAlign);
    procedure SetLayers(Value: TLayerCollection);
    procedure SetOffsetHorz(Value: Single);
    procedure SetOffsetVert(Value: Single);
    procedure SetScale(Value: Single);
    procedure SetScaleMode(Value: TScaleMode);
    procedure SetOnPixelCombine(Value: TPixelCombineEvent);
  protected
    CachedBitmapRect: TRect;
    CachedXForm: TCoordXForm;
    CacheValid: Boolean;
    OldSzX, OldSzY: Integer;
    PaintToMode: Boolean;
    procedure BitmapResized; virtual;
{$IFNDEF CLX}
    function  CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
{$ENDIF}
    procedure DoInitStages; virtual;
    procedure DoPaintBuffer; override;
    procedure DoPaintGDIOverlay; override;
    procedure DoScaleChange; virtual;
    procedure InitDefaultStages; virtual;
    procedure InvalidateCache;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); overload; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); overload; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); overload; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer); reintroduce; overload; virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer); reintroduce; overload; virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer); reintroduce; overload; virtual;
{$IFDEF CLX}
    procedure MouseLeave(AControl: TControl); override;
{$ELSE}
    procedure MouseLeave; override;
{$ENDIF}
    procedure UpdateCache;
    property  UpdateCount: Integer read FUpdateCount;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate; virtual;
    function  BitmapToControl(const APoint: TPoint): TPoint;
    procedure Changed; virtual;
    function  ControlToBitmap(const APoint: TPoint): TPoint;
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
    procedure PaintTo(Dest: TBitmap32; DestRect: TRect);
    procedure Resize; override;
    procedure SetupBitmap(DoClear: Boolean = False; ClearColor: TColor32 = $FF000000);
    property Bitmap: TBitmap32 read FBitmap write SetBitmap;
    property BitmapAlign: TBitmapAlign read FBitmapAlign write SetBitmapAlign;
    property Canvas;
    property Layers: TLayerCollection read FLayers write SetLayers;
    property OffsetHorz: Single read FOffsetHorz write SetOffsetHorz;
    property OffsetVert: Single read FOffsetVert write SetOffsetVert;
    property PaintStages: TPaintStages read FPaintStages;
    property Scale: Single read FScale write SetScale;
    property ScaleMode: TScaleMode read FScaleMode write SetScaleMode;
    property OnBitmapResize: TNotifyEvent read FOnBitmapResize write FOnBitmapResize;
    property OnBitmapPixelCombine: TPixelCombineEvent read GetOnPixelCombine write SetOnPixelCombine;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnInitStages: TNotifyEvent read FOnInitStages write FOnInitStages;
    property OnMouseDown: TImgMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TImgMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TImgMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnPaintStage: TPaintStageEvent read FOnPaintStage write FOnPaintStage;
  end;

  TImage32 = class(TCustomImage32)
  published
    property Align;
    property Anchors;
{$IFNDEF CLX}
    property AutoSize;
{$ENDIF}
    property Bitmap;
    property BitmapAlign;
    property Color;
    property Constraints;
    property Cursor;
{$IFNDEF CLX}
    property DragCursor;
{$ENDIF}
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property Scale;
    property ScaleMode;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnBitmapResize;
{$IFNDEF CLX}
    property OnCanResize;
{$ENDIF}
    property OnClick;
    property OnChange;
{$IFDEF DELPHI5}
    property OnContextPopup;
{$ENDIF}
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

  { TIVScrollProperties }
  TIVScrollProperties = class(TArrowBarAccess)
  private
    function GetIncrement: Integer;
    function GetSize: Integer;
    procedure SetIncrement(Value: Integer);
    procedure SetSize(Value: Integer);
  protected
    ImgView: TCustomImgView32;
  published
    property Increment: Integer read GetIncrement write SetIncrement default 8;
    property Size: Integer read GetSize write SetSize default 0;
  end;

  TSizeGripStyle = (sgAuto, sgNone, sgAlways);

  { TCustomImgView32 }
  TCustomImgView32 = class(TCustomImage32)
  private
    FCentered: Boolean;
    FScrollBarSize: Integer;
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
    function  GetScrollBarSize: Integer;
    function  GetSizeGripRect: TRect;
    function  IsSizeGripVisible: Boolean;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
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
    property SizeGrip: TSizeGripStyle read FSizeGrip write SetSizeGrip;
    property OverSize: Integer read FOverSize write SetOverSize;    
    property OnScroll: TNotifyEvent read FOnScroll write FOnScroll;
  end;

  TImgView32 = class(TCustomImgView32)
    property Align;
    property Anchors;
{$IFNDEF CLX}
    property AutoSize;
{$ENDIF}
    property Bitmap;
    property Centered;
    property Color;
    property Constraints;
    property Cursor;
{$IFNDEF CLX}
    property DragCursor;
{$ENDIF}    
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property Scale;
    property ScrollBars;
    property ShowHint;
    property SizeGrip;
    property OverSize;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnBitmapResize;
{$IFNDEF CLX}
    property OnCanResize;
{$ENDIF}
    property OnClick;
    property OnChange;
{$IFDEF DELPHI5}
    property OnContextPopup;
{$ENDIF}
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

uses Math, TypInfo, GR32_System;

type
  TLayerAccess = class(TCustomLayer);
  TLayerCollectionAccess = class(TLayerCollection);

const
  UnitXForm: TCoordXForm = (
    ScaleX: $10000;
    ScaleY: $10000;
    ShiftX: 0;
    ShiftY: 0;
    RevScaleX: 65536;
    RevScaleY: 65536);

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

{$IFNDEF CLX}
procedure TCustomPaintBox32.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  MouseEnter;
end;

procedure TCustomPaintBox32.CMMouseLeave(var Message: TMessage);
begin
  MouseLeave;
  inherited;
end;
{$ENDIF}

constructor TCustomPaintBox32.Create(AOwner: TComponent);
begin
  inherited;
  FBuffer := TBitmap32.Create;
  FBuffer.BeginUpdate; // just to speed the things up a little
  FBufferOversize := 40;
  Height := 192;
  Width := 192;
end;

destructor TCustomPaintBox32.Destroy;
begin
  FBuffer.Free;
  inherited;
end;

procedure TCustomPaintBox32.DoPaintBuffer;
begin
  // do nothing by default, descendants should override this method
  // for painting operations, not the Paint method!!!
  FBufferValid := True;
end;

procedure TCustomPaintBox32.DoPaintGDIOverlay;
begin
  if Assigned(FOnGDIOverlay) then FOnGDIOverlay(Self);
end;

procedure TCustomPaintBox32.Flush;
begin
{$IFDEF CLX}
  if Assigned(Canvas.Handle) and Assigned(FBuffer.Handle) then
{$ELSE}
  if (Canvas.Handle <> 0) and (FBuffer.Handle <> 0) then
{$ENDIF}
  begin
    Canvas.Lock;
    try
      FBuffer.Lock;
      try
        with GetViewportRect do
{$IFDEF CLX}
        begin
          if not QPainter_isActive(FBuffer.Handle) then
            if not QPainter_begin(FBuffer.Handle, FBuffer.Pixmap) then
              raise EInvalidGraphicOperation.CreateRes(@SInvalidCanvasState);
          QPainter_drawPixmap(Canvas.Handle, Top, Left, FBuffer.Pixmap, 0, 0, Right - Left, Bottom - Top);
          QPainter_end(FBuffer.Handle);

          TBitmap32Access(FBuffer).CheckPixmap; // try to avoid QPixmap -> QImage conversion, since we don't need that.
        end;
{$ELSE}
          BitBlt(Canvas.Handle, Left, Top, Right - Left, Bottom - Top,
            FBuffer.Handle, 0, 0, SRCCOPY);
{$ENDIF}
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
{$IFDEF CLX}
  if Assigned(Canvas.Handle) and Assigned(FBuffer.Handle) then
{$ELSE}
  if (Canvas.Handle <> 0) and (FBuffer.Handle <> 0) then
{$ENDIF}
  begin
    Canvas.Lock;
    try
      FBuffer.Lock;
      try
        R := GetViewPortRect;
        with SrcRect do
{$IFDEF CLX}
        begin
          if not QPainter_isActive(FBuffer.Handle) then
            if not QPainter_begin(FBuffer.Handle, FBuffer.Pixmap) then
              raise EInvalidGraphicOperation.CreateRes(@SInvalidCanvasState);
          QPainter_drawPixmap(Canvas.Handle, Top + R.Top, Left + R.Left,
            FBuffer.Pixmap, 0, 0, Right - Left, Bottom - Top);
          QPainter_end(FBuffer.Handle);

          TBitmap32Access(FBuffer).CheckPixmap; // try to avoid QPixmap -> QImage conversion, since we don't need that.
        end;
{$ELSE}
          BitBlt(Canvas.Handle, Left + R.Left, Top + R.Top, Right - Left, Bottom - Top,
            FBuffer.Handle, Left, Top, SRCCOPY);
{$ENDIF}
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

{$IFDEF CLX}
procedure TCustomPaintBox32.MouseEnter(AControl: TControl);
begin
  FMouseInControl := True;
  inherited;
end;

procedure TCustomPaintBox32.MouseLeave(AControl: TControl);
begin
  FMouseInControl := False;
  inherited;
end;

{$ELSE}

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
{$ENDIF}

procedure TCustomPaintBox32.Paint;
begin
  ResizeBuffer;
  if not FBufferValid then
  begin
{$IFDEF CLX}
    TBitmap32Access(FBuffer).ImageNeeded;
{$ENDIF}
    DoPaintBuffer;
  end;

  FBuffer.Lock;
  try
    with GetViewportRect do
{$IFDEF CLX}
    begin
      if not QPainter_isActive(FBuffer.Handle) then
        if not QPainter_begin(FBuffer.Handle, FBuffer.Pixmap) then
          raise EInvalidGraphicOperation.CreateRes(@SInvalidCanvasState);
      QPainter_drawPixmap(Canvas.Handle, Top, Left, FBuffer.Pixmap, 0, 0, Right - Left, Bottom - Top);
      QPainter_end(FBuffer.Handle);

      TBitmap32Access(FBuffer).CheckPixmap; // try to avoid QPixmap -> QImage conversion, since we don't need that.
    end;
{$ELSE}
      BitBlt(Canvas.Handle, Left, Top, Right - Left, Bottom - Top,
        FBuffer.Handle, 0, 0, SRCCOPY);
{$ENDIF}
  finally
    FBuffer.Unlock;
  end;
  DoPaintGDIOverlay;
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
  if NewWidth > W then W := NewWidth + FBufferOversize
  else if NewWidth < W - FBufferOversize then W := NewWidth;
  if W < 1 then W := 1;

  H := FBuffer.Height;
  if NewHeight > H then H := NewHeight + FBufferOversize
  else if NewHeight < H - FBufferOversize then H := NewHeight;
  if H < 1 then H := 1;

  if (W <> FBuffer.Width) or (H <> FBuffer.Height) then
  begin
    FBuffer.Lock;
    FBuffer.SetSize(NewWidth, NewHeight);
    FBuffer.Unlock;
    FBufferValid := False;
  end;
end;

procedure TCustomPaintBox32.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  FBufferValid := False;
  inherited;
end;

procedure TCustomPaintBox32.SetBufferOversize(Value: Integer);
begin
  if Value < 0 then Value := 0;
  FBufferOversize := Value;
end;

{$IFDEF CLX}
function TCustomPaintBox32.WidgetFlags: Integer;
begin
  Result := Inherited WidgetFlags or Integer(WidgetFlags_WRepaintNoErase) or
    Integer(WidgetFlags_WResizeNoErase);
end;
{$ELSE}
procedure TCustomPaintBox32.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TCustomPaintBox32.WMGetDlgCode(var Msg: TWmGetDlgCode);
begin
  with Msg do if pboWantArrowKeys in Options then
    Result:= Result or DLGC_WANTARROWS
  else
    Result:= Result and not DLGC_WANTARROWS;
end;
{$ENDIF}



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
{$IFNDEF CLX}
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
{$ELSE}
begin
{$ENDIF}
  if (FUpdateCount <> 0) and Assigned(FOnBitmapResize) then FOnBitmapResize(Self);
  InvalidateCache;
  Invalidate;
end;

function TCustomImage32.BitmapToControl(const APoint: TPoint): TPoint;
begin
  // convert coordinates from bitmap's ref. frame to control's ref. frame
  UpdateCache;
  with CachedXForm, APoint do
  begin
    Result.X := X * ScaleX div $10000 + ShiftX;
    Result.Y := Y * ScaleY div $10000 + ShiftY;
  end;
end;

{$IFNDEF CLX}
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
{$ENDIF}

procedure TCustomImage32.Changed;
begin
  if FUpdateCount = 0 then
  begin
    Invalidate;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TCustomImage32.ChangedHandler(Sender: TObject);
begin
  Changed;
end;

function TCustomImage32.ControlToBitmap(const APoint: TPoint): TPoint;
begin
  // convert point coords from control's ref. frame to bitmap's ref. frame
  // the coordinates are not clipped to bitmap image boundary
  UpdateCache;
  with CachedXForm, APoint do
  begin
    Result.X := (X - ShiftX) * RevScaleX div $10000;
    Result.Y := (Y - ShiftY) * RevScaleY div $10000;
  end;
end;

constructor TCustomImage32.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csDoubleClicks, csReplicatable, csOpaque];
  FBitmap := TBitmap32.Create;
  FBitmap.OnChange := ChangedHandler;
  FBitmap.OnResize := ResizedHandler;
  FLayers := TLayerCollection.Create(Self);
  with TLayerCollectionAccess(FLayers) do
  begin
    CoordXForm := @CachedXForm;
    OnChange := ChangedHandler;
    OnGDIUpdate := GDIUpdateHandler;
  end;
  FPaintStages := TPaintStages.Create;
  FScale := 1.0;
  InitDefaultStages;
end;

destructor TCustomImage32.Destroy;
begin
  BeginUpdate;
  FPaintStages.Free;
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
  I: Integer;
  DT, RT: Boolean;
begin
  UpdateCache;
  DT := csDesigning in ComponentState;
  RT := not DT;

  for I := 0 to FPaintStages.Count - 1 do
    with FPaintStages[I]^ do
      if (DsgnTime and DT) or (RunTime and RT) then
        case Stage of
          PST_CUSTOM: ExecCustom(Buffer, I);
          PST_CLEAR_BUFFER: ExecClearBuffer(Buffer, I);
          PST_CLEAR_BACKGND: ExecClearBackgnd(Buffer, I);
          PST_DRAW_BITMAP: ExecDrawBitmap(Buffer, I);
          PST_DRAW_LAYERS: ExecDrawLayers(Buffer, I);
          PST_CONTROL_FRAME: ExecControlFrame(Buffer, I);
          PST_BITMAP_FRAME: ExecBitmapFrame(Buffer, I);
        end;
  inherited;
end;

procedure TCustomImage32.DoPaintGDIOverlay;
var
  I: Integer;
begin
  for I := 0 to Layers.Count - 1 do
    if (Layers[I].LayerOptions and $40000000) <> 0 then
      TLayerAccess(Layers[I]).PaintGDI(Canvas);
  inherited;
end;

procedure TCustomImage32.DoScaleChange;
begin
  // do nothing here
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
begin
  C := Color32(Color);
  if (Bitmap.Empty) or (Bitmap.DrawMode <> dmOpaque) then Dest.Clear(C)
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

procedure TCustomImage32.ExecClearBuffer(Dest: TBitmap32; StageNum: Integer);
begin
  Dest.Clear(Color32(Color));
end;

procedure TCustomImage32.ExecControlFrame(Dest: TBitmap32; StageNum: Integer);
begin
  {$IFDEF CLX}
  Dest.Canvas.DrawFocusRect(Rect(0, 0, Width, Height));
  {$ELSE}
  DrawFocusRect(Dest.Handle, Rect(0, 0, Width, Height));
  {$ENDIF}
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

procedure TCustomImage32.GDIUpdateHandler(Sender: TObject);
begin
  Paint;
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
  ScaleX, ScaleY: Single;
begin
  with Result do
  begin
    if Bitmap.Empty or (Width = 0) or (Height = 0) then
    begin
      Cx := 0;
      Cy := 0;
      Exit;
    end;

    case ScaleMode of
      smNormal:
        begin
          Cx := Bitmap.Width;
          Cy := Bitmap.Height;
        end;

      smStretch:
        begin
          Cx := Width;
          Cy := Height;
        end;

      smResize:
        begin
          Cx := Bitmap.Width;
          Cy := Bitmap.Height;
          ScaleX := Width / Cx;
          ScaleY := Height / Cy;
          if ScaleX >= ScaleY then
          begin
            Cx := Round(Cx * ScaleY);
            Cy := Height;
          end
          else
          begin
            Cx := Width;
            Cy := Round(Cy * ScaleX);
          end;
        end;
    else // smScale
      begin
        Cx := Round(Bitmap.Width * Scale);
        Cy := Round(Bitmap.Height * Scale);
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
    Parameter := $80000000;
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

{$IFDEF CLX}
procedure TCustomImage32.MouseLeave(AControl: TControl);
begin
  if (Layers.MouseEvents) and (Layers.MouseListener = nil) then
    Screen.Cursor := crDefault;
  inherited;
end;

{$ELSE}

procedure TCustomImage32.MouseLeave;
begin
  if (Layers.MouseEvents) and (Layers.MouseListener = nil) then
    Screen.Cursor := crDefault;
  inherited;
end;
{$ENDIF}

procedure TCustomImage32.PaintTo(Dest: TBitmap32; DestRect: TRect);
var
  I: Integer;
begin
  CachedBitmapRect := DestRect;

  with CachedBitmapRect, CachedXForm do
  begin
    if (Right - Left <= 0) or (Bottom - Top <= 0) or Bitmap.Empty then
      CachedXForm := UnitXForm
    else
    begin
      ShiftX := Left;
      ShiftY := Top;
      ScaleX := MulDiv(Right - Left, $10000, Bitmap.Width);
      ScaleY := MulDiv(Bottom - Top, $10000, Bitmap.Height);
      RevScaleX := MulDiv(Bitmap.Width, $10000, Right - Left);
      RevScaleY := MulDiv(Bitmap.Height, $10000, Bottom - Top);
    end;
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
end;

procedure TCustomImage32.Resize;
begin
  InvalidateCache;
  inherited;
end;

procedure TCustomImage32.ResizedHandler(Sender: TObject);
begin
{$IFDEF CLX}
  // workaround to stop CLX from calling BitmapResized and to prevent
  // AV when accessing Layers. Layers is already freed at that time
  if not(csDestroying in ComponentState) then
{$ENDIF}
  BitmapResized;
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

procedure TCustomImage32.SetOffsetHorz(Value: Single);
begin
  if Value <> FOffsetHorz then
  begin
    InvalidateCache;
    FOffsetHorz := Value;
    Changed;
  end;
end;

procedure TCustomImage32.SetOffsetVert(Value: Single);
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

procedure TCustomImage32.SetScale(Value: Single);
begin
  if Value < 0.001 then Value := 0.001;
  if Value <> FScale then
  begin
    InvalidateCache;
    FScale := Value;
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

procedure TCustomImage32.UpdateCache;
begin
  if CacheValid then Exit;
  CachedBitmapRect := GetBitmapRect;
  with CachedBitmapRect, CachedXForm do
  begin
    if Bitmap.Empty then CachedXForm := UnitXForm
    else
    begin
      Assert((Right > Left) and (Bottom > Top));
      ShiftX := Left;
      ShiftY := Top;
      ScaleX := MulDiv(Right - Left, $10000, Bitmap.Width);
      ScaleY := MulDiv(Bottom - Top, $10000, Bitmap.Height);
      RevScaleX := MulDiv(Bitmap.Width, $10000, Right - Left);
      RevScaleY := MulDiv(Bitmap.Height, $10000, Bottom - Top);
    end;
  end;
  CacheValid := True;
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

procedure TIVScrollProperties.SetIncrement(Value: Integer);
begin
  TCustomRangeBar(Master).Increment := Value;
  TCustomRangeBar(Slave).Increment := Value;
end;

procedure TIVScrollProperties.SetSize(Value: Integer);
begin
  ImgView.FScrollBarSize := Value;
  ImgView.AlignAll;
end;




{ TCustomImgView32 }

procedure TCustomImgView32.AlignAll;
begin
  with GetViewportRect do
  begin
    If Assigned(HScroll) then
    begin
      HScroll.BoundsRect := Rect(Left, Bottom, Right, Height);
      HScroll.Repaint;
    end;

    If Assigned(VScroll) then
    begin
      VScroll.BoundsRect := Rect(Right, Top, Width, Bottom);
      VScroll.Repaint;
    end;
  end;
end;

procedure TCustomImgView32.BitmapResized;
begin
  inherited;
  UpdateScrollBars;
  if Centered then ScrollToCenter(Bitmap.Width div 2, Bitmap.Height div 2)
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
  FScrollBars.Free;
  inherited;
end;

procedure TCustomImgView32.DoDrawSizeGrip(R: TRect);
begin
{$IFNDEF CLX}
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
{$IFDEF CLX}
var
  Size: TSize;
{$ENDIF}
begin
  Result := FScrollBarSize;
{$IFDEF CLX}
  QStyle_scrollBarExtent(Application.Style.Handle, @Size);
  if Result = 0 then Result := Size.cy;
{$ELSE}
  if Result = 0 then Result := GetSystemMetrics(SM_CYHSCROLL);
{$ENDIF}
end;

function TCustomImgView32.GetSizeGripRect: TRect;
var
  Sz: Integer;
begin
  Sz := GetScrollBarSize;
  Result := GetClientRect;
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
{$IFNDEF CLX}
var
  Action: Cardinal;
  Msg: TMessage;
  P: TPoint;
begin
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
{$ELSE}
begin  
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
  if IsSizeGripVisible then
    DoDrawSizeGrip(GetSizeGripRect)
  else
  begin
    Canvas.Brush.Color := clBtnFace;
    Canvas.FillRect(GetSizeGripRect);
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

procedure TCustomImgView32.SetCentered(Value: Boolean);
begin
  InvalidateCache;
  FCentered := Value;
  HScroll.Centered := Value;
  VScroll.Centered := Value;
  UpdateScrollBars;
  UpdateImage;
  if Value then with Bitmap do ScrollToCenter(Width div 2, Height div 2)
  else ScrollToCenter(0, 0);
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
  If Assigned(HScroll) and Assigned(VScroll) then
  begin
    Sz := GetBitmapSize;
    ScaledDOversize := Round(2 * FOversize * Scale);

    HScroll.Range := Sz.Cx + ScaledDOversize;
    VScroll.Range := Sz.Cy + ScaledDOversize;
  end;
end;

{ TBitmap32Item }

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
