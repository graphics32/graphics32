unit GR32_RangeBars;

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
 * Andre Beckedorf <Andre@metaException.de>
 * Marc Lafon
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
{$IFDEF FPC}
  LCLIntf, LMessages, LCLType, Graphics, Controls, Forms, Dialogs, ExtCtrls,
  {$IFDEF Windows} Windows, {$ENDIF}
{$ELSE}
  Windows, Messages, {$IFDEF INLININGSUPPORTED}Types,{$ENDIF}
  Graphics, Controls, Forms, Dialogs, ExtCtrls,
{$ENDIF}
  SysUtils, Classes, GR32;

type
  TRBDirection = (drLeft, drUp, drRight, drDown);
  TRBDirections = set of TRBDirection;
  TRBZone = (zNone, zBtnPrev, zTrackPrev, zHandle, zTrackNext, zBtnNext);
  TRBStyle = (rbsDefault, rbsMac);
  TRBBackgnd = (bgPattern, bgSolid);
  TRBGetSizeEvent = procedure(Sender: TObject; var Size: Integer) of object;

  TArrowBar = class(TCustomControl)
  private
    FBackgnd: TRBBackgnd;
    FBorderStyle: TBorderStyle;
    FButtonSize: Integer;
    FHandleColor: TColor;
    FButtoncolor:TColor;
    FHighLightColor:TColor;
    FShadowColor:TColor;
    FBorderColor:TColor;
    FKind: TScrollBarKind;
    FShowArrows: Boolean;
    FShowHandleGrip: Boolean;
    FStyle: TRBStyle;
    FOnChange: TNotifyEvent;
    FOnUserChange: TNotifyEvent;
    procedure SetButtonSize(Value: Integer);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetHandleColor(Value: TColor);
    procedure SetHighLightColor(Value: TColor);
    procedure SetShadowColor(Value: TColor);
    procedure SetButtonColor(Value: TColor);
    procedure SetBorderColor(Value: TColor);
    procedure SetKind(Value: TScrollBarKind);
    procedure SetShowArrows(Value: Boolean);
    procedure SetShowHandleGrip(Value: Boolean);
    procedure SetStyle(Value: TRBStyle);
    procedure SetBackgnd(Value: TRBBackgnd);
{$IFDEF FPC}
    procedure CMEnabledChanged(var Message: TLMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseLeave(var Message: TLMessage); message CM_MOUSELEAVE;
    procedure WMNCCalcSize(var Message: TLMNCCalcSize); message LM_NCCALCSIZE;
    procedure WMEraseBkgnd(var Message: TLmEraseBkgnd); message LM_ERASEBKGND;
{$IFDEF Windows}
    procedure WMNCPaint(var Message: TWMNCPaint); message LM_NCPAINT;
{$ENDIF}
{$ELSE}
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
{$ENDIF}
  protected
    GenChange: Boolean;
    DragZone: TRBZone;
    HotZone: TRBZone;
    Timer: TTimer;
    TimerMode: Integer;
    StoredX, StoredY: Integer;
    PosBeforeDrag: Single;
    procedure DoChange; virtual;
    procedure DoDrawButton(R: TRect; Direction: TRBDirection; Pushed, Enabled, Hot: Boolean); virtual;
    procedure DoDrawHandle(R: TRect; Horz: Boolean; Pushed, Hot: Boolean); virtual;
    procedure DoDrawTrack(R: TRect; Direction: TRBDirection; Pushed, Enabled, Hot: Boolean); virtual;
    function  DrawEnabled: Boolean; virtual;
    function  GetBorderSize: Integer;
    function  GetHandleRect: TRect; virtual;
    function  GetButtonSize: Integer;
    function  GetTrackBoundary: TRect;
    function  GetZone(X, Y: Integer): TRBZone;
    function  GetZoneRect(Zone: TRBZone): TRect;
    procedure MouseLeft; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure StartDragTracking;
    procedure StartHotTracking;
    procedure StopDragTracking;
    procedure StopHotTracking;
    procedure TimerHandler(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    property Color default clScrollBar;
    property Backgnd: TRBBackgnd read FBackgnd write SetBackgnd;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property ButtonSize: Integer read FButtonSize write SetButtonSize default 0;
    property HandleColor: TColor read FHandleColor write SetHandleColor default clBtnShadow;
    property ButtonColor:TColor read FButtonColor write SetButtonColor default clBtnFace;
    property HighLightColor:TColor read FHighLightColor write SetHighLightColor default clBtnHighlight;
    property ShadowColor:TColor read FShadowColor write SetShadowColor default clBtnShadow;
    property BorderColor:TColor read FBorderColor write SetBorderColor default clWindowFrame;
    property Kind: TScrollBarKind read FKind write SetKind default sbHorizontal;
    property ShowArrows: Boolean read FShowArrows write SetShowArrows default True;
    property ShowHandleGrip: Boolean read FShowHandleGrip write SetShowHandleGrip;
    property Style: TRBStyle read FStyle write SetStyle default rbsDefault;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnUserChange: TNotifyEvent read FOnUserChange write FOnUserChange;
  end;

  TRBIncrement = 1..32768;

  TCustomRangeBar = class(TArrowBar)
  private
    FCentered: Boolean;
    FEffectiveWindow: Integer;
    FIncrement: TRBIncrement;
    FPosition: Single;
    FRange: Integer;
    FWindow: Integer;
    function IsPositionStored: Boolean;
    procedure SetPosition(Value: Single);
    procedure SetRange(Value: Integer);
    procedure SetWindow(Value: Integer);
  protected
    procedure AdjustPosition;
    function  DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    function  DrawEnabled: Boolean; override;
    function  GetHandleRect: TRect; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure TimerHandler(Sender: TObject); override;
    procedure UpdateEffectiveWindow;
    property EffectiveWindow: Integer read FEffectiveWindow;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Resize; override;
    procedure SetParams(NewRange, NewWindow: Integer);
    property Centered: Boolean read FCentered write FCentered;
    property Increment: TRBIncrement read FIncrement write FIncrement default 8;
    property Position: Single read FPosition write SetPosition stored IsPositionStored;
    property Range: Integer read FRange write SetRange default 0;
    property Window: Integer read FWindow write SetWindow default 0;
  end;

  TRangeBar = class(TCustomRangeBar)
  published
    property Align;
    property Anchors;
    property Constraints;
    property Color;
    property Backgnd;
    property BorderStyle;
    property ButtonSize;
    property Enabled;
    property HandleColor;
    property ButtonColor;
    property HighLightColor;
    property ShadowColor;
    property BorderColor;
    property Increment;
    property Kind;
    property Range;
    property Style;
    property Visible;
    property Window;
    property ShowArrows;
    property ShowHandleGrip;
    property Position; // this should be located after the Range property
    property OnChange;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
    property OnStartDrag;
    property OnUserChange;
  end;

  TCustomGaugeBar = class(TArrowBar)
  private
    FHandleSize: Integer;
    FLargeChange: Integer;
    FMax: Integer;
    FMin: Integer;
    FPosition: Integer;
    FSmallChange: Integer;
    procedure SetHandleSize(Value: Integer);
    procedure SetMax(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure SetPosition(Value: Integer);
    procedure SetLargeChange(Value: Integer);
    procedure SetSmallChange(Value: Integer);
  protected
    procedure AdjustPosition;
    function  DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    function  GetHandleRect: TRect; override;
    function  GetHandleSize: Integer;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure TimerHandler(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    property HandleSize: Integer read FHandleSize write SetHandleSize default 0;
    property LargeChange: Integer read FLargeChange write SetLargeChange default 1;
    property Max: Integer read FMax write SetMax default 100;
    property Min: Integer read FMin write SetMin default 0;
    property Position: Integer read FPosition write SetPosition;
    property SmallChange: Integer read FSmallChange write SetSmallChange default 1;
    property OnChange;
    property OnUserChange;
  end;

  TGaugeBar = class(TCustomGaugeBar)
  published
    property Align;
    property Anchors;
    property Constraints;
    property Color;
    property Backgnd;
    property BorderStyle;
    property ButtonSize;
    property Enabled;
    property HandleColor;
    property ButtonColor;
    property HighLightColor;
    property ShadowColor;
    property BorderColor;
    property HandleSize;
    property Kind;
    property LargeChange;
    property Max;
    property Min;
    property ShowArrows;
    property ShowHandleGrip;
    property Style;
    property SmallChange;
    property Visible;
    property Position;
    property OnChange;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnUserChange;
  end;

  { TArrowBarAccess }
  { This class is designed to facilitate access to
    properties of TArrowBar class when creating custom controls, which
    incorporate TArrowBar. It allows controlling up to two arrow bars.
    Master is used to read and write properties, slave - only to write.

    Well, maybe it is not so useful itself, but it is a common ancestor
    for TRangeBarAccess and TGaugeBarAccess classes, which work much the
    same way.

    When writing a new control, which uses TArrowBar, declare the bar as
    protected member, TArrowBarAccess as published property, and assign
    its Master to the arrow bar }
  TArrowBarAccess = class(TPersistent)
  private
    FMaster: TArrowBar;
    FSlave: TArrowBar;
    function GetBackgnd: TRBBackgnd;
    function GetButtonSize: Integer;
    function GetColor: TColor;
    function GetHandleColor: TColor;
    function GetHighLightColor: TColor;
    function GetButtonColor: TColor;
    function GetBorderColor: TColor;
    function GetShadowColor: TColor;
    function GetShowArrows: Boolean;
    function GetShowHandleGrip: Boolean;
    function GetStyle: TRBStyle;
    procedure SetBackgnd(Value: TRBBackgnd);
    procedure SetButtonSize(Value: Integer);
    procedure SetColor(Value: TColor);
    procedure SetHandleColor(Value: TColor);
    procedure SetShowArrows(Value: Boolean);
    procedure SetShowHandleGrip(Value: Boolean);
    procedure SetStyle(Value: TRBStyle);
    procedure SetHighLightColor(Value: TColor);
    procedure SetShadowColor(Value: TColor);
    procedure SetButtonColor(Value: TColor);
    procedure SetBorderColor(Value: TColor);
  public
    property Master: TArrowBar read FMaster write FMaster;
    property Slave: TArrowBar read FSlave write FSlave;
  published
    property Color: TColor read GetColor write SetColor default clScrollBar;
    property Backgnd: TRBBackgnd read GetBackgnd write SetBackgnd default bgPattern;
    property ButtonSize: Integer read GetButtonSize write SetButtonSize default 0;
    property HandleColor: TColor read GetHandleColor write SetHandleColor default clBtnShadow;
    property ButtonColor:TColor read GetButtonColor write SetButtonColor default clBtnFace;
    property HighLightColor:TColor read GetHighLightColor write SetHighLightColor default clBtnHighlight;
    property ShadowColor:TColor read GetShadowColor write SetShadowColor default clBtnShadow;
    property BorderColor:TColor read GetBorderColor write SetBorderColor default clWindowFrame;
    property ShowArrows: Boolean read GetShowArrows write SetShowArrows default True;
    property ShowHandleGrip: Boolean read GetShowHandleGrip write SetShowHandleGrip;
    property Style: TRBStyle read GetStyle write SetStyle;
  end;

implementation

uses
  Math, GR32_XPThemes;

const
  OppositeDirection: array [TRBDirection] of TRBDirection = (drRight, drDown, drLeft, drUp);
  tmScrollFirst = 1;
  tmScroll = 2;
  tmHotTrack = 3;

function ClrLighten(C: TColor; Amount: Integer): TColor;
var
  R, G, B: Integer;
begin
{$IFDEF Windows}
  if C < 0 then C := GetSysColor(C and $000000FF);
{$ELSE}
  C := ColorToRGB(C);
{$ENDIF}
  R := C and $FF + Amount;
  G := C shr 8 and $FF + Amount;
  B := C shr 16 and $FF + Amount;
  if R < 0 then R := 0 else if R > 255 then R := 255;
  if G < 0 then G := 0 else if G > 255 then G := 255;
  if B < 0 then B := 0 else if B > 255 then B := 255;
  Result := R or (G shl 8) or (B shl 16);
end;

function MixColors(C1, C2: TColor; W1: Integer): TColor;
var
  W2: Cardinal;
begin
  Assert(W1 in [0..255]);
  W2 := W1 xor 255;
{$IFDEF Windows}
  if Integer(C1) < 0 then C1 := GetSysColor(C1 and $000000FF);
  if Integer(C2) < 0 then C2 := GetSysColor(C2 and $000000FF);
{$ELSE}
  C1 := ColorToRGB(C1);
  C2 := ColorToRGB(C2);
{$ENDIF}
  Result := Integer(
    ((Cardinal(C1) and $FF00FF) * Cardinal(W1) +
    (Cardinal(C2) and $FF00FF) * W2) and $FF00FF00 +
    ((Cardinal(C1) and $00FF00) * Cardinal(W1) +
    (Cardinal(C2) and $00FF00) * W2) and $00FF0000) shr 8;
end;

procedure DitherRect(Canvas: TCanvas; const R: TRect; C1, C2: TColor);
var
{$IFDEF FPC}
  Brush: TBrush;
  OldBrush: TBrush;
{$ELSE}
  B: TBitmap;
  Brush: HBRUSH;
{$ENDIF}
begin
  if IsRectEmpty(R) then Exit;
{$IFDEF FPC}
  Brush := TBrush.Create;
  Brush.Color := ColorToRGB(C1);
  if C1 <> C2 then
  begin
    Brush.Bitmap := Graphics.TBitmap.Create;
    with Brush.Bitmap do
    begin
      Height := 2;
      Width := 2;
      Canvas.Pixels[0,0] := C1;
      Canvas.Pixels[1,0] := C2;
      Canvas.Pixels[0,1] := C2;
      Canvas.Pixels[1,1] := C1;
    end;
    Brush.Color := ColorToRGB(C1);
  end;
  OldBrush := TBrush.Create;
  OldBrush.Assign(Canvas.Brush);
  Canvas.Brush.Assign(Brush);
  Canvas.FillRect(R);
  Canvas.Brush.Assign(OldBrush);
  Brush.Free;
  OldBrush.Free;
{$ELSE}
  if C1 = C2 then
    Brush := CreateSolidBrush(ColorToRGB(C1))
  else
  begin
    B := AllocPatternBitmap(C1, C2);
    B.HandleType := bmDDB;
    Brush := CreatePatternBrush(B.Handle);
  end;
  FillRect(Canvas.Handle, R, Brush);
  DeleteObject(Brush);
{$ENDIF}
end;

procedure DrawRectEx(Canvas: TCanvas; var R: TRect; Sides: TRBDirections; C: TColor);
begin
  if Sides <> [] then with Canvas, R do
  begin
    Pen.Color := C;
    if drUp in Sides then
    begin
      MoveTo(Left, Top); LineTo(Right, Top); Inc(Top);
    end;
    if drDown in Sides then
    begin
      Dec(Bottom); MoveTo(Left, Bottom); LineTo(Right, Bottom);
    end;
    if drLeft in Sides then
    begin
      MoveTo(Left, Top); LineTo(Left, Bottom); Inc(Left);
    end;
    if drRight in Sides then
    begin
      Dec(Right); MoveTo(Right, Top); LineTo(Right, Bottom);
    end;
  end;
end;

procedure Frame3D(Canvas: TCanvas; var ARect: TRect; TopColor, BottomColor: TColor; AdjustRect: Boolean = True);
var
  TopRight, BottomLeft: TPoint;
begin
  with Canvas, ARect do
  begin
    Pen.Width := 1;
    Dec(Bottom); Dec(Right);
    TopRight.X := Right;
    TopRight.Y := Top;
    BottomLeft.X := Left;
    BottomLeft.Y := Bottom;
    Pen.Color := TopColor;
    PolyLine([BottomLeft, TopLeft, TopRight]);
    Pen.Color := BottomColor;
    Dec(Left);
    PolyLine([TopRight, BottomRight, BottomLeft]);
    if AdjustRect then
    begin
      Inc(Top); Inc(Left, 2);
    end
    else
    begin
      Inc(Left); Inc(Bottom); Inc(Right);
    end;
  end;
end;

procedure DrawHandle(Canvas: TCanvas; R: TRect; Color: TColor;
  Pushed, ShowGrip, IsHorz: Boolean; ColorBorder: TColor);
var
  CHi, CLo: TColor;
  I, S: Integer;
begin
  CHi := ClrLighten(Color, 24);
  CLo := ClrLighten(Color, -24);

  Canvas.Brush.Color := ColorBorder;
  FrameRect(Canvas.Handle, R, Canvas.Brush.Handle);

  InflateRect(R, -1, -1);
  if Pushed then Frame3D(Canvas, R, CLo, Color)
  else Frame3D(Canvas, R, CHi, MixColors(ColorBorder, Color, 96));
  Canvas.Brush.Color := Color;
  Canvas.FillRect(R);

  if ShowGrip then
  begin
    if Pushed then OffsetRect(R, 1, 1);
    if IsHorz then
    begin
      S := R.Right - R.Left;
      R.Left := (R.Left + R.Right) div 2 - 5;
      R.Right := R.Left + 2;
      Inc(R.Top); Dec(R.Bottom);

      if S > 10 then Frame3D(Canvas, R, CHi, CLo, False);
      Inc(R.Left, 3); Inc(R.Right, 3);
      Frame3D(Canvas, R, CHi, CLo, False);
      Inc(R.Left, 3); Inc(R.Right, 3);
      Frame3D(Canvas, R, CHi, CLo, False);
      Inc(R.Left, 3); Inc(R.Right, 3);
      if S > 10 then Frame3D(Canvas, R, CHi, CLo, False);
    end
    else
    begin
      I := (R.Top + R.Bottom) div 2;
      S := R.Bottom - R.Top;
      R.Top := I - 1;
      R.Bottom := I + 1;
      Dec(R.Right);
      Inc(R.Left);

      OffsetRect(R, 0, -4);
      if S > 10 then Frame3D(Canvas, R, CHi, CLo, False);

      OffsetRect(R, 0, 3);
      Frame3D(Canvas, R, CHi, CLo, False);

      OffsetRect(R, 0, 3);
      Frame3D(Canvas, R, CHi, CLo, False);

      if S > 10 then
      begin
        OffsetRect(R, 0, 3);
        Frame3D(Canvas, R, CHi, CLo, False);
      end;
    end;
  end;
end;

procedure DrawArrow(Canvas: TCanvas; R: TRect; Direction: TRBDirection; Color: TColor);
var
  X, Y, Sz, Shift: Integer;
begin
  X := (R.Left + R.Right - 1) div 2;
  Y := (R.Top + R.Bottom - 1) div 2;
  Sz := (Min(X - R.Left, Y - R.Top)) * 3 div 4 - 1;
  if Sz = 0 then Sz := 1;
  if Direction in [drUp, drLeft] then Shift := (Sz + 1) * 1 div 3
  else Shift := Sz * 1 div 3;
  Canvas.Pen.Color := Color;
  Canvas.Brush.Color := Color;
  case Direction of
    drUp:
      begin
        Inc(Y, Shift);
        Canvas.Polygon([Point(X + Sz, Y), Point(X, Y - Sz), Point(X - Sz, Y)]);
      end;
    drDown:
      begin
        Dec(Y, Shift);
        Canvas.Polygon([Point(X + Sz, Y), Point(X, Y + Sz), Point(X - Sz, Y)]);
      end;
    drLeft:
      begin
        Inc(X, Shift);
        Canvas.Polygon([Point(X, Y + Sz), Point(X - Sz, Y), Point(X, Y - Sz)]);
      end;
    drRight:
      begin
        Dec(X, Shift);
        Canvas.Polygon([Point(X, Y + Sz), Point(X + Sz, Y), Point(X, Y - Sz)]);
      end;
  end;
end;

const
  FIRST_DELAY = 600;
  SCROLL_INTERVAL = 100;
  HOTTRACK_INTERVAL = 150;
  MIN_SIZE = 17;

{ TArrowBar }

{$IFDEF FPC}
procedure TArrowBar.CMEnabledChanged(var Message: TLMessage);
{$ELSE}
procedure TArrowBar.CMEnabledChanged(var Message: TMessage);
{$ENDIF}
begin
  inherited;
  Invalidate;
end;

{$IFDEF FPC}
procedure TArrowBar.CMMouseLeave(var Message: TLMessage);
{$ELSE}
procedure TArrowBar.CMMouseLeave(var Message: TMessage);
{$ENDIF}
begin
  MouseLeft;
end;

constructor TArrowBar.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csAcceptsControls, csDoubleClicks] + [csOpaque];
  Width := 100;
  Height := 16;
  ParentColor := False;
  Color := clScrollBar;
  Timer := TTimer.Create(Self);
  Timer.OnTimer := TimerHandler;
  FShowArrows := True;
  FBorderStyle := bsSingle;
  FHandleColor := clBtnShadow;
  FButtonColor := clBtnFace;
  FHighLightColor := clBtnHighlight;
  FShadowColor := clBtnShadow;
  FBorderColor := clWindowFrame;
  FShowHandleGrip := True;
end;

procedure TArrowBar.DoChange;
begin
  if Assigned(FOnChange) then FOnChange(Self);
  if GenChange and Assigned(FOnUserChange) then FOnUserChange(Self);
end;

procedure TArrowBar.DoDrawButton(R: TRect; Direction: TRBDirection; Pushed, Enabled, Hot: Boolean);
const
  EnabledFlags: array [Boolean] of Integer = (DFCS_INACTIVE, 0);
  PushedFlags: array [Boolean] of Integer = (0, DFCS_PUSHED or DFCS_FLAT);
  DirectionFlags: array [TRBDirection] of Integer = (DFCS_SCROLLLEFT, DFCS_SCROLLUP,
    DFCS_SCROLLRIGHT, DFCS_SCROLLDOWN);
{$IFDEF Windows}
  DirectionXPFlags: array [TRBDirection] of Cardinal = (ABS_LEFTNORMAL,
    ABS_UPNORMAL, ABS_RIGHTNORMAL, ABS_DOWNNORMAL);
{$ENDIF}
var
  Edges: TRBDirections;
{$IFDEF Windows}
  Flags: Integer;
{$ENDIF}
begin
  if Style = rbsDefault then
  begin
{$IFDEF FPC}
{$IFNDEF Windows}
    Canvas.Brush.Color := clButton;
    Canvas.FillRect(R);
    LCLIntf.DrawFrameControl(Canvas.Handle, R, DFC_BUTTON, 0);
    InflateRect(R, -2, -2);

    If not DrawEnabled then
    begin
      InflateRect(R, -1, -1);
      OffsetRect(R, 1, 1);
      DrawArrow(Canvas, R, Direction, clWhite);
      OffsetRect(R, -1, -1);
      DrawArrow(Canvas, R, Direction, clDisabledButtonText);
    end
    else
    begin
      If Pushed then OffsetRect(R, 1, 1);
      DrawArrow(Canvas, R, Direction, clButtonText);
    end;
{$ENDIF}
{$ENDIF}

{$IFDEF Windows}
    if USE_THEMES then
    begin
      Flags := DirectionXPFlags[Direction];
      if not Enabled then Inc(Flags, 3)
      else if Pushed then Inc(Flags, 2)
      else if Hot then Inc(Flags);
      DrawThemeBackground(SCROLLBAR_THEME, Canvas.Handle, SBP_ARROWBTN, Flags, R, nil);
    end
    else
      DrawFrameControl(Canvas.Handle, R, DFC_SCROLL,
        DirectionFlags[Direction] or EnabledFlags[DrawEnabled] or PushedFlags[Pushed])
{$ENDIF}
  end
  else
  begin
    Edges := [drLeft, drUp, drRight, drDown];
    Exclude(Edges, OppositeDirection[Direction]);

    if not DrawEnabled then
    begin
      DrawRectEx(Canvas, R, Edges, fShadowColor);
      Canvas.Brush.Color := fButtonColor;
      FillRect(Canvas.Handle, R, Canvas.Brush.Handle);
      InflateRect(R, -1, -1);
      OffsetRect(R, 1, 1);
      DrawArrow(Canvas, R, Direction, fHighLightColor);
      OffsetRect(R, -1, -1);
      DrawArrow(Canvas, R, Direction, fShadowColor);
    end
    else
    begin
      DrawRectEx(Canvas, R, Edges, fBorderColor);
      if Pushed then
      begin
        Canvas.Brush.Color := fButtonColor;
        FillRect(Canvas.Handle, R, Canvas.Brush.Handle);
        OffsetRect(R, 1, 1);
        InflateRect(R, -1, -1);
      end
      else
      begin
        Frame3D(Canvas, R, fHighLightColor, fShadowColor, True);
        Canvas.Brush.Color := fButtonColor;
        FillRect(Canvas.Handle, R, Canvas.Brush.Handle);
      end;
      DrawArrow(Canvas, R, Direction, fBorderColor);
    end;
  end;
end;

procedure TArrowBar.DoDrawHandle(R: TRect; Horz, Pushed, Hot: Boolean);
{$IFDEF Windows}
const
  PartXPFlags: array [Boolean] of Cardinal = (SBP_THUMBBTNVERT, SBP_THUMBBTNHORZ);
  GripperFlags: array [Boolean] of Cardinal = (SBP_GRIPPERVERT, SBP_GRIPPERHORZ);
var
  Flags: Cardinal;
{$ENDIF}
begin
  if IsRectEmpty(R) then Exit;
  case Style of
    rbsDefault:
    begin
{$IFDEF Windows}
      if USE_THEMES then
      begin
        Flags := SCRBS_NORMAL;
        if not Enabled then Inc(Flags, 3)
        else if Pushed then Inc(Flags, 2)
        else if Hot then Inc(Flags);
        DrawThemeBackground(SCROLLBAR_THEME, Canvas.Handle, PartXPFlags[Horz], Flags, R, nil);
        if ShowHandleGrip then
          DrawThemeBackground(SCROLLBAR_THEME, Canvas.Handle, GripperFlags[Horz], 0, R, nil);
      end
      else
        DrawEdge(Canvas.Handle, R, EDGE_RAISED, BF_RECT or BF_MIDDLE);
{$ENDIF}
    end;

    rbsMac:
    begin
      DrawHandle(Canvas, R, HandleColor, Pushed, ShowHandleGrip, Horz, fBorderColor);
    end;
  end;
end;

procedure TArrowBar.DoDrawTrack(R: TRect; Direction: TRBDirection; Pushed, Enabled, Hot: Boolean);
{$IFDEF Windows}
const
  PartXPFlags: array [TRBDirection] of Cardinal =
    (SBP_LOWERTRACKHORZ, SBP_LOWERTRACKVERT, SBP_UPPERTRACKHORZ, SBP_UPPERTRACKVERT);
{$ENDIF}
var
{$IFDEF Windows}
  Flags: Cardinal;
{$ENDIF}
  C: TColor;
  Edges: set of TRBDirection;
begin
  if (R.Right <= R.Left) or (R.Bottom <= R.Top) then Exit;
  if Style = rbsDefault then
  begin
{$IFDEF Windows}
    if USE_THEMES then
    begin
      Flags := SCRBS_NORMAL;
      if Pushed then Inc(Flags, 2);
      DrawThemeBackground(SCROLLBAR_THEME, Canvas.Handle, PartXPFlags[Direction], Flags, R, nil);
    end
    else
{$ENDIF}
    begin
      if Pushed then DitherRect(Canvas, R, clWindowFrame, clWindowFrame)
      else DitherRect(Canvas, R, clBtnHighlight, Color);
    end;
  end
  else
  with Canvas, R do
  begin
    if DrawEnabled then C := FBorderColor
    else C := FShadowColor;
    Edges := [drLeft, drUp, drRight, drDown];
    Exclude(Edges, OppositeDirection[Direction]);
    DrawRectEx(Canvas, R, Edges, C);
    if Pushed then DitherRect(Canvas, R, fBorderColor,fBorderColor)
    else if not IsRectEmpty(R) then with R do
    begin
      if DrawEnabled then
      begin
        Pen.Color := MixColors(fBorderColor, MixColors(fHighLightColor, Color, 127), 32);
        case Direction of
          drLeft, drUp:
            begin
              MoveTo(Left, Bottom - 1); LineTo(Left, Top); LineTo(Right, Top);
              Inc(Top); Inc(Left);
            end;
          drRight:
            begin
              MoveTo(Left, Top); LineTo(Right, Top);
              Inc(Top);
            end;
          drDown:
            begin
              MoveTo(Left, Top); LineTo(Left, Bottom);
              Inc(Left);
            end;
        end;
        if Backgnd = bgPattern then DitherRect(Canvas, R, fHighLightColor, Color)
        else DitherRect(Canvas, R, Color, Color);
      end
      else
      begin
        Brush.Color := fButtonColor;
        FillRect(R);
      end;
    end;
  end;
end;

function TArrowBar.DrawEnabled: Boolean;
begin
  Result := Enabled;
end;

function TArrowBar.GetBorderSize: Integer;
const
  CSize: array [Boolean] of Integer = (0, 1);
begin
  Result := CSize[BorderStyle = bsSingle];
end;

function TArrowBar.GetButtonSize: Integer;
var
  W, H: Integer;
begin
  if not ShowArrows then Result := 0
  else
  begin
    Result := ButtonSize;
    if Kind = sbHorizontal then
    begin
      W := ClientWidth;
      H := ClientHeight;
    end
    else
    begin
      W := ClientHeight;
      H := ClientWidth;
    end;
    if Result = 0 then Result := Min(H, 32);
    if Result * 2 >= W then Result := W div 2;
    if Style = rbsMac then Dec(Result);
    if Result < 2 then Result := 0;
  end;
end;

function TArrowBar.GetHandleRect: TRect;
begin
  Result := Rect(0, 0, 0, 0);
end;

function TArrowBar.GetTrackBoundary: TRect;
begin
  Result := ClientRect;
  if Kind = sbHorizontal then InflateRect(Result, -GetButtonSize, 0)
  else InflateRect(Result, 0, -GetButtonSize);
end;

function TArrowBar.GetZone(X, Y: Integer): TRBZone;
var
  P: TPoint;
  R, R1: TRect;
  Sz: Integer;
begin
  Result := zNone;

  P := Point(X, Y);
  R := ClientRect;
  if not PtInrect(R, P) then Exit;

  Sz := GetButtonSize;
  R1 := R;
  if Kind = sbHorizontal then
  begin
    R1.Right := R1.Left + Sz;
    if PtInRect(R1, P) then Result := zBtnPrev
    else
    begin
      R1.Right := R.Right;
      R1.Left := R.Right - Sz;
      if PtInRect(R1, P) then Result := zBtnNext;
    end;
  end
  else
  begin
    R1.Bottom := R1.Top + Sz;
    if PtInRect(R1, P) then Result := zBtnPrev
    else
    begin
      R1.Bottom := R.Bottom;
      R1.Top := R.Bottom - Sz;
      if PtInRect(R1, P) then Result := zBtnNext;
    end;
  end;

  if Result = zNone then
  begin
    R := GetHandleRect;
    P := Point(X, Y);
    if PtInRect(R, P) then Result := zHandle
    else
    begin
      if Kind = sbHorizontal then
      begin
        if (X > 0) and (X < R.Left) then Result := zTrackPrev
        else if (X >= R.Right) and (X < ClientWidth - 1) then Result := zTrackNext;
      end
      else
      begin
        if (Y > 0) and (Y < R.Top) then Result := zTrackPrev
        else if (Y >= R.Bottom) and (Y < ClientHeight - 1) then Result := zTrackNext;
      end;
    end;
  end;
end;

function TArrowBar.GetZoneRect(Zone: TRBZone): TRect;
const
  CEmptyRect: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);
var
  BtnSize: Integer;
  Horz: Boolean;
  R: TRect;
begin
  Horz := Kind = sbHorizontal;
  BtnSize:= GetButtonSize;
  case Zone of
    zNone: Result := CEmptyRect;
    zBtnPrev:
      begin
        Result := ClientRect;
        if Horz then Result.Right := Result.Left + BtnSize
        else Result.Bottom := Result.Top + BtnSize;
      end;
    zTrackPrev..zTrackNext:
      begin
        Result := GetTrackBoundary;
        R := GetHandleRect;
        if not DrawEnabled or IsRectEmpty(R) then
        begin
          R.Left := (Result.Left + Result.Right) div 2;
          R.Top := (Result.Top + Result.Bottom) div 2;
          R.Right := R.Left;
          R.Bottom := R.Top;
        end;
        case Zone of
          zTrackPrev:
            if Horz then Result.Right := R.Left
            else Result.Bottom := R.Top;
          zHandle:
            Result := R;
          zTrackNext:
            if Horz then Result.Left := R.Right
            else Result.Top := R.Bottom;
        end;
      end;
    zBtnNext:
      begin
        Result := ClientRect;
        if Horz then Result.Left := Result.Right - BtnSize
        else Result.Top := Result.Bottom - BtnSize;
      end;
  end;
end;

procedure TArrowBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button <> mbLeft then Exit;
  DragZone := GetZone(X, Y);
  Invalidate;
  StoredX := X;
  StoredY := Y;
  StartDragTracking;
end;

procedure TArrowBar.MouseLeft;
begin
  StopHotTracking;
end;

procedure TArrowBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewHotZone: TRBZone;
begin
  inherited;
  if (DragZone = zNone) and DrawEnabled then
  begin
    NewHotZone := GetZone(X, Y);
    if NewHotZone <> HotZone then
    begin
      HotZone := NewHotZone;
      if HotZone <> zNone then StartHotTracking;
      Invalidate;
    end;
  end;
end;

procedure TArrowBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  DragZone := zNone;
  Invalidate;
  StopDragTracking;
end;

procedure TArrowBar.Paint;
const
  CPrevDirs: array [Boolean] of TRBDirection = (drUp, drLeft);
  CNextDirs: array [Boolean] of TRBDirection = (drDown, drRight);
var
  BSize: Integer;
  ShowEnabled: Boolean;
  R, BtnRect, HandleRect: TRect;
  Horz, ShowHandle: Boolean;
begin
  R := ClientRect;
  Horz := Kind = sbHorizontal;
  ShowEnabled := DrawEnabled;
  BSize := GetButtonSize;

  if ShowArrows then
  begin
    { left / top button }
    BtnRect := R;
    with BtnRect do if Horz then Right := Left + BSize else Bottom := Top + BSize;
    DoDrawButton(BtnRect, CPrevDirs[Horz], DragZone = zBtnPrev, ShowEnabled, HotZone = zBtnPrev);

    { right / bottom button }
    BtnRect := R;
    with BtnRect do if Horz then Left := Right - BSize else Top := Bottom - BSize;
    DoDrawButton(BtnRect, CNextDirs[Horz], DragZone = zBtnNext, ShowEnabled, HotZone = zBtnNext);
  end;

  if Horz then InflateRect(R, -BSize, 0) else InflateRect(R, 0, -BSize);
  if ShowEnabled then HandleRect := GetHandleRect
  else HandleRect := Rect(0, 0, 0, 0);
  ShowHandle := not IsRectEmpty(HandleRect);

  DoDrawTrack(GetZoneRect(zTrackPrev), CPrevDirs[Horz], DragZone = zTrackPrev, ShowEnabled, HotZone = zTrackPrev);
  DoDrawTrack(GetZoneRect(zTrackNext), CNextDirs[Horz], DragZone = zTrackNext, ShowEnabled, HotZone = zTrackNext);
  if ShowHandle then DoDrawHandle(HandleRect, Horz, DragZone = zHandle, HotZone = zHandle);
end;

procedure TArrowBar.SetBackgnd(Value: TRBBackgnd);
begin
  if Value <> FBackgnd then
  begin
    FBackgnd := Value;
    Invalidate;
  end;
end;

procedure TArrowBar.SetBorderStyle(Value: TBorderStyle);
begin
  if Value <> FBorderStyle then
  begin
    FBorderStyle := Value;
{$IFNDEF FPC}
    RecreateWnd;
{$ELSE}
    Invalidate;
{$ENDIF}
  end;
end;

procedure TArrowBar.SetButtonSize(Value: Integer);
begin
  if Value <> FButtonSize then
  begin
    FButtonSize := Value;
    Invalidate;
  end;
end;

procedure TArrowBar.SetHandleColor(Value: TColor);
begin
  if Value <> FHandleColor then
  begin
    FHandleColor := Value;
    Invalidate;
  end;
end;

procedure TArrowBar.SetHighLightColor(Value: TColor);
begin
  if Value <> FHighLightColor then
  begin
    FHighLightColor := Value;
    Invalidate;
  end;
end;

procedure TArrowBar.SetButtonColor(Value: TColor);
begin
  if Value <> FButtonColor then
  begin
    FButtonColor := Value;
    Invalidate;
  end;
end;

procedure TArrowBar.SetBorderColor(Value: TColor);
begin
  if Value <> FBorderColor then
  begin
    FBorderColor := Value;
    Invalidate;
  end;
end;

procedure TArrowBar.SetShadowColor(Value: TColor);
begin
  if Value <> FShadowColor then
  begin
    FShadowColor := Value;
    Invalidate;
  end;
end;

procedure TArrowBar.SetKind(Value: TScrollBarKind);
var
  Tmp: Integer;
begin
  if Value <> FKind then
  begin
    FKind := Value;
    if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    begin
      Tmp := Width;
      Width := Height;
      Height := Tmp;
    end;
    Invalidate;
  end;
end;

procedure TArrowBar.SetShowArrows(Value: Boolean);
begin
  if Value <> FShowArrows then
  begin
    FShowArrows := Value;
    Invalidate;
  end;
end;

procedure TArrowBar.SetShowHandleGrip(Value: Boolean);
begin
  if Value <> FShowHandleGrip then
  begin
    FShowHandleGrip := Value;
    Invalidate;
  end;
end;

procedure TArrowBar.SetStyle(Value: TRBStyle);
begin
  FStyle := Value;
{$IFDEF FPC}
  Invalidate;
{$ELSE}
  RecreateWnd;
{$ENDIF}
end;

procedure TArrowBar.StartDragTracking;
begin
  Timer.Interval := FIRST_DELAY;
  TimerMode := tmScroll;
  TimerHandler(Self);
  TimerMode := tmScrollFirst;
  Timer.Enabled := True;
end;

procedure TArrowBar.StartHotTracking;
begin
  Timer.Interval := HOTTRACK_INTERVAL;
  TimerMode := tmHotTrack;
  Timer.Enabled := True;
end;

procedure TArrowBar.StopDragTracking;
begin
  StartHotTracking;
end;

procedure TArrowBar.StopHotTracking;
begin
  Timer.Enabled := False;
  HotZone := zNone;
  Invalidate;
end;

procedure TArrowBar.TimerHandler(Sender: TObject);
var
  Pt: TPoint;
begin
  case TimerMode of
    tmScrollFirst:
      begin
        Timer.Interval := SCROLL_INTERVAL;
        TimerMode := tmScroll;
      end;
    tmHotTrack:
      begin
        Pt := ScreenToClient(Mouse.CursorPos);
        if not PtInRect(ClientRect, Pt) then
        begin
          StopHotTracking;
          Invalidate;
        end;
      end;
  end;
end;

{$IFDEF FPC}
procedure TArrowBar.WMEraseBkgnd(var Message: TLmEraseBkgnd);
begin
  Message.Result := -1;
end;

procedure TArrowBar.WMNCCalcSize(var Message: TLMNCCalcSize);
var
  Sz: Integer;
begin
  Sz := GetBorderSize;
  InflateRect(Message.CalcSize_Params.rgrc[0], -Sz, -Sz);
end;

{$IFDEF Windows}
procedure TArrowBar.WMNCPaint(var Message: TWMNCPaint);

  procedure DrawNCArea(ADC: HDC; const Clip: HRGN);
  var
    DC: HDC;
    R: TRect;
  begin
    if BorderStyle = bsNone then Exit;
    if ADC = 0 then DC := GetWindowDC(Handle)
    else DC := ADC;
    try
      GetWindowRect(Handle, R);
      OffsetRect(R, -R.Left, -R.Top);
      DrawEdge(DC, R, BDR_SUNKENOUTER, BF_RECT);
    finally
      if ADC = 0 then ReleaseDC(Handle, DC);
    end;
  end;

begin
  DrawNCArea(0, Message.RGN);
end;
{$ENDIF}

{$ELSE}

procedure TArrowBar.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := -1;
end;

procedure TArrowBar.WMNCCalcSize(var Message: TWMNCCalcSize);
var
  Sz: Integer;
begin
  Sz := GetBorderSize;
  InflateRect(Message.CalcSize_Params.rgrc[0], -Sz, -Sz);
end;

procedure TArrowBar.WMNCPaint(var Message: TWMNCPaint);

  procedure DrawNCArea(ADC: HDC; const Clip: HRGN);
  var
    DC: HDC;
    R: TRect;
  begin
    if BorderStyle = bsNone then Exit;
    if ADC = 0 then DC := GetWindowDC(Handle)
    else DC := ADC;
    try
      GetWindowRect(Handle, R);
      OffsetRect(R, -R.Left, -R.Top);
      DrawEdge(DC, R, BDR_SUNKENOUTER, BF_RECT);
    finally
      if ADC = 0 then ReleaseDC(Handle, DC);
    end;
  end;

begin
  DrawNCArea(0, Message.RGN);
end;
{$ENDIF}

{ TCustomRangeBar }

procedure TCustomRangeBar.AdjustPosition;
begin
  if FPosition > Range - EffectiveWindow then FPosition := Range - EffectiveWindow;
  if FPosition < 0 then FPosition := 0;
end;

constructor TCustomRangeBar.Create(AOwner: TComponent);
begin
  inherited;
  FIncrement := 8;
end;

function TCustomRangeBar.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
const OneHundredTwenteenth = 1 / 120;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if not Result then Position := Position + Increment * WheelDelta * OneHundredTwenteenth;
  Result := True;
end;

function TCustomRangeBar.DrawEnabled: Boolean;
begin
  Result := Enabled and (Range > EffectiveWindow);
end;

function TCustomRangeBar.GetHandleRect: TRect;
var
  BtnSz, ClientSz: Integer;
  HandleSz, HandlePos: Integer;
  R: TRect;
  Horz: Boolean;
begin
  R := Rect(0, 0, ClientWidth, ClientHeight);
  Horz := Kind = sbHorizontal;
  BtnSz := GetButtonSize;
  if Horz then
  begin
    InflateRect(R, -BtnSz, 0);
    ClientSz := R.Right - R.Left;
  end
  else
  begin
    InflateRect(R, 0, -BtnSz);
    ClientSz := R.Bottom - R.Top;
  end;
  if ClientSz < 18 then
  begin
    Result := Rect(0, 0, 0, 0);
    Exit;
  end;

  if Range > EffectiveWindow then
  begin
    HandleSz := Round(ClientSz * EffectiveWindow / Range);
    if HandleSz >= MIN_SIZE then HandlePos := Round(ClientSz * Position / Range)
    else
    begin
      HandleSz := MIN_SIZE;
      HandlePos := Round((ClientSz - MIN_SIZE) * Position / (Range - EffectiveWindow));
    end;
    Result := R;
    if Horz then
    begin
      Result.Left := R.Left + HandlePos;
      Result.Right := R.Left + HandlePos + HandleSz;
    end
    else
    begin
      Result.Top := R.Top + HandlePos;
      Result.Bottom := R.Top + HandlePos + HandleSz;
    end;
  end
  else Result := R;
end;

function TCustomRangeBar.IsPositionStored: Boolean;
begin
  Result := FPosition > 0;
end;

procedure TCustomRangeBar.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Range <= EffectiveWindow then DragZone := zNone
  else
  begin
    inherited;
    if DragZone = zHandle then
    begin
      StopDragTracking;
      PosBeforeDrag := Position;
    end;
  end;
end;

procedure TCustomRangeBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Delta: Single;
  WinSz: Single;
  ClientSz, HandleSz: Integer;
begin
  inherited;
  if DragZone = zHandle then
  begin
    WinSz := EffectiveWindow;

    if Range <= WinSz then Exit;
    if Kind = sbHorizontal then Delta := X - StoredX else Delta := Y - StoredY;

    if Kind = sbHorizontal then ClientSz := ClientWidth  else ClientSz := ClientHeight;
    Dec(ClientSz, GetButtonSize * 2);
    if BorderStyle = bsSingle then Dec(ClientSz, 2);
    HandleSz := Round(ClientSz * WinSz / Range);

    if HandleSz < MIN_SIZE then Delta := Round(Delta * (Range - WinSz) / (ClientSz - MIN_SIZE))
    else Delta := Delta * Range / ClientSz;

    GenChange := True;
    Position := PosBeforeDrag + Delta;
    GenChange := False;
  end;
end;

procedure TCustomRangeBar.Resize;
var
  OldWindow: Integer;
  Center: Single;
begin
  if Centered then
  begin
    OldWindow := EffectiveWindow;
    UpdateEffectiveWindow;
    if Range > EffectiveWindow then
    begin
      if (Range > OldWindow) and (Range <> 0) then Center := (FPosition + OldWindow * 0.5) / Range
      else Center := 0.5;
      FPosition := Center * Range - EffectiveWindow * 0.5;
    end;
  end;
  AdjustPosition;
  inherited;
end;

procedure TCustomRangeBar.SetParams(NewRange, NewWindow: Integer);
var
  OldWindow, OldRange: Integer;
  Center: Single;
begin
  if NewRange < 0 then NewRange := 0;
  if NewWindow < 0 then NewWindow := 0;
  if (NewRange <> FRange) or (NewWindow <> EffectiveWindow) then
  begin
    OldWindow := EffectiveWindow;
    OldRange := Range;
    FRange := NewRange;
    FWindow := NewWindow;
    UpdateEffectiveWindow;
    if Centered and (Range > EffectiveWindow) then
    begin
      if (OldRange > OldWindow) and (OldRange <> 0) then
        Center := (FPosition + OldWindow * 0.5) / OldRange
      else
        Center := 0.5;
      FPosition := Center * Range - EffectiveWindow * 0.5;
    end;
    AdjustPosition;
    Invalidate;
  end;
end;

procedure TCustomRangeBar.SetPosition(Value: Single);
var
  OldPosition: Single;
begin
  if Value <> FPosition then
  begin
    OldPosition := FPosition;
    FPosition := Value;
    AdjustPosition;
    if OldPosition <> FPosition then
    begin
      Invalidate;
      DoChange;
    end;
  end;
end;

procedure TCustomRangeBar.SetRange(Value: Integer);
begin
  SetParams(Value, Window);
end;

procedure TCustomRangeBar.SetWindow(Value: Integer);
begin
  SetParams(Range, Value);
end;

procedure TCustomRangeBar.TimerHandler(Sender: TObject);
var
  OldPosition: Single;
  Pt: TPoint;

  function MousePos: TPoint;
  begin
    Result := ScreenToClient(Mouse.CursorPos);
    if Result.X < 0 then Result.X := 0;
    if Result.Y < 0 then Result.Y := 0;
    if Result.X >= ClientWidth then Result.X := ClientWidth - 1;
    if Result.Y >= ClientHeight then Result.Y := ClientHeight - 1
  end;

begin
  inherited;
  GenChange := True;
  OldPosition := Position;

  case DragZone of
    zBtnPrev:
      begin
        Position := Position - Increment;
        if Position = OldPosition then StopDragTracking;
      end;

    zBtnNext:
      begin
        Position := Position + Increment;
        if Position = OldPosition then StopDragTracking;
      end;

    zTrackNext:
      begin
        Pt := MousePos;
        if GetZone(Pt.X, Pt.Y) in [zTrackNext, zBtnNext] then
        Position := Position + EffectiveWindow;
      end;

    zTrackPrev:
      begin
        Pt := MousePos;
        if GetZone(Pt.X, Pt.Y) in [zTrackPrev, zBtnPrev] then
        Position := Position - EffectiveWindow;
      end;
  end;
  GenChange := False;
end;

procedure TCustomRangeBar.UpdateEffectiveWindow;
begin
  if FWindow > 0 then FEffectiveWindow := FWindow
  else
  begin
    if Kind = sbHorizontal then FEffectiveWindow := Width
    else FEffectiveWindow := Height;
  end;
end;

//----------------------------------------------------------------------------//

{ TCustomGaugeBar }

procedure TCustomGaugeBar.AdjustPosition;
begin
  if Position < Min then Position := Min
  else if Position > Max then Position := Max;
end;

constructor TCustomGaugeBar.Create(AOwner: TComponent);
begin
  inherited;
  FLargeChange := 1;
  FMax := 100;
  FSmallChange := 1;
end;

function TCustomGaugeBar.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if not Result then Position := Position + FSmallChange * WheelDelta div 120;
  Result := True;
end;

function TCustomGaugeBar.GetHandleRect: TRect;
var
  Sz, HandleSz: Integer;
  Horz: Boolean;
  Pos: Integer;
begin
  Result := GetTrackBoundary;
  Horz := Kind = sbHorizontal;
  HandleSz := GetHandleSize;

  if Horz then Sz := Result.Right - Result.Left
  else Sz := Result.Bottom - Result.Top;

  Pos := Round((Position - Min) / (Max - Min) * (Sz - GetHandleSize));

  if Horz then
  begin
    Inc(Result.Left, Pos);
    Result.Right := Result.Left + HandleSz;
  end
  else
  begin
    Inc(Result.Top, Pos);
    Result.Bottom := Result.Top + HandleSz;
  end;
end;

function TCustomGaugeBar.GetHandleSize: Integer;
var
  R: TRect;
  Sz: Integer;
begin
  Result := HandleSize;
  if Result = 0 then
  begin
    if Kind = sbHorizontal then Result := ClientHeight else Result := ClientWidth;
  end;
  R := GetTrackBoundary;
  if Kind = sbHorizontal then Sz := R.Right - R.Left
  else Sz := R.Bottom - R.Top;
  if Sz - Result < 1 then Result := Sz - 1;
  if Result < 0 then Result := 0;
end;

procedure TCustomGaugeBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if DragZone = zHandle then
  begin
    StopDragTracking;
    PosBeforeDrag := Position;
  end;
end;

procedure TCustomGaugeBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Delta: Single;
  R: TRect;
  ClientSz: Integer;
begin
  inherited;
  if DragZone = zHandle then
  begin
    if Kind = sbHorizontal then Delta := X - StoredX else Delta := Y - StoredY;
    R := GetTrackBoundary;

    if Kind = sbHorizontal then ClientSz := R.Right - R.Left
    else ClientSz := R.Bottom - R.Top;

    Delta := Delta * (Max - Min) / (ClientSz - GetHandleSize);

    GenChange := True;
    Position := Round(PosBeforeDrag + Delta);
    GenChange := False;
  end;
end;

procedure TCustomGaugeBar.SetHandleSize(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if Value <> FHandleSize then
  begin
    FHandleSize := Value;
    Invalidate;
  end;
end;

procedure TCustomGaugeBar.SetLargeChange(Value: Integer);
begin
  if Value < 1 then Value := 1;
  FLargeChange := Value;
end;

procedure TCustomGaugeBar.SetMax(Value: Integer);
begin
  if (Value <= FMin) and not (csLoading in ComponentState) then Value := FMin + 1;
  if Value <> FMax then
  begin
    FMax := Value;
    AdjustPosition;
    Invalidate;
  end;
end;

procedure TCustomGaugeBar.SetMin(Value: Integer);
begin
  if (Value >= FMax) and not (csLoading in ComponentState) then Value := FMax - 1;
  if Value <> FMin then
  begin
    FMin := Value;
    AdjustPosition;
    Invalidate;
  end;
end;

procedure TCustomGaugeBar.SetPosition(Value: Integer);
begin
  if Value < Min then Value := Min
  else if Value > Max then Value := Max;
  if Round(FPosition) <> Value then
  begin
    FPosition := Value;
    Invalidate;
    DoChange;
  end;
end;

procedure TCustomGaugeBar.SetSmallChange(Value: Integer);
begin
  if Value < 1 then Value := 1;
  FSmallChange := Value;
end;

procedure TCustomGaugeBar.TimerHandler(Sender: TObject);
var
  OldPosition: Single;
  Pt: TPoint;

  function MousePos: TPoint;
  begin
    Result := ScreenToClient(Mouse.CursorPos);
    if Result.X < 0 then Result.X := 0;
    if Result.Y < 0 then Result.Y := 0;
    if Result.X >= ClientWidth then Result.X := ClientWidth - 1;
    if Result.Y >= ClientHeight then Result.Y := ClientHeight - 1
  end;

begin
  inherited;
  GenChange := True;
  OldPosition := Position;

  case DragZone of
    zBtnPrev:
      begin
        Position := Position - SmallChange;
        if Position = OldPosition then StopDragTracking;
      end;

    zBtnNext:
      begin
        Position := Position + SmallChange;
        if Position = OldPosition then StopDragTracking;
      end;

    zTrackNext:
      begin
        Pt := MousePos;
        if GetZone(Pt.X, Pt.Y) in [zTrackNext, zBtnNext] then
        Position := Position + LargeChange;
      end;

    zTrackPrev:
      begin
        Pt := MousePos;
        if GetZone(Pt.X, Pt.Y) in [zTrackPrev, zBtnPrev] then
        Position := Position - LargeChange;
      end;
  end;
  GenChange := False;
end;

{ TArrowBarAccess }

function TArrowBarAccess.GetBackgnd: TRBBackgnd;
begin
  Result := FMaster.Backgnd;
end;

function TArrowBarAccess.GetButtonSize: Integer;
begin
  Result := FMaster.ButtonSize;
end;

function TArrowBarAccess.GetColor: TColor;
begin
  Result := FMaster.Color;
end;

function TArrowBarAccess.GetHandleColor: TColor;
begin
  Result := FMaster.HandleColor;
end;

function TArrowBarAccess.GetHighLightColor: TColor;
begin
  Result := FMaster.HighLightColor;
end;

function TArrowBarAccess.GetShadowColor: TColor;
begin
  Result := FMaster.ShadowColor;
end;

function TArrowBarAccess.GetButtonColor: TColor;
begin
  Result := FMaster.ButtonColor;
end;

function TArrowBarAccess.GetBorderColor: TColor;
begin
  Result := FMaster.BorderColor;
end;

function TArrowBarAccess.GetShowArrows: Boolean;
begin
  Result := FMaster.ShowArrows;
end;

function TArrowBarAccess.GetShowHandleGrip: Boolean;
begin
  Result := FMaster.ShowHandleGrip;
end;

function TArrowBarAccess.GetStyle: TRBStyle;
begin
  Result := FMaster.Style;
end;

procedure TArrowBarAccess.SetBackgnd(Value: TRBBackgnd);
begin
  FMaster.Backgnd := Value;
  if FSlave <> nil then FSlave.Backgnd := Value;
end;

procedure TArrowBarAccess.SetButtonSize(Value: Integer);
begin
  FMaster.ButtonSize := Value;
  if FSlave <> nil then FSlave.ButtonSize := Value;
end;

procedure TArrowBarAccess.SetColor(Value: TColor);
begin
  FMaster.Color := Value;
  if FSlave <> nil then FSlave.Color := Value;
end;

procedure TArrowBarAccess.SetHandleColor(Value: TColor);
begin
  FMaster.HandleColor := Value;
  if FSlave <> nil then FSlave.HandleColor := Value;
end;

procedure TArrowBarAccess.SetHighLightColor(Value: TColor);
begin
  FMaster.HighLightColor := Value;
  if FSlave <> nil then FSlave.HighLightColor := Value;
end;

procedure TArrowBarAccess.SetShadowColor(Value: TColor);
begin
  FMaster.ShadowColor := Value;
  if FSlave <> nil then FSlave.ShadowColor := Value;
end;

procedure TArrowBarAccess.SetButtonColor(Value: TColor);
begin
  FMaster.ButtonColor := Value;
  if FSlave <> nil then FSlave.ButtonColor := Value;
end;

procedure TArrowBarAccess.SetBorderColor(Value: TColor);
begin
  FMaster.BorderColor := Value;
  if FSlave <> nil then FSlave.BorderColor := Value;
end;

procedure TArrowBarAccess.SetShowArrows(Value: Boolean);
begin
  FMaster.ShowArrows := Value;
  if FSlave <> nil then FSlave.ShowArrows := Value;
end;

procedure TArrowBarAccess.SetShowHandleGrip(Value: Boolean);
begin
  FMaster.ShowHandleGrip := Value;
  if FSlave <> nil then FSlave.ShowHandleGrip := Value;
end;

procedure TArrowBarAccess.SetStyle(Value: TRBStyle);
begin
  FMaster.Style := Value;
  if FSlave <> nil then FSlave.Style := Value;
end;

{$IFDEF FPC}
initialization
//  PatternManager := TPatternManager.Create;
finalization
//  PatternManager.Free;
{$ENDIF}

end.
