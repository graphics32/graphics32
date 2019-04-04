unit GR32_ColorPicker;

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
 * Christan-W. Budde <Christian@savioursofsoul.de>
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$I GR32.inc}

uses
{$IFDEF FPC}
  LCLIntf, LCLType, LMessages, Types,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
{$ELSE}
  Windows, Messages, Types,
{$ENDIF}
  Classes, Controls, Forms, GR32, GR32_Polygons, GR32_Containers,
  GR32_ColorGradients;

type
  TScreenColorPickerForm = class(TCustomForm)
  private
    FSelectedColor: TColor32;
    FOnColorSelected: TNotifyEvent;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;

    property SelectedColor: TColor32 read FSelectedColor write FSelectedColor;
    property OnColorSelected: TNotifyEvent read FOnColorSelected write FOnColorSelected;
  published
    property OnKeyUp;
    property OnKeyPress;
    property OnKeyDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseDown;
  end;

  THueCirclePolygonFiller = class(TCustomPolygonFiller)
  private
    FCenter: TFloatPoint;
    FWebSafe: Boolean;
  protected
    function GetFillLine: TFillLineEvent; override;
    procedure FillLine(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32; CombineMode: TCombineMode); virtual;
    procedure FillLineWebSafe(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32; CombineMode: TCombineMode); virtual;
  public
    constructor Create(Center: TFloatPoint; WebSafe: Boolean = False);

    property Center: TFloatPoint read FCenter write FCenter;
    property WebSafe: Boolean read FWebSafe write FWebSafe;
  end;

  THueSaturationCirclePolygonFiller = class(THueCirclePolygonFiller)
  private
    FRadius: Single;
    FInvRadius: Single;
    FValue: Single;
    procedure SetRadius(const Value: Single);
  protected
    procedure FillLine(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32; CombineMode: TCombineMode); override;
    procedure FillLineWebSafe(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32; CombineMode: TCombineMode); override;
  public
    constructor Create(Center: TFloatPoint; Radius, Value: Single;
      WebSafe: Boolean = False);

    property Radius: Single read FRadius write SetRadius;
    property Value: Single read FValue write FValue;
  end;

  TBarycentricGradientPolygonFillerEx = class(TBarycentricGradientPolygonFiller)
  private
    FWebSafe: Boolean;
  protected
    function GetFillLine: TFillLineEvent; override;
    procedure FillLineWebSafe(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32; CombineMode: TCombineMode);
  public
    property WebSafe: Boolean read FWebSafe write FWebSafe;
  end;

  TVisualAid = set of (vaHueLine, vaSaturationCircle, vaSelection);
  TVisualAidRenderType = (vatSolid, vatInvert, vatBW);

  TAdjustCalc = procedure (X, Y: Single) of object;
  TPreserveComponent = set of (pcHue, pcSaturation, pcLuminance, pcValue);

  TVisualAidOptions = class(TPersistent)
  private
    FOwner: TPersistent;
    FRenderType: TVisualAidRenderType;
    FColor: TColor32;
    FLineWidth: Single;
    procedure SetRenderType(const Value: TVisualAidRenderType);
    procedure SetColor(const Value: TColor32);
    procedure SetLineWidth(const Value: Single);
  protected
    function GetOwner: TPersistent; override;
    procedure Changed; virtual;
  public
    constructor Create(AOwner: TPersistent); virtual;

    property Owner: TPersistent read FOwner;
  published
    property RenderType: TVisualAidRenderType read FRenderType write SetRenderType default vatInvert;
    property Color: TColor32 read FColor write SetColor;
    property LineWidth: Single read FLineWidth write SetLineWidth;
  end;

  TCustomColorPicker = class(TCustomControl)
  private
    FBuffer: TBitmap32;
    FAdjustCalc: TAdjustCalc;
    FSelectedColor: TColor32;
    FBufferValid: Boolean;
    FPreserveComponent: TPreserveComponent;
    FVisualAidOptions: TVisualAidOptions;
    FWebSafe: Boolean;
    FBorder: Boolean;
    FOnChanged: TNotifyEvent;
    procedure SetBorder(const Value: Boolean);
    procedure SetWebSafe(const Value: Boolean);
    procedure SetSelectedColor(const Value: TColor32);
{$IFDEF FPC}
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
    procedure WMGetDlgCode(var Msg: TLMessage); message LM_GETDLGCODE;
{$ELSE}
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Msg: TWmGetDlgCode); message WM_GETDLGCODE;
{$ENDIF}
  protected
    procedure Paint; override;
    procedure PaintColorPicker; virtual; abstract;
    procedure SelectedColorChanged; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Invalidate; override;
    procedure Resize; override;

    property Border: Boolean read FBorder write SetBorder default False;
    property VisualAidOptions: TVisualAidOptions read FVisualAidOptions;
    property SelectedColor: TColor32 read FSelectedColor write SetSelectedColor;
    property WebSafe: Boolean read FWebSafe write SetWebSafe;

    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TColorComponent = (ccRed, ccGreen, ccBlue, ccAlpha);

  TCustomColorPickerComponent = class(TCustomColorPicker)
  private
    FMouseDown: Boolean;
    FColorComponent: TColorComponent;
    procedure SetColorComponent(const Value: TColorComponent);
  protected
    procedure PaintColorPicker; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;

    property ColorComponent: TColorComponent read FColorComponent write SetColorComponent;
  end;

  TCustomColorPickerRGBA = class(TCustomColorPicker)
  private
    FBarHeight: Integer;
    FSpaceHeight: Integer;
    procedure SetBarHeight(const Value: Integer);
    procedure SetSpaceHeight(const Value: Integer);
    procedure PickAlpha(X, Y: Single);
    procedure PickBlue(X, Y: Single);
    procedure PickGreen(X, Y: Single);
    procedure PickRed(X, Y: Single);
  protected
    procedure PaintColorPicker; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;

    property BarHeight: Integer read FBarHeight write SetBarHeight default 24;
    property SpaceHeight: Integer read FSpaceHeight write SetSpaceHeight default 8;
  end;

  TMarkerType = (mtCross, mtCircle);

  TCustomColorPickerHS = class(TCustomColorPicker)
  private
    FHue: Single;
    FSaturation: Single;
    FMarkerType: TMarkerType;
    procedure PickHue(X, Y: Single);
    procedure SetHue(const Value: Single);
    procedure SetSaturation(const Value: Single);
    procedure SetMarkerType(const Value: TMarkerType);
  protected
    procedure PaintColorPicker; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure SelectedColorChanged; override;
  public
    constructor Create(AOwner: TComponent); override;

    property MarkerType: TMarkerType read FMarkerType write SetMarkerType;
    property Hue: Single read FHue write SetHue;
    property Saturation: Single read FSaturation write SetSaturation;
  end;

  TCustomColorPickerHSV = class(TCustomColorPicker)
  private
    FCenter: TFloatPoint;
    FHue: Single;
    FRadius: TFloat;
    FCircleSteps: Integer;
    FSaturation: Single;
    FValue: Single;
    FVisualAid: TVisualAid;
    procedure PickHue(X, Y: Single);
    procedure PickValue(X, Y: Single);
    procedure SetHue(const Value: Single);
    procedure SetSaturation(const Value: Single);
    procedure SetValue(const Value: Single);
    procedure SetVisualAid(const Value: TVisualAid);
  protected
    procedure PaintColorPicker; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure SelectedColorChanged; override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Resize; override;

    property Hue: Single read FHue write SetHue;
    property Saturation: Single read FSaturation write SetSaturation;
    property Value: Single read FValue write SetValue;
    property VisualAid: TVisualAid read FVisualAid write SetVisualAid;
  end;

  TVisualAidGTK = set of (vagHueLine, vagSelection);

  TCustomColorPickerGTK = class(TCustomColorPicker)
  private
    FCenter: TFloatPoint;
    FHue: Single;
    FRadius: TFloat;
    FInnerRadius: TFloat;
    FCircleSteps: Integer;
    FSaturation: Single;
    FValue: Single;
    FVisualAid: TVisualAidGTK;
    procedure PickHue(X, Y: Single);
    procedure PickSaturationValue(X, Y: Single);
    procedure SetHue(const Value: Single);
    procedure SetSaturation(const Value: Single);
    procedure SetValue(const Value: Single);
    procedure SetVisualAid(const Value: TVisualAidGTK);
    procedure SetRadius(const Value: TFloat);
  protected
    procedure PaintColorPicker; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure SelectedColorChanged; override;

    property Radius: TFloat read FRadius write SetRadius;
    property Center: TFloatPoint read FCenter write FCenter;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Resize; override;

    property Hue: Single read FHue write SetHue;
    property Saturation: Single read FSaturation write SetSaturation;
    property Value: Single read FValue write SetValue;
    property VisualAid: TVisualAidGTK read FVisualAid write SetVisualAid;
  end;

  TColorPickerComponent = class(TCustomColorPickerComponent)
  published
    property Align;
    property Anchors;
    property Border;
    property ColorComponent;
    property DragCursor;
    property DragKind;
    property Enabled;
{$IFDEF HasParentBackground}
    property ParentBackground;
{$ENDIF}
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property SelectedColor;
    property TabOrder;
    property TabStop;
    property VisualAidOptions;
    property WebSafe default False;

{$IFNDEF PLATFORM_INDEPENDENT}
    property OnCanResize;
{$ENDIF}
    property OnChanged;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
{$IFDEF COMPILER2005_UP}
    property OnMouseEnter;
    property OnMouseLeave;
{$ENDIF}
    property OnResize;
    property OnStartDrag;
  end;

  TColorPickerRGBA = class(TCustomColorPickerRGBA)
  published
    property Align;
    property Anchors;
    property BarHeight;
    property Border;
    property DragCursor;
    property DragKind;
    property Enabled;
{$IFDEF HasParentBackground}
    property ParentBackground;
{$ENDIF}
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property SelectedColor;
    property SpaceHeight;
    property TabOrder;
    property TabStop;
    property VisualAidOptions;
    property WebSafe default False;

{$IFNDEF PLATFORM_INDEPENDENT}
    property OnCanResize;
{$ENDIF}
    property OnChanged;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
{$IFDEF COMPILER2005_UP}
    property OnMouseEnter;
    property OnMouseLeave;
{$ENDIF}
    property OnResize;
    property OnStartDrag;
  end;

  TColorPickerHS = class(TCustomColorPickerHS)
  published
    property Align;
    property Anchors;
    property DragCursor;
    property DragKind;
    property Enabled;
    property Hue;
    property MarkerType;
{$IFDEF HasParentBackground}
    property ParentBackground;
{$ENDIF}
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property Saturation;
    property SelectedColor;
    property TabOrder;
    property TabStop;
    property WebSafe default False;

{$IFNDEF PLATFORM_INDEPENDENT}
    property OnCanResize;
{$ENDIF}
    property OnChanged;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
{$IFDEF COMPILER2005_UP}
    property OnMouseEnter;
    property OnMouseLeave;
{$ENDIF}
    property OnResize;
    property OnStartDrag;
  end;

  TColorPickerHSV = class(TCustomColorPickerHSV)
  published
    property Align;
    property Anchors;
    property Border;
    property DragCursor;
    property DragKind;
    property Enabled;
    property Hue;
{$IFDEF HasParentBackground}
    property ParentBackground;
{$ENDIF}
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property Saturation;
    property SelectedColor;
    property TabOrder;
    property TabStop;
    property Value;
    property VisualAid default [vaHueLine, vaSaturationCircle, vaSelection];
    property VisualAidOptions;
    property WebSafe default False;

{$IFNDEF PLATFORM_INDEPENDENT}
    property OnCanResize;
{$ENDIF}
    property OnChanged;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
{$IFDEF COMPILER2005_UP}
    property OnMouseEnter;
    property OnMouseLeave;
{$ENDIF}
    property OnResize;
    property OnStartDrag;
  end;

  TColorPickerGTK = class(TCustomColorPickerGTK)
  published
    property Align;
    property Anchors;
    property Border;
    property DragCursor;
    property DragKind;
    property Enabled;
    property Hue;
{$IFDEF HasParentBackground}
    property ParentBackground;
{$ENDIF}
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property Saturation;
    property SelectedColor;
    property TabOrder;
    property TabStop;
    property Value;
    property VisualAid default [vagHueLine, vagSelection];
    property VisualAidOptions;
    property WebSafe default False;

{$IFNDEF PLATFORM_INDEPENDENT}
    property OnCanResize;
{$ENDIF}
    property OnChanged;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
{$IFDEF COMPILER2005_UP}
    property OnMouseEnter;
    property OnMouseLeave;
{$ENDIF}
    property OnResize;
    property OnStartDrag;
  end;

implementation

uses
  Math, Graphics, GR32_Backends, GR32_Math, GR32_Blend, GR32_VectorUtils;

procedure RoundToWebSafe(var Color: TColor32);
begin
  with TColor32Entry(Color) do
  begin
    R := ((R + $19) div $33) * $33;
    G := ((G + $19) div $33) * $33;
    B := ((B + $19) div $33) * $33;
  end;
end;

{$IFDEF MSWINDOWS}
function GetDesktopColor(const x, y: Integer): TColor32;
var
  c: TCanvas;
begin
  c := TCanvas.Create;
  try
    c.Handle := GetWindowDC(GetDesktopWindow);
    Result := Color32(GetPixel(c.Handle, x, y));
  finally
    c.Free;
  end;
end;
{$ENDIF}


{ TVisualAidOptions }

constructor TVisualAidOptions.Create(AOwner: TPersistent);
begin
  inherited Create;

  FOwner := AOwner;
  FColor := $AF000000;
  FRenderType := vatInvert;
  FLineWidth := 2;
end;

procedure TVisualAidOptions.Changed;
begin
  if Owner is TCustomColorPicker then
    TCustomColorPicker(Owner).Invalidate;
end;

function TVisualAidOptions.GetOwner: TPersistent;
begin
  if FOwner is TPersistent then
    Result := TPersistent(FOwner)
  else
    Result := nil;
end;

procedure TVisualAidOptions.SetColor(const Value: TColor32);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    if FRenderType = vatSolid then
      Changed;
  end;
end;

procedure TVisualAidOptions.SetLineWidth(const Value: Single);
begin
  if FLineWidth <> Value then
  begin
    FLineWidth := Value;
    Changed;
  end;
end;

procedure TVisualAidOptions.SetRenderType(const Value: TVisualAidRenderType);
begin
  if FRenderType <> Value then
  begin
    FRenderType := Value;
    Changed;
  end;
end;


{ TScreenColorPickerForm }

constructor TScreenColorPickerForm.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);
  Align := alClient;
  BorderIcons := [];
  BorderStyle := bsNone;
  Caption := 'Pick a color...';
  FormStyle := fsStayOnTop;
  Position := poDefault;
  FSelectedColor := 0;
end;

procedure TScreenColorPickerForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle := WS_EX_TRANSPARENT or WS_EX_TOPMOST;
end;

procedure TScreenColorPickerForm.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then
    ModalResult := mrCancel
  else
    inherited;
end;

procedure TScreenColorPickerForm.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    {$IFDEF MSWINDOWS}
    FSelectedColor := GetDesktopColor(X, Y);
    if Assigned(FOnColorSelected) then
      FOnColorSelected(Self);
    {$ENDIF}
    ModalResult := mrOk
  end
  else
    inherited;
end;

procedure TScreenColorPickerForm.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  {$IFDEF MSWINDOWS}
  FSelectedColor := GetDesktopColor(X, Y);
  {$ENDIF}
  inherited;
end;


{ THueCirclePolygonFiller }

constructor THueCirclePolygonFiller.Create(Center: TFloatPoint;
  WebSafe: Boolean = False);
begin
  FCenter := Center;
  FWebSafe := WebSafe;

  inherited Create;
end;

procedure THueCirclePolygonFiller.FillLine(Dst: PColor32; DstX, DstY,
  Length: Integer; AlphaValues: PColor32; CombineMode: TCombineMode);
var
  X: Integer;
  H: Single;
const
  CTwoPiInv = 1 / (2 * Pi);
begin
  for X := DstX to DstX + Length - 1 do
  begin
    // calculate squared distance
    H := 0.5 + ArcTan2(DstY - FCenter.Y, X - FCenter.X) * CTwoPiInv;
    CombineMem(HSVtoRGB(H, 1, 1), Dst^, AlphaValues^);
    EMMS;
    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

procedure THueCirclePolygonFiller.FillLineWebSafe(Dst: PColor32; DstX, DstY,
  Length: Integer; AlphaValues: PColor32; CombineMode: TCombineMode);
var
  X: Integer;
  H: Single;
  Color: TColor32;
const
  CTwoPiInv = 1 / (2 * Pi);
begin
  for X := DstX to DstX + Length - 1 do
  begin
    // calculate squared distance
    H := 0.5 + ArcTan2(DstY - FCenter.Y, X - FCenter.X) * CTwoPiInv;
    Color := HSVtoRGB(H, 1, 1);
    RoundToWebSafe(Color);
    CombineMem(Color, Dst^, AlphaValues^);
    EMMS;
    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

function THueCirclePolygonFiller.GetFillLine: TFillLineEvent;
begin
  if FWebSafe then
    Result := FillLineWebSafe
  else
    Result := FillLine;
end;


{ THueSaturationCirclePolygonFiller }

constructor THueSaturationCirclePolygonFiller.Create(Center: TFloatPoint;
  Radius, Value: Single; WebSafe: Boolean = False);
begin
  FRadius := Max(1, Radius);
  FInvRadius := 1 / FRadius;
  FValue := Value;

  inherited Create(Center, WebSafe);
end;

procedure THueSaturationCirclePolygonFiller.FillLine(Dst: PColor32; DstX, DstY,
  Length: Integer; AlphaValues: PColor32; CombineMode: TCombineMode);
var
  X: Integer;
  SqrYDist, H, S: Single;
const
  CTwoPiInv = 1 / (2 * Pi);
begin
  SqrYDist := Sqr(DstY - FCenter.Y);
  for X := DstX to DstX + Length - 1 do
  begin
    // calculate squared distance
    H := 0.5 + ArcTan2(DstY - FCenter.Y, X - FCenter.X) * CTwoPiInv;
    S := Sqrt(Sqr(X - Center.X) + SqrYDist) * FInvRadius;
    if S > 1 then
      S := 1;

    CombineMem(HSVtoRGB(H, S, Value), Dst^, AlphaValues^);
    EMMS;
    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

procedure THueSaturationCirclePolygonFiller.FillLineWebSafe(Dst: PColor32; DstX, DstY,
  Length: Integer; AlphaValues: PColor32; CombineMode: TCombineMode);
var
  X: Integer;
  SqrYDist, H, S: Single;
  Color: TColor32;
const
  CTwoPiInv = 1 / (2 * Pi);
begin
  SqrYDist := Sqr(DstY - FCenter.Y);
  for X := DstX to DstX + Length - 1 do
  begin
    // calculate squared distance
    H := 0.5 + ArcTan2(DstY - FCenter.Y, X - FCenter.X) * CTwoPiInv;
    S := Sqrt(Sqr(X - Center.X) + SqrYDist) * FInvRadius;
    if S > 1 then
      S := 1;

    Color := HSVtoRGB(H, S, Value);
    RoundToWebSafe(Color);

    CombineMem(Color, Dst^, AlphaValues^);
    EMMS;
    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

procedure THueSaturationCirclePolygonFiller.SetRadius(const Value: Single);
begin
  if FRadius <> Value then
  begin
    FRadius := Value;
    FInvRadius := 1 / FRadius;
  end;
end;


{ TBarycentricGradientPolygonFillerEx }

procedure TBarycentricGradientPolygonFillerEx.FillLineWebSafe(Dst: PColor32; DstX,
  DstY, Length: Integer; AlphaValues: PColor32; CombineMode: TCombineMode);
var
  X: Integer;
  Color32: TColor32;
  Temp, DotY1, DotY2: TFloat;
  Barycentric: array [0..1] of TFloat;
  BlendMemEx: TBlendMemEx;
begin
  BlendMemEx := BLEND_MEM_EX[CombineMode]^;
  Temp := DstY - FColorPoints[2].Point.Y;
  DotY1 := FDists[0].X * Temp;
  DotY2 := FDists[1].X * Temp;
  for X := DstX to DstX + Length - 1 do
  begin
    Temp := (X - FColorPoints[2].Point.X);
    Barycentric[0] := FDists[0].Y * Temp + DotY1;
    Barycentric[1] := FDists[1].Y * Temp + DotY2;

    Color32 := Linear3PointInterpolation(FColorPoints[0].Color32,
      FColorPoints[1].Color32, FColorPoints[2].Color32,
      Barycentric[0], Barycentric[1], 1 - Barycentric[1] - Barycentric[0]);
    RoundToWebSafe(Color32);

    BlendMemEx(Color32, Dst^, AlphaValues^);
    EMMS;
    Inc(Dst);
    Inc(AlphaValues);
  end;
end;

function TBarycentricGradientPolygonFillerEx.GetFillLine: TFillLineEvent;
begin
  if FWebSafe then
    Result := FillLineWebSafe
  else
    Result := inherited GetFillLine;
end;


{ TCustomColorPicker }

constructor TCustomColorPicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csOpaque];
  FBuffer := TBitmap32.Create;
  FPreserveComponent := [];
  FSelectedColor := clSalmon32;
  FVisualAidOptions := TVisualAidOptions.Create(Self);
end;

destructor TCustomColorPicker.Destroy;
begin
  FVisualAidOptions.Free;
  FBuffer.Free;
  inherited;
end;

procedure TCustomColorPicker.Invalidate;
begin
  FBufferValid := False;
  inherited;
end;

procedure TCustomColorPicker.Paint;
begin
  if not Assigned(Parent) then
    Exit;

  if not FBufferValid then
  begin
    (FBuffer.Backend as IPaintSupport).ImageNeeded;
    PaintColorPicker;
    (FBuffer.Backend as IPaintSupport).CheckPixmap;
    FBufferValid := True;
  end;

  FBuffer.Lock;
  with Canvas do
  try
    (FBuffer.Backend as IDeviceContextSupport).DrawTo(Canvas.Handle, 0, 0);
  finally
    FBuffer.Unlock;
  end;
end;

procedure TCustomColorPicker.Resize;
begin
  inherited;
  FBuffer.SetSize(Width, Height);
  FBufferValid := False;
end;

procedure TCustomColorPicker.SelectedColorChanged;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);

  Invalidate;
end;

procedure TCustomColorPicker.SetBorder(const Value: Boolean);
begin
  if FBorder <> Value then
  begin
    FBorder := Value;
    Invalidate;
  end;
end;

procedure TCustomColorPicker.SetSelectedColor(const Value: TColor32);
begin
  if FSelectedColor <> Value then
  begin
    FSelectedColor := Value;
    SelectedColorChanged;
  end;
end;

procedure TCustomColorPicker.SetWebSafe(const Value: Boolean);
begin
  if FWebSafe <> Value then
  begin
    FWebSafe := Value;
    Invalidate;
  end;
end;

procedure TCustomColorPicker.WMEraseBkgnd(var Message: {$IFDEF FPC}TLmEraseBkgnd{$ELSE}TWmEraseBkgnd{$ENDIF});
begin
  Message.Result := 1;
end;

procedure TCustomColorPicker.WMGetDlgCode(var Msg: {$IFDEF FPC}TLMessage{$ELSE}TWmGetDlgCode{$ENDIF});
begin
  with Msg do
    Result := Result or DLGC_WANTARROWS;
end;


{ TCustomColorPickerComponent }

constructor TCustomColorPickerComponent.Create(AOwner: TComponent);
begin
  inherited;

  FVisualAidOptions.Color := clBlack32;
  FVisualAidOptions.LineWidth := 1.5;
end;

procedure TCustomColorPickerComponent.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FMouseDown := (Button = mbLeft);

  inherited;
end;

procedure TCustomColorPickerComponent.MouseMove(Shift: TShiftState; X,
  Y: Integer);
var
  Value: Single;
  Color: TColor32Entry;
begin
  if FMouseDown then
  begin
    Value := EnsureRange((X - 3) / (Width - 3), 0, 1);
    Color := TColor32Entry(SelectedColor);
    case FColorComponent of
      ccRed:
        Color.R := Round(Value * 255);
      ccGreen:
        Color.G := Round(Value * 255);
      ccBlue:
        Color.B := Round(Value * 255);
      ccAlpha:
        Color.A := Round(Value * 255);
    end;
    SelectedColor := Color.ARGB;
  end;

  inherited;
end;

procedure TCustomColorPickerComponent.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) then
    FMouseDown := False;

  inherited;
end;

procedure TCustomColorPickerComponent.PaintColorPicker;
var
  Polygon: TArrayOfFloatPoint;
  InvertFiller: TInvertPolygonFiller;

  procedure RenderPolygon;
  begin
    case FVisualAidOptions.RenderType of
      vatInvert:
        PolygonFS(FBuffer, Polygon, InvertFiller);
      vatBW:
        if Intensity(FSelectedColor) < 127 then
          PolygonFS(FBuffer, Polygon, clWhite32)
        else
          PolygonFS(FBuffer, Polygon, clBlack32);
      else
        PolygonFS(FBuffer, Polygon, FVisualAidOptions.Color);
    end;
  end;

var
  X, Y: Integer;
  ScanLine: PColor32Array;
  Value: Single;
  LeftColor, RightColor: TColor32Entry;
  OddY: Boolean;
  BorderOffset: Integer;
  GradientFiller: TLinearGradientPolygonFiller;
const
  CByteScale = 1 / 255;
  CCheckerBoardColor: array [Boolean] of TColor32 = ($FFA0A0A0, $FF5F5F5F);
begin
  FBuffer.Clear(Color32(Color));

  BorderOffset := Integer(FBorder);

  InvertFiller := TInvertPolygonFiller.Create;
  try

    LeftColor := TColor32Entry(FSelectedColor);
    RightColor := TColor32Entry(FSelectedColor);

    case FColorComponent of
      ccRed:
        begin
          Value := LeftColor.R * CByteScale;
          LeftColor.R := 0;
          RightColor.R := 255;
          LeftColor.A := 255;
          RightColor.A := 255;
        end;
      ccGreen:
        begin
          Value := LeftColor.G * CByteScale;
          LeftColor.G := 0;
          RightColor.G := 255;
          LeftColor.A := 255;
          RightColor.A := 255;
        end;
      ccBlue:
        begin
          Value := LeftColor.B * CByteScale;
          LeftColor.B := 0;
          RightColor.B := 255;
          LeftColor.A := 255;
          RightColor.A := 255;
        end;
      ccAlpha:
        begin
          Value := LeftColor.A * CByteScale;
          LeftColor.A := 0;
          RightColor.A := 255;

          for Y := 0 to Height - 1 do
          begin
            OddY := Odd(Y div 8);
            ScanLine := FBuffer.ScanLine[Y];
            for X := 3 to Width - 4 do
              ScanLine^[X] := CCheckerBoardColor[Odd(X shr 3) = OddY];
          end;
        end
      else
        Exit;
    end;

    GradientFiller := TLinearGradientPolygonFiller.Create;
    try
      GradientFiller.SimpleGradientX(3, LeftColor.ARGB,
        Width - 3, RightColor.ARGB);
      PolygonFS(FBuffer, Rectangle(FloatRect(3, 0, Width - 3, Height)), GradientFiller);
    finally
      GradientFiller.Free;
    end;

    if FBorder then
    begin
      FBuffer.FrameRectTS(3, 0, Width - 3, Height, $DF000000);
      FBuffer.RaiseRectTS(4, 0, Width - 4, Height - 1, 20);
    end;

    SetLength(Polygon, 3);
    Polygon[0] := FloatPoint(3 + Value * (Width - 6), Height - BorderOffset - 5);
    Polygon[1] := FloatPoint(Polygon[0].X - 3, Polygon[0].Y + 5);
    Polygon[2] := FloatPoint(Polygon[0].X + 3, Polygon[0].Y + 5);
    RenderPolygon;

    Polygon[0].Y := BorderOffset + 5;
    Polygon[1].Y := BorderOffset;
    Polygon[2].Y := BorderOffset;
    RenderPolygon;
  finally
    InvertFiller.Free;
  end;

  inherited;
end;

procedure TCustomColorPickerComponent.SetColorComponent(
  const Value: TColorComponent);
begin
  if FColorComponent <> Value then
  begin
    FColorComponent := Value;
    Invalidate;
  end;
end;


{ TCustomColorPickerRGBA }

constructor TCustomColorPickerRGBA.Create(AOwner: TComponent);
begin
  inherited;

  FBarHeight := 24;
  FSpaceHeight := 8;
  FVisualAidOptions.Color := clBlack32;
  FVisualAidOptions.LineWidth := 1.5;
end;

procedure TCustomColorPickerRGBA.PickRed(X, Y: Single);
var
  Value: Single;
  Color: TColor32Entry;
begin
  Value := EnsureRange((X - 3) / (Width - 3), 0, 1);
  Color := TColor32Entry(SelectedColor);
  Color.R := Round(Value * 255);
  SelectedColor := Color.ARGB;
end;

procedure TCustomColorPickerRGBA.PickGreen(X, Y: Single);
var
  Value: Single;
  Color: TColor32Entry;
begin
  Value := EnsureRange((X - 3) / (Width - 3), 0, 1);
  Color := TColor32Entry(SelectedColor);
  Color.G := Round(Value * 255);
  SelectedColor := Color.ARGB;
end;

procedure TCustomColorPickerRGBA.PickBlue(X, Y: Single);
var
  Value: Single;
  Color: TColor32Entry;
begin
  Value := EnsureRange((X - 3) / (Width - 3), 0, 1);
  Color := TColor32Entry(SelectedColor);
  Color.B := Round(Value * 255);
  SelectedColor := Color.ARGB;
end;

procedure TCustomColorPickerRGBA.PickAlpha(X, Y: Single);
var
  Value: Single;
  Color: TColor32Entry;
begin
  Value := EnsureRange((X - 3) / (Width - 3), 0, 1);
  Color := TColor32Entry(SelectedColor);
  Color.A := Round(Value * 255);
  SelectedColor := Color.ARGB;
end;

procedure TCustomColorPickerRGBA.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
begin
  if (Button = mbLeft) and (X >= 3) or (X <= Width - 3) then
  begin
    Index := Y div (FBarHeight + FSpaceHeight);
    case Index of
      0:
        FAdjustCalc := PickRed;
      1:
        FAdjustCalc := PickGreen;
      2:
        FAdjustCalc := PickBlue;
      3:
        FAdjustCalc := PickAlpha;
    end;
  end;

  if Assigned(FAdjustCalc) then
    FAdjustCalc(X, Y);

  inherited;
end;

procedure TCustomColorPickerRGBA.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if (ssLeft in Shift) and Assigned(FAdjustCalc) then
    FAdjustCalc(X, Y);
  inherited;
end;

procedure TCustomColorPickerRGBA.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FAdjustCalc := nil;

  inherited;
end;

procedure TCustomColorPickerRGBA.PaintColorPicker;
var
  Polygon: TArrayOfFloatPoint;
  InvertFiller: TInvertPolygonFiller;

  procedure RenderPolygon;
  begin
    case FVisualAidOptions.RenderType of
      vatInvert:
        PolygonFS(FBuffer, Polygon, InvertFiller);
      vatBW:
        if Intensity(FSelectedColor) < 127 then
          PolygonFS(FBuffer, Polygon, clWhite32)
        else
          PolygonFS(FBuffer, Polygon, clBlack32);
      else
        PolygonFS(FBuffer, Polygon, FVisualAidOptions.Color);
    end;
  end;

var
  X, Y, Index: Integer;
  ScanLine: PColor32Array;
  Value: Single;
  LeftColor, RightColor: TColor32Entry;
  ValueRect: TRect;
  OddY: Boolean;
  BorderOffset: Integer;
  GradientFiller: TLinearGradientPolygonFiller;
const
  CByteScale = 1 / 255;
  CCheckerBoardColor: array [Boolean] of TColor32 = ($FFA0A0A0, $FF5F5F5F);
begin
  FBuffer.Clear(Color32(Color));

  BorderOffset := Integer(FBorder);

  SetLength(Polygon, 3);
  InvertFiller := TInvertPolygonFiller.Create;
  try
    for Index := 0 to 3 do
    begin
      ValueRect := Rect(3, Index * (FBarHeight + FSpaceHeight),
        Width - 3, Index * (FBarHeight + FSpaceHeight) + FBarHeight);

      LeftColor := TColor32Entry(FSelectedColor);
      RightColor := TColor32Entry(FSelectedColor);

      case Index of
        0:
          begin
            Value := LeftColor.R * CByteScale;
            LeftColor.R := 0;
            RightColor.R := 255;
            LeftColor.A := 255;
            RightColor.A := 255;
          end;
        1:
          begin
            Value := LeftColor.G * CByteScale;
            LeftColor.G := 0;
            RightColor.G := 255;
            LeftColor.A := 255;
            RightColor.A := 255;
          end;
        2:
          begin
            Value := LeftColor.B * CByteScale;
            LeftColor.B := 0;
            RightColor.B := 255;
            LeftColor.A := 255;
            RightColor.A := 255;
          end;
        3:
          begin
            Value := LeftColor.A * CByteScale;
            LeftColor.A := 0;
            RightColor.A := 255;

            for Y := ValueRect.Top to Min(ValueRect.Bottom, Height) - 1 do
            begin
              OddY := Odd(Y div 8);
              ScanLine := FBuffer.ScanLine[Y];
              for X := ValueRect.Left to ValueRect.Right - 1 do
                ScanLine^[X] := CCheckerBoardColor[Odd(X shr 3) = OddY];
            end;
          end;
        else
          Exit;
      end;

      GradientFiller := TLinearGradientPolygonFiller.Create;
      try
        GradientFiller.SimpleGradientX(ValueRect.Left, LeftColor.ARGB,
          ValueRect.Right, RightColor.ARGB);
        PolygonFS(FBuffer, Rectangle(FloatRect(ValueRect)), GradientFiller);
      finally
        GradientFiller.Free;
      end;

      if FBorder then
      begin
        FBuffer.FrameRectTS(ValueRect, $DF000000);
        FBuffer.RaiseRectTS(ValueRect.Left + 1, ValueRect.Top + 1,
          ValueRect.Right - 1, ValueRect.Bottom - 1, 20);
      end;

      Polygon[0] := FloatPoint(3 + Value * (Width - 6), ValueRect.Bottom - BorderOffset - 5);
      Polygon[1] := FloatPoint(Polygon[0].X - 3, Polygon[0].Y + 5);
      Polygon[2] := FloatPoint(Polygon[0].X + 3, Polygon[0].Y + 5);
      RenderPolygon;

      Polygon[0].Y := ValueRect.Top + BorderOffset + 5;
      Polygon[1].Y := ValueRect.Top + BorderOffset;
      Polygon[2].Y := ValueRect.Top + BorderOffset;
      RenderPolygon;
    end;
  finally
    InvertFiller.Free;
  end;

  inherited;
end;

procedure TCustomColorPickerRGBA.SetBarHeight(const Value: Integer);
begin
  if FBarHeight <> Value then
  begin
    FBarHeight := Value;
    Invalidate;
  end;
end;

procedure TCustomColorPickerRGBA.SetSpaceHeight(const Value: Integer);
begin
  if FSpaceHeight <> Value then
  begin
    FSpaceHeight := Value;
    Invalidate;
  end;
end;


{ TCustomColorPickerHS }

constructor TCustomColorPickerHS.Create(AOwner: TComponent);
var
  Luminance: Single;
begin
  inherited;
  FVisualAidOptions.Color := clBlack32;
  FVisualAidOptions.LineWidth := 1.5;
  RGBtoHSL(FSelectedColor, FHue, FSaturation, Luminance);
end;

procedure TCustomColorPickerHS.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    PickHue(X, Y);

  inherited;
end;

procedure TCustomColorPickerHS.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if (ssLeft in Shift) then
    PickHue(X, Y);

  inherited;
end;

procedure TCustomColorPickerHS.PaintColorPicker;
var
  X, Y: Integer;
  Saturation, InvWidth, InvHeight: Single;
  Line: PColor32Array;
  Pos: TFloatPoint;
  VectorData: TArrayOfArrayOfFloatPoint;
  InvertFiller: TInvertPolygonFiller;
begin
  InvWidth := 1 / FBuffer.Width;
  InvHeight := 1 / FBuffer.Height;

  if FWebSafe then
    for Y := 0 to FBuffer.Height - 1 do
    begin
      Line := FBuffer.ScanLine[Y];
      Saturation := 1 - Y * InvHeight;
      for X := 0 to FBuffer.Width - 1 do
      begin
        Line^[X] := HSLtoRGB(X * InvWidth, Saturation, 0.5);
        RoundToWebSafe(Line^[X]);
      end;
    end
  else
    for Y := 0 to FBuffer.Height - 1 do
    begin
      Line := FBuffer.ScanLine[Y];
      Saturation := 1 - Y * InvHeight;
      for X := 0 to FBuffer.Width - 1 do
        Line^[X] := HSLtoRGB(X * InvWidth, Saturation, 0.5);
    end;

  Pos.X := Round(FHue * FBuffer.Width);
  Pos.Y := Round((1 - FSaturation) * FBuffer.Height);
  case FMarkerType of
    mtCross:
      begin
        SetLength(VectorData, 4);
        VectorData[0] := HorzLine(Pos.X - 5, Pos.Y, Pos.X - 2);
        VectorData[1] := HorzLine(Pos.X + 2, Pos.Y, Pos.X + 5);
        VectorData[2] := VertLine(Pos.X, Pos.Y - 5, Pos.Y - 2);
        VectorData[3] := VertLine(Pos.X, Pos.Y + 2, Pos.Y + 5);
        case FVisualAidOptions.RenderType of
          vatSolid:
            PolyPolylineFS(FBuffer, VectorData, FVisualAidOptions.Color, False, FVisualAidOptions.LineWidth);
          vatInvert:
            begin
              InvertFiller := TInvertPolygonFiller.Create;
              try
                PolyPolylineFS(FBuffer, VectorData, InvertFiller, False, FVisualAidOptions.LineWidth)
              finally
                InvertFiller.Free;
              end;
            end;
          vatBW:
            PolyPolylineFS(FBuffer, VectorData, FVisualAidOptions.Color, False, FVisualAidOptions.LineWidth);
        end;
      end;
    mtCircle:
      begin
        SetLength(VectorData, 1);
        VectorData[0] := Circle(Pos, 4, 12);
        PolygonFS(FBuffer, VectorData[0], FSelectedColor);

        case FVisualAidOptions.RenderType of
          vatSolid:
            PolylineFS(FBuffer, VectorData[0], FVisualAidOptions.Color, True, FVisualAidOptions.LineWidth);
          vatInvert:
            begin
              InvertFiller := TInvertPolygonFiller.Create;
              try
                PolylineFS(FBuffer, VectorData[0], InvertFiller, True, 1.5)
              finally
                InvertFiller.Free;
              end;
            end;
          vatBW:
            PolylineFS(FBuffer, VectorData[0], FVisualAidOptions.Color, True, 1.5);
      end;
    end;
  end;
end;

procedure TCustomColorPickerHS.PickHue(X, Y: Single);
begin
  FHue := EnsureRange(X / FBuffer.Width, 0, 1);
  FSaturation := EnsureRange(1 - Y / FBuffer.Height, 0, 1);
  SelectedColor := SetAlpha(HSLtoRGB(FHue, FSaturation, 0.5), SelectedColor shr 24);
end;

procedure TCustomColorPickerHS.SelectedColorChanged;
var
  H, S, L: Single;
begin
  RGBtoHSL(FSelectedColor, H, S, L);
  if not (pcHue in FPreserveComponent) then
    FHue := H;
  if not (pcSaturation in FPreserveComponent) then
    FSaturation := S;

  FPreserveComponent := [];

  inherited;
end;

procedure TCustomColorPickerHS.SetHue(const Value: Single);
begin
  if FHue <> Value then
  begin
    FHue := Value;
    FPreserveComponent := FPreserveComponent + [pcHue];
    SelectedColor := SetAlpha(HSLtoRGB(FHue, FSaturation, 1), SelectedColor shr 24);
  end;
end;

procedure TCustomColorPickerHS.SetSaturation(const Value: Single);
begin
  if FSaturation <> Value then
  begin
    FSaturation := Value;
    FPreserveComponent := FPreserveComponent + [pcSaturation];
    SelectedColor := SetAlpha(HSLtoRGB(FHue, FSaturation, 1), SelectedColor shr 24);
  end;
end;

procedure TCustomColorPickerHS.SetMarkerType(const Value: TMarkerType);
begin
  if FMarkerType <> Value then
  begin
    FMarkerType := Value;
    Invalidate;
  end;
end;


{ TCustomColorPickerHSV }

constructor TCustomColorPickerHSV.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FVisualAid := [vaHueLine, vaSaturationCircle, vaSelection];
  FVisualAidOptions.LineWidth := 1.5;
  RGBToHSV(FSelectedColor, FHue, FSaturation, FValue);

  { Setting a initial size here will cause the control to crash under LCL }
{$IFNDEF FPC}
  Height := 192;
  Width := 256;
{$ENDIF}
end;

procedure TCustomColorPickerHSV.PaintColorPicker;
var
  Polygon: TArrayOfFloatPoint;
  ValueRect: TRect;
  GradientFiller: TLinearGradientPolygonFiller;
  HueSaturationFiller: THueSaturationCirclePolygonFiller;
  InvertFiller: TInvertPolygonFiller;
  LineWidth: Single;
begin
  FBuffer.Clear(Color32(Color));

  Polygon := Circle(FCenter, FRadius, FCircleSteps);
  HueSaturationFiller := THueSaturationCirclePolygonFiller.Create(FCenter,
    FRadius, FValue, FWebSafe);
  try
    PolygonFS(FBuffer, Polygon, HueSaturationFiller);
  finally
    HueSaturationFiller.Free;
  end;

  if FBorder then
    PolylineFS(FBuffer, Polygon, clBlack32, True, 1);

  LineWidth := FVisualAidOptions.LineWidth;

  InvertFiller := TInvertPolygonFiller.Create;
  try
    if vaSaturationCircle in FVisualAid then
    begin
      Polygon := Circle(FCenter, FSaturation * FRadius, -1);
      case FVisualAidOptions.RenderType of
        vatInvert:
          PolylineFS(FBuffer, Polygon, InvertFiller, True, LineWidth);
        vatBW:
          if Intensity(FSelectedColor) < 127 then
            PolylineFS(FBuffer, Polygon, clWhite32, True, LineWidth)
          else
            PolylineFS(FBuffer, Polygon, clBlack32, True, LineWidth);
        else
          PolylineFS(FBuffer, Polygon, FVisualAidOptions.Color, True, LineWidth);
      end;
    end;

    if vaHueLine in FVisualAid then
    begin
      SetLength(Polygon, 2);
      Polygon[0] := FCenter;
      Polygon[1] := FloatPoint(
        FCenter.X - FRadius * Cos(2 * Pi * FHue),
        FCenter.Y - FRadius * Sin(2 * Pi * FHue));

      case FVisualAidOptions.RenderType of
        vatInvert:
          PolylineFS(FBuffer, Polygon, InvertFiller, False, LineWidth);
        vatBW:
          if Intensity(FSelectedColor) < 127 then
            PolylineFS(FBuffer, Polygon, clWhite32, False, LineWidth)
          else
            PolylineFS(FBuffer, Polygon, clBlack32, False, LineWidth);
        else
          PolylineFS(FBuffer, Polygon, FVisualAidOptions.Color, False, LineWidth);
      end;
    end;

    if vaSelection in FVisualAid then
    begin
      Polygon := Circle(
        FCenter.X - FSaturation * FRadius * Cos(2 * Pi * FHue),
        FCenter.Y - FSaturation * FRadius * Sin(2 * Pi * FHue), 4, 8);
      PolygonFS(FBuffer, Polygon, FSelectedColor);

      case FVisualAidOptions.RenderType of
        vatInvert:
          PolylineFS(FBuffer, Polygon, InvertFiller, True, LineWidth);
        vatBW:
          if Intensity(FSelectedColor) < 127 then
            PolylineFS(FBuffer, Polygon, clWhite32, True, LineWidth)
          else
            PolylineFS(FBuffer, Polygon, clBlack32, True, LineWidth);
        else
          PolylineFS(FBuffer, Polygon, FVisualAidOptions.Color, True, LineWidth);
      end;
    end;

    ValueRect := Rect(Width - 24, 8, Width - 8, Height - 8);
    Polygon := Rectangle(FloatRect(ValueRect));

    GradientFiller := TLinearGradientPolygonFiller.Create;
    try
      GradientFiller.SimpleGradientY(ValueRect.Top, clWhite32,
        ValueRect.Bottom, clBlack32);
      PolygonFS(FBuffer, Polygon, GradientFiller);
    finally
      GradientFiller.Free;
    end;

    SetLength(Polygon, 3);
    Polygon[0] := FloatPoint(Width - 8, 8 + (1 - FValue) * (Height - 16));
    Polygon[1] := FloatPoint(Polygon[0].X + 7, Polygon[0].Y - 4);
    Polygon[2] := FloatPoint(Polygon[0].X + 7, Polygon[0].Y + 4);
    case FVisualAidOptions.RenderType of
      vatInvert:
        PolygonFS(FBuffer, Polygon, InvertFiller);
      vatBW:
        if Intensity(FSelectedColor) < 127 then
          PolygonFS(FBuffer, Polygon, clWhite32)
        else
          PolygonFS(FBuffer, Polygon, clBlack32);
      else
        PolygonFS(FBuffer, Polygon, FVisualAidOptions.Color);
    end;

    if FBorder then
    begin
      FBuffer.FrameRectTS(ValueRect, $DF000000);
      FBuffer.RaiseRectTS(ValueRect.Left + 1, ValueRect.Top + 1,
        ValueRect.Right - 1, ValueRect.Bottom - 1, 20);
    end;
  finally
    InvertFiller.Free;
  end;

  inherited;
end;

procedure TCustomColorPickerHSV.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if Button = mbLeft then
  begin
    if X > Width - 28 then
      FAdjustCalc := PickValue
    else
      FAdjustCalc := PickHue;
  end;

  if Assigned(FAdjustCalc) then
    FAdjustCalc(X, Y);

  inherited;
end;

procedure TCustomColorPickerHSV.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  FAdjustCalc := nil;
  inherited;
end;

procedure TCustomColorPickerHSV.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if (ssLeft in Shift) and Assigned(FAdjustCalc) then
    FAdjustCalc(X, Y);
  inherited;
end;

procedure TCustomColorPickerHSV.Resize;
begin
  inherited;

  if Height < Width then
  begin
    FRadius := Min(0.5 * Width - 1 - 16, 0.5 * Height - 1);
    FCircleSteps := CalculateCircleSteps(FRadius);
    FCenter := FloatPoint(0.5 * Width - 16, 0.5 * Height);
  end
  else
  begin
    FRadius := Min(0.5 * Width - 1, 0.5 * Height - 1 - 16);
    FCircleSteps := CalculateCircleSteps(FRadius);
    FCenter := FloatPoint(0.5 * Width, 0.5 * Height - 16);
  end;
end;

procedure TCustomColorPickerHSV.PickHue(X, Y: Single);
const
  CTwoPiInv = 1 / (2 * Pi);
begin
  FHue := 0.5 + ArcTan2(Y - FCenter.Y, X - FCenter.X) * CTwoPiInv;
  FSaturation := Sqrt(Sqr(Y - FCenter.Y) + Sqr(X - FCenter.X)) / FRadius;
  if FSaturation > 1 then
    FSaturation := 1;

  FPreserveComponent := FPreserveComponent + [pcSaturation, pcHue];
  SelectedColor := SetAlpha(HSVtoRGB(FHue, FSaturation, FValue), SelectedColor shr 24);
end;

procedure TCustomColorPickerHSV.PickValue(X, Y: Single);
begin
  Value := 1 - EnsureRange((Y - 8) / (Height - 16), 0, 1);
end;

procedure TCustomColorPickerHSV.SetHue(const Value: Single);
begin
  if FHue <> Value then
  begin
    FHue := Value;
    FPreserveComponent := FPreserveComponent + [pcHue];
    SelectedColor := SetAlpha(HSVtoRGB(FHue, FSaturation, FValue), SelectedColor shr 24);
  end;
end;

procedure TCustomColorPickerHSV.SetSaturation(const Value: Single);
begin
  if FSaturation <> Value then
  begin
    FSaturation := Value;
    FPreserveComponent := FPreserveComponent + [pcSaturation];
    SelectedColor := SetAlpha(HSVtoRGB(FHue, FSaturation, FValue), SelectedColor shr 24);
  end;
end;

procedure TCustomColorPickerHSV.SelectedColorChanged;
var
  H, S, V: Single;
begin
  RGBtoHSV(FSelectedColor, H, S, V);
  if not (pcHue in FPreserveComponent) then
    FHue := H;
  if not (pcSaturation in FPreserveComponent) then
    FSaturation := S;
  if not (pcValue in FPreserveComponent) then
    FValue := V;

  FPreserveComponent := [];

  inherited;
end;

procedure TCustomColorPickerHSV.SetValue(const Value: Single);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    FPreserveComponent := FPreserveComponent + [pcValue];
    SelectedColor := SetAlpha(HSVtoRGB(FHue, FSaturation, FValue), SelectedColor shr 24);
  end;
end;

procedure TCustomColorPickerHSV.SetVisualAid(const Value: TVisualAid);
begin
  if FVisualAid <> Value then
  begin
    FVisualAid := Value;
    Invalidate;
  end;
end;


{ TCustomColorPickerGTK }

constructor TCustomColorPickerGTK.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FVisualAid := [vagHueLine, vagSelection];
  FVisualAidOptions.RenderType := vatBW;
  FVisualAidOptions.LineWidth := 2;
  RGBToHSV(FSelectedColor, FHue, FSaturation, FValue);

  { Setting a initial size here will cause the control to crash under LCL }
{$IFNDEF FPC}
  Height := 192;
  Width := 192;
{$ENDIF}
end;

procedure TCustomColorPickerGTK.PaintColorPicker;
var
  Polygon: TArrayOfFloatPoint;
  HueBand: TArrayOfArrayOfFloatPoint;
  GradientFiller: TBarycentricGradientPolygonFillerEx;
  HueFiller: THueCirclePolygonFiller;
  InvertFiller: TInvertPolygonFiller;
  Pos: TFloatPoint;
  HalfInnerRadius: Single;
  LineWidth: Single;
const
  CY = 1.7320508075688772935274463415059;
begin
  FBuffer.Clear(Color32(Color));

  Polygon := Circle(FCenter, 0.5 * (FRadius + FInnerRadius), FCircleSteps);
  HueBand := BuildPolyPolyline(PolyPolygon(Polygon), True, FRadius - FInnerRadius);
  HueFiller := THueCirclePolygonFiller.Create(FCenter, FWebSafe);
  try
    PolyPolygonFS(FBuffer, HueBand, HueFiller);
  finally
    HueFiller.Free;
  end;

  LineWidth := FVisualAidOptions.LineWidth;

  if vagHueLine in FVisualAid then
  begin
    SetLength(Polygon, 2);
    Polygon[0] := FloatPoint(
      FCenter.X - FInnerRadius * Cos(2 * Pi * FHue),
      FCenter.Y - FInnerRadius * Sin(2 * Pi * FHue));
    Polygon[1] := FloatPoint(
      FCenter.X - FRadius * Cos(2 * Pi * FHue),
      FCenter.Y - FRadius * Sin(2 * Pi * FHue));

    case FVisualAidOptions.RenderType of
      vatSolid:
        PolylineFS(FBuffer, Polygon, FVisualAidOptions.Color, False, LineWidth);
      vatInvert:
        begin
          InvertFiller := TInvertPolygonFiller.Create;
          try
            PolylineFS(FBuffer, Polygon, InvertFiller, False, LineWidth);
          finally
            InvertFiller.Free;
          end;
        end;
      vatBW:
        if Intensity(HSVtoRGB(FHue, 1, 1)) < 127 then
          PolylineFS(FBuffer, Polygon, $F0FFFFFF, True, LineWidth)
        else
          PolylineFS(FBuffer, Polygon, $F0000000, True, LineWidth)
    end;
  end;

  GR32_Math.SinCos(2 * Pi * FHue, Pos.Y, Pos.X);
  SetLength(Polygon, 3);
  Polygon[0] := FloatPoint(
    FCenter.X - FInnerRadius * Pos.X,
    FCenter.Y - FInnerRadius * Pos.Y);
  HalfInnerRadius := 0.5 * FInnerRadius;
  Pos := FloatPoint(Pos.X + CY * Pos.Y, Pos.X * CY - Pos.Y);
  Polygon[1] := FloatPoint(
    FCenter.X + HalfInnerRadius * Pos.X,
    FCenter.Y - HalfInnerRadius * Pos.Y);
  HalfInnerRadius := 0.5 * HalfInnerRadius;
  Pos := FloatPoint(Pos.X - CY * Pos.Y, Pos.Y + Pos.X * CY);
  Polygon[2] := FloatPoint(
    FCenter.X - HalfInnerRadius * Pos.X,
    FCenter.Y + HalfInnerRadius * Pos.Y);

  GradientFiller := TBarycentricGradientPolygonFillerEx.Create;
  try
    GradientFiller.SetPoints(Polygon);
    GradientFiller.Color[0] := HSVtoRGB(Hue, 1, 1);
    GradientFiller.Color[1] := clWhite32;
    GradientFiller.Color[2] := clBlack32;
    GradientFiller.WebSafe := FWebSafe;
    PolygonFS(FBuffer, Polygon, GradientFiller);
  finally
    GradientFiller.Free;
  end;

  if FBorder then
  begin
    PolyPolygonFS(FBuffer, BuildPolyPolyline(HueBand, True, 1), clBlack32);
    PolylineFS(FBuffer, Polygon, clBlack32, True, 1);
  end;

  if vagSelection in FVisualAid then
  begin
    Polygon := Circle(
      Polygon[2].X + FValue * (Polygon[1].X + FSaturation * (Polygon[0].X - Polygon[1].X) - Polygon[2].X),
      Polygon[2].Y + FValue * (Polygon[1].Y + FSaturation * (Polygon[0].Y - Polygon[1].Y) - Polygon[2].Y),
      4, 12);

    PolygonFS(FBuffer, Polygon, FSelectedColor);

    case FVisualAidOptions.RenderType of
      vatSolid:
        PolylineFS(FBuffer, Polygon, FVisualAidOptions.Color, True, LineWidth);
      vatInvert:
        begin
          InvertFiller := TInvertPolygonFiller.Create;
          try
            PolylineFS(FBuffer, Polygon, InvertFiller, True, LineWidth);
          finally
            InvertFiller.Free;
          end;
        end;
      vatBW:
        if Intensity(FSelectedColor) < 127 then
          PolylineFS(FBuffer, Polygon, clWhite32, True, LineWidth)
        else
          PolylineFS(FBuffer, Polygon, clBlack32, True, LineWidth)
    end
  end;

  inherited;
end;

procedure TCustomColorPickerGTK.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if Button = mbLeft then
  begin
    if Sqrt(Sqr(X - FCenter.X) + Sqr(Y - FCenter.Y)) > FInnerRadius then
      FAdjustCalc := PickHue
    else
      FAdjustCalc := PickSaturationValue;
  end;

  if Assigned(FAdjustCalc) then
    FAdjustCalc(X, Y);

  inherited;
end;

procedure TCustomColorPickerGTK.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  FAdjustCalc := nil;
  inherited;
end;

procedure TCustomColorPickerGTK.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if (ssLeft in Shift) and Assigned(FAdjustCalc) then
    FAdjustCalc(X, Y);
  inherited;
end;

procedure TCustomColorPickerGTK.Resize;
begin
  inherited;

  Radius := Min(0.5 * Width - 1, 0.5 * Height - 1);
  Center := FloatPoint(0.5 * Width, 0.5 * Height);
end;

procedure TCustomColorPickerGTK.PickHue(X, Y: Single);
const
  CTwoPiInv = 1 / (2 * Pi);
begin
  Hue := 0.5 + ArcTan2(Y - FCenter.Y, X - FCenter.X) * CTwoPiInv;
  FPreserveComponent := FPreserveComponent + [pcHue];
end;

procedure TCustomColorPickerGTK.PickSaturationValue(X, Y: Single);
var
  Pos: TFloatPoint;
const
  CY = 1.7320508075688772935274463415059;
begin
  with TBarycentricGradientSampler.Create do
  try
    GR32_Math.SinCos(2 * Pi * FHue, Pos.Y, Pos.X);
    Point[0] := FloatPoint(
      FCenter.X - FInnerRadius * Pos.X,
      FCenter.Y - FInnerRadius * Pos.Y);
    Pos := FloatPoint(-0.5 * (Pos.X + CY * Pos.Y), 0.5 * (Pos.X * CY - Pos.Y));
    Point[1] := FloatPoint(
      FCenter.X - FInnerRadius * Pos.X,
      FCenter.Y - FInnerRadius * Pos.Y);
    Pos := FloatPoint(-0.5 * (Pos.X + CY * Pos.Y), 0.5 * (Pos.X * CY - Pos.Y));
    Point[2] := FloatPoint(
      FCenter.X - FInnerRadius * Pos.X,
      FCenter.Y - FInnerRadius * Pos.Y);
    Color[0] := HSVtoRGB(Hue, 1, 1);
    Color[1] := clWhite32;
    Color[2] := clBlack32;

    PrepareSampling;
    FPreserveComponent := FPreserveComponent + [pcHue];
    SelectedColor := SetAlpha(GetSampleFloatInTriangle(X, Y), SelectedColor shr 24);
  finally
    Free;
  end;
end;

procedure TCustomColorPickerGTK.SetHue(const Value: Single);
begin
  if FHue <> Value then
  begin
    FHue := Value;
    FPreserveComponent := FPreserveComponent + [pcHue];
    SelectedColor := SetAlpha(HSVtoRGB(FHue, FSaturation, FValue), SelectedColor shr 24);
  end;
end;

procedure TCustomColorPickerGTK.SetRadius(const Value: TFloat);
begin
  if FRadius <> Value then
  begin
    FRadius := Value;
    FInnerRadius := 0.8 * FRadius;
    FCircleSteps := CalculateCircleSteps(FRadius);
  end;
end;

procedure TCustomColorPickerGTK.SetSaturation(const Value: Single);
begin
  if FSaturation <> Value then
  begin
    FSaturation := Value;
    FPreserveComponent := FPreserveComponent + [pcSaturation];
    SelectedColor := SetAlpha(HSVtoRGB(FHue, FSaturation, FValue), SelectedColor shr 24);
  end;
end;

procedure TCustomColorPickerGTK.SelectedColorChanged;
var
  H, S, V: Single;
begin
  RGBtoHSV(FSelectedColor, H, S, V);
  if not (pcHue in FPreserveComponent) then
    FHue := H;
  if not (pcSaturation in FPreserveComponent) then
    FSaturation := S;
  if not (pcValue in FPreserveComponent) then
    FValue := V;

  FPreserveComponent := [];

  inherited;
end;

procedure TCustomColorPickerGTK.SetValue(const Value: Single);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    FPreserveComponent := FPreserveComponent + [pcValue];
    SelectedColor := SetAlpha(HSVtoRGB(FHue, FSaturation, FValue), SelectedColor shr 24);
  end;
end;

procedure TCustomColorPickerGTK.SetVisualAid(const Value: TVisualAidGTK);
begin
  if FVisualAid <> Value then
  begin
    FVisualAid := Value;
    Invalidate;
  end;
end;

end.
