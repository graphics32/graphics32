unit GR32_ColorPicker;

interface

uses
  Classes, Controls, GR32, GR32_Image, GR32_Polygons, GR32_Containers,
  GR32_ColorGradients;

type
  { THueCirclePolygonFiller }
  THueCirclePolygonFiller = class(TCustomPolygonFiller)
  private
    FCenter: TFloatPoint;
    FWebSafe: Boolean;
  protected
    function GetFillLine: TFillLineEvent; override;
    procedure FillLine(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32); virtual;
    procedure FillLineWebSafe(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32); virtual;
  public
    constructor Create(Center: TFloatPoint; WebSafe: Boolean = False);

    property Center: TFloatPoint read FCenter write FCenter;
    property WebSafe: Boolean read FWebSafe write FWebSafe;
  end;

  { THueSaturationCirclePolygonFiller }
  THueSaturationCirclePolygonFiller = class(THueCirclePolygonFiller)
  private
    FRadius: Single;
    FInvRadius: Single;
    FValue: Single;
    procedure SetRadius(const Value: Single);
  protected
    procedure FillLine(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32); override;
    procedure FillLineWebSafe(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32); override;
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
      AlphaValues: PColor32);
  public
    property WebSafe: Boolean read FWebSafe write FWebSafe;
  end;

  TVisualAidType = (vatSolid, vatInvert);

  { TCustomColorPicker }
  TCustomColorPicker = class(TCustomControl)
  type
    TAdjustCalc = procedure (X, Y: Single) of object;
  private
    FBuffer: TBitmap32;
    FAdjustCalc: TAdjustCalc;
    FSelectedColor: TColor32;
    FBufferValid: Boolean;
    FWebSafe: Boolean;
    FVisualAidType: TVisualAidType;
    FVisualAidColor: TColor32;
    procedure SetWebSafe(const Value: Boolean);
    procedure SetSelectedColor(const Value: TColor32);
    procedure SetVisualAidType(const Value: TVisualAidType);
    procedure SetVisualAidColor(const Value: TColor32);
  protected
    procedure SelectedColorChanged; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Invalidate; override;

    property SelectedColor: TColor32 read FSelectedColor write SetSelectedColor;
    property VisualAidType: TVisualAidType read FVisualAidType write SetVisualAidType;
    property VisualAidColor: TColor32 read FVisualAidColor write SetVisualAidColor;
    property WebSafe: Boolean read FWebSafe write SetWebSafe;
  end;


  { TCustomColorPickerHSV }
  TCustomColorPickerHSV = class(TCustomColorPicker)
  type
    TAdjustCalc = procedure (X, Y: Single) of object;
    TVisualAid = set of (vaHueLine, vaSaturationCircle, vaSelection);
  private
    FCenter: TFloatPoint;
    FHue: Single;
    FRadius: TFloat;
    FCircleSteps: Integer;
    FSaturation: Single;
    FValue: Single;
    FVisualAid: TVisualAid;
    procedure PaintColorPicker;
    procedure PickHue(X, Y: Single);
    procedure PickValue(X, Y: Single);
    procedure SetHue(const Value: Single);
    procedure SetSaturation(const Value: Single);
    procedure SetValue(const Value: Single);
    procedure SetVisualAid(const Value: TVisualAid);
  protected
    procedure Paint; override;
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

  { TCustomColorPickerGTK }
  TCustomColorPickerGTK = class(TCustomColorPicker)
  type
    TAdjustCalc = procedure (X, Y: Single) of object;
    TVisualAidGTK = set of (vagHueLine, vagSelection);
  private
    FCenter: TFloatPoint;
    FHue: Single;
    FRadius: TFloat;
    FCircleSteps: Integer;
    FSaturation: Single;
    FValue: Single;
    FVisualAid: TVisualAidGTK;
    procedure PaintColorPicker;
    procedure PickHue(X, Y: Single);
    procedure PickSaturationValue(X, Y: Single);
    procedure SetHue(const Value: Single);
    procedure SetSaturation(const Value: Single);
    procedure SetValue(const Value: Single);
    procedure SetVisualAid(const Value: TVisualAidGTK);
  protected
    procedure Paint; override;
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
    property VisualAid: TVisualAidGTK read FVisualAid write SetVisualAid;
  end;

  { TColorPickerHSV }
  TColorPickerHSV = class(TCustomColorPickerHSV)
  published
    property Align;
    property Anchors;
    property DragCursor;
    property DragKind;
    property Enabled;
    property Hue;
    property ParentBackground;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property Saturation;
    property SelectedColor;
    property TabOrder;
    property TabStop;
    property Value;
    property VisualAid default [vaHueLine, vaSaturationCircle, vaSelection];
    property WebSafe default False;
  end;

  { TColorPickerGTK }
  TColorPickerGTK = class(TCustomColorPickerGTK)
  published
    property Align;
    property Anchors;
    property DragCursor;
    property DragKind;
    property Enabled;
    property Hue;
    property ParentBackground;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property Saturation;
    property SelectedColor;
    property TabOrder;
    property TabStop;
    property Value;
    property VisualAid default [vagHueLine, vagSelection];
    property WebSafe default False;
  end;

implementation

uses
  Math, GR32_Backends, GR32_Math, GR32_Blend, GR32_VectorUtils;

procedure RoundToWebSafe(var Color: TColor32);
begin
  with TColor32Entry(Color) do
  begin
    R := ((R + $19) div $33) * $33;
    G := ((G + $19) div $33) * $33;
    B := ((B + $19) div $33) * $33;
  end;
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
  Length: Integer; AlphaValues: PColor32);
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
  Length: Integer; AlphaValues: PColor32);
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
  Length: Integer; AlphaValues: PColor32);
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
  Length: Integer; AlphaValues: PColor32);
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
  DstY, Length: Integer; AlphaValues: PColor32);
var
  X: Integer;
  Color32: TColor32;
  Temp, DotY1, DotY2: TFloat;
  Barycentric: array [0..1] of TFloat;
begin
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
    Result := inherited;
end;


{ TCustomColorPicker }

constructor TCustomColorPicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csOpaque];
  FBuffer := TBitmap32.Create;
  FSelectedColor := clSalmon32;
  FVisualAidColor := $AF000000;
end;

destructor TCustomColorPicker.Destroy;
begin
  FBuffer.Free;
  inherited;
end;

procedure TCustomColorPicker.Invalidate;
begin
  FBufferValid := False;
  inherited;
end;

procedure TCustomColorPicker.SelectedColorChanged;
begin
  Invalidate;
end;

procedure TCustomColorPicker.SetSelectedColor(const Value: TColor32);
begin
  if FSelectedColor <> Value then
  begin
    FSelectedColor := Value;
    SelectedColorChanged;
  end;
end;

procedure TCustomColorPicker.SetVisualAidColor(const Value: TColor32);
begin
  if FVisualAidColor <> Value then
  begin
    FVisualAidColor := Value;
    if FVisualAidType = vatSolid then
      Invalidate;
  end;
end;

procedure TCustomColorPicker.SetVisualAidType(const Value: TVisualAidType);
begin
  if FVisualAidType <> Value then
  begin
    FVisualAidType := Value;
    Invalidate;
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


{ TCustomColorPickerHSV }

constructor TCustomColorPickerHSV.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FVisualAid := [vaHueLine, vaSaturationCircle, vaSelection];
  FVisualAidType := vatInvert;
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
  ValueRect: TFloatRect;
  GradientFiller: TLinearGradientPolygonFiller;
  HueSaturationFiller: THueSaturationCirclePolygonFiller;
  InvertFiller: TInvertPolygonFiller;
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

  InvertFiller := TInvertPolygonFiller.Create;
  try
    if vaSaturationCircle in FVisualAid then
    begin
      Polygon := Circle(FCenter, FSaturation * FRadius, -1);
      if FVisualAidType = vatInvert then
        PolylineFS(FBuffer, Polygon, InvertFiller, True, 1.5)
      else
        PolylineFS(FBuffer, Polygon, FVisualAidColor, True, 1.5);
    end;

    if vaHueLine in FVisualAid then
    begin
      SetLength(Polygon, 2);
      Polygon[0] := FCenter;
      Polygon[1] := FloatPoint(
        FCenter.X - FRadius * Cos(2 * Pi * FHue),
        FCenter.Y - FRadius * Sin(2 * Pi * FHue));

      if FVisualAidType = vatInvert then
        PolylineFS(FBuffer, Polygon, InvertFiller, False, 1.5)
      else
        PolylineFS(FBuffer, Polygon, FVisualAidColor, False, 1.5);
    end;

    if vaSelection in FVisualAid then
    begin
      Polygon := Circle(
        FCenter.X - FSaturation * FRadius * Cos(2 * Pi * FHue),
        FCenter.Y - FSaturation * FRadius * Sin(2 * Pi * FHue), 4, 8);
      PolygonFS(FBuffer, Polygon, FSelectedColor);

      if FVisualAidType = vatInvert then
        PolylineFS(FBuffer, Polygon, InvertFiller, True, 1.5)
      else
        PolylineFS(FBuffer, Polygon, FVisualAidColor, True, 1.5);
    end;

    ValueRect := FloatRect(Width - 24, 8, Width - 8, Height - 8);
    Polygon := Rectangle(ValueRect);

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
    if FVisualAidType = vatInvert then
      PolygonFS(FBuffer, Polygon, InvertFiller)
    else
      PolygonFS(FBuffer, Polygon, VisualAidColor);
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

  FBuffer.SetSize(Width, Height);
  FBufferValid := False;

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

procedure TCustomColorPickerHSV.Paint;
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

procedure TCustomColorPickerHSV.PickHue(X, Y: Single);
const
  CTwoPiInv = 1 / (2 * Pi);
begin
  FHue := 0.5 + ArcTan2(Y - FCenter.Y, X - FCenter.X) * CTwoPiInv;
  FSaturation := Sqrt(Sqr(Y - FCenter.Y) + Sqr(X - FCenter.X)) / FRadius;
  if FSaturation > 1 then
    FSaturation := 1;

  SelectedColor := HSVtoRGB(FHue, FSaturation, FValue);
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
    FSelectedColor := HSVtoRGB(FHue, FSaturation, FValue);
    Invalidate;
  end;
end;

procedure TCustomColorPickerHSV.SetSaturation(const Value: Single);
begin
  if FSaturation <> Value then
  begin
    FSaturation := Value;
    FSelectedColor := HSVtoRGB(FHue, FSaturation, FValue);
    Invalidate;
  end;
end;

procedure TCustomColorPickerHSV.SelectedColorChanged;
begin
  RGBToHSV(FSelectedColor, FHue, FSaturation, FValue);
  inherited;
end;

procedure TCustomColorPickerHSV.SetValue(const Value: Single);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    FSelectedColor := HSVtoRGB(FHue, FSaturation, FValue);
    Invalidate;
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

  ControlStyle := ControlStyle + [csOpaque];
  FBuffer := TBitmap32.Create;

  FSelectedColor := clSalmon32;
  FVisualAid := [vagHueLine, vagSelection];
  FVisualAidType := vatSolid;
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
  GradientFiller: TBarycentricGradientPolygonFillerEx;
  HueFiller: THueCirclePolygonFiller;
  InvertFiller: TInvertPolygonFiller;
  Pos: TFloatPoint;
const
  CY = 1.7320508075688772935274463415059;
begin
  FBuffer.Clear(Color32(Color));

  Polygon := Circle(FCenter, FRadius - 8, FCircleSteps);
  HueFiller := THueCirclePolygonFiller.Create(FCenter, FWebSafe);
  try
    PolyPolygonFS(FBuffer, BuildPolyPolyline(PolyPolygon(Polygon), True, 16), HueFiller);
  finally
    HueFiller.Free;
  end;

  if vagHueLine in FVisualAid then
  begin
    SetLength(Polygon, 2);
    Polygon[0] := FloatPoint(
      FCenter.X - (FRadius - 16) * Cos(2 * Pi * FHue),
      FCenter.Y - (FRadius - 16) * Sin(2 * Pi * FHue));
    Polygon[1] := FloatPoint(
      FCenter.X - FRadius * Cos(2 * Pi * FHue),
      FCenter.Y - FRadius * Sin(2 * Pi * FHue));

    if FVisualAidType = vatInvert then
    begin
      InvertFiller := TInvertPolygonFiller.Create;
      try
        PolylineFS(FBuffer, Polygon, InvertFiller, False, 2);
      finally
        InvertFiller.Free;
      end;
    end
    else
      PolylineFS(FBuffer, Polygon, FVisualAidColor, False, 2);
  end;

  GR32_Math.SinCos(2 * Pi * FHue, Pos.Y, Pos.X);
  SetLength(Polygon, 3);
  Polygon[0] := FloatPoint(
    FCenter.X - (FRadius - 16) * Pos.X,
    FCenter.Y - (FRadius - 16) * Pos.Y);
  Pos := FloatPoint(-0.5 * (Pos.X + CY * Pos.Y), 0.5 * (Pos.X * CY - Pos.Y));
  Polygon[1] := FloatPoint(
    FCenter.X - (FRadius - 16) * Pos.X,
    FCenter.Y - (FRadius - 16) * Pos.Y);
  Pos := FloatPoint(-0.5 * (Pos.X + CY * Pos.Y), 0.5 * (Pos.X * CY - Pos.Y));
  Polygon[2] := FloatPoint(
    FCenter.X - (FRadius - 16) * Pos.X,
    FCenter.Y - (FRadius - 16) * Pos.Y);

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

  if vagSelection in FVisualAid then
  begin
    Polygon := Circle(
      Polygon[2].X + FValue * (Polygon[1].X + FSaturation * (Polygon[0].X - Polygon[1].X) - Polygon[2].X),
      Polygon[2].Y + FValue * (Polygon[1].Y + FSaturation * (Polygon[0].Y - Polygon[1].Y) - Polygon[2].Y),
      4, 12);

    PolygonFS(FBuffer, Polygon, FSelectedColor);

    if FVisualAidType = vatInvert then
    begin
      InvertFiller := TInvertPolygonFiller.Create;
      try
        PolylineFS(FBuffer, Polygon, InvertFiller, True, 2);
      finally
        InvertFiller.Free;
      end;
    end
    else
      PolylineFS(FBuffer, Polygon, FVisualAidColor, True, 2);
  end;

  inherited;
end;

procedure TCustomColorPickerGTK.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if Button = mbLeft then
  begin
    if Sqrt(Sqr(X - FCenter.X) + Sqr(Y - FCenter.Y)) > FRadius - 16 then
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

  FBuffer.SetSize(Width, Height);
  FBufferValid := False;

  if Height < Width then
  begin
    FRadius := Min(0.5 * Width - 1, 0.5 * Height - 1);
    FCircleSteps := CalculateCircleSteps(FRadius);
    FCenter := FloatPoint(0.5 * Width, 0.5 * Height);
  end
  else
  begin
    FRadius := Min(0.5 * Width - 1, 0.5 * Height - 1);
    FCircleSteps := CalculateCircleSteps(FRadius);
    FCenter := FloatPoint(0.5 * Width, 0.5 * Height);
  end;
end;

procedure TCustomColorPickerGTK.Paint;
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

procedure TCustomColorPickerGTK.PickHue(X, Y: Single);
const
  CTwoPiInv = 1 / (2 * Pi);
begin
  Hue := 0.5 + ArcTan2(Y - FCenter.Y, X - FCenter.X) * CTwoPiInv;
end;

procedure TCustomColorPickerGTK.PickSaturationValue(X, Y: Single);
var
  Pos: TFloatPoint;
  H: Single;
const
  CY = 1.7320508075688772935274463415059;
begin
  with TBarycentricGradientSampler.Create do
  try
    GR32_Math.SinCos(2 * Pi * FHue, Pos.Y, Pos.X);
    Point[0] := FloatPoint(
      FCenter.X - (FRadius - 16) * Pos.X,
      FCenter.Y - (FRadius - 16) * Pos.Y);
    Pos := FloatPoint(-0.5 * (Pos.X + CY * Pos.Y), 0.5 * (Pos.X * CY - Pos.Y));
    Point[1] := FloatPoint(
      FCenter.X - (FRadius - 16) * Pos.X,
      FCenter.Y - (FRadius - 16) * Pos.Y);
    Pos := FloatPoint(-0.5 * (Pos.X + CY * Pos.Y), 0.5 * (Pos.X * CY - Pos.Y));
    Point[2] := FloatPoint(
      FCenter.X - (FRadius - 16) * Pos.X,
      FCenter.Y - (FRadius - 16) * Pos.Y);
    Color[0] := HSVtoRGB(Hue, 1, 1);
    Color[1] := clWhite32;
    Color[2] := clBlack32;

    PrepareSampling;
    FSelectedColor := GetSampleFloatInTriangle(X, Y);
    RGBtoHSV(FSelectedColor, H, FSaturation, FValue);
    Invalidate;
  finally
    Free;
  end;
end;

procedure TCustomColorPickerGTK.SetHue(const Value: Single);
begin
  if FHue <> Value then
  begin
    FHue := Value;
    FSelectedColor := HSVtoRGB(FHue, FSaturation, FValue);
    Invalidate;
  end;
end;

procedure TCustomColorPickerGTK.SetSaturation(const Value: Single);
begin
  if FSaturation <> Value then
  begin
    FSaturation := Value;
    FSelectedColor := HSVtoRGB(FHue, FSaturation, FValue);
    Invalidate;
  end;
end;

procedure TCustomColorPickerGTK.SelectedColorChanged;
begin
  RGBToHSV(FSelectedColor, FHue, FSaturation, FValue);
  inherited;
end;

procedure TCustomColorPickerGTK.SetValue(const Value: Single);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    FSelectedColor := HSVtoRGB(FHue, FSaturation, FValue);
    Invalidate;
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
