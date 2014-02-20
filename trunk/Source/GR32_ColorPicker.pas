unit GR32_ColorPicker;

interface

uses
  Classes, Controls, GR32, GR32_Image, GR32_Polygons, GR32_Containers;

type
  { THueSaturationCirclePolygonFiller }
  THueSaturationCirclePolygonFiller = class(TCustomPolygonFiller)
  private
    FRadius: Single;
    FInvRadius: Single;
    FValue: Single;
    FCenter: TFloatPoint;
    FWebSafe: Boolean;
    procedure SetRadius(const Value: Single);
    procedure FillLineWebSafe(Dst: PColor32; DstX, DstY, Length: Integer;
      AlphaValues: PColor32);
  protected
    function GetFillLine: TFillLineEvent; override;
    procedure FillLine(Dst: PColor32; DstX, DstY, Length: Integer; AlphaValues: PColor32);
  public
    constructor Create(Center: TFloatPoint; Radius, Value: Single; WebSafe: Boolean = False);

    property Center: TFloatPoint read FCenter write FCenter;
    property Radius: Single read FRadius write SetRadius;
    property Value: Single read FValue write FValue;
    property WebSafe: Boolean read FWebSafe write FWebSafe;
  end;

  { TCustomColorPickerHSV }
  TCustomColorPickerHSV = class(TCustomControl)
  type
    TAdjustCalc = procedure (X, Y: Single) of object;
    TVisualAid = set of (vaHueLine, vaSaturationCircle, vaSelection);
  private
    FBuffer: TBitmap32;
    FAdjustCalc: TAdjustCalc;
    FCenter: TFloatPoint;
    FHue: Single;
    FRadius: TFloat;
    FCircleSteps: Integer;
    FSaturation: Single;
    FSelectedColor: TColor32;
    FValue: Single;
    FVisualAid: TVisualAid;
    FBufferValid: Boolean;
    FWebSafe: Boolean;
    procedure PaintColorPicker;
    procedure PickHue(X, Y: Single);
    procedure PickValue(X, Y: Single);
    procedure SetHue(const Value: Single);
    procedure SetSaturation(const Value: Single);
    procedure SetValue(const Value: Single);
    procedure SetVisualAid(const Value: TVisualAid);
    procedure SetSelectedColor(const Value: TColor32);
    procedure SetWebSafe(const Value: Boolean);
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Resize; override;
    procedure Invalidate; override;

    property SelectedColor: TColor32 read FSelectedColor write SetSelectedColor;
    property Hue: Single read FHue write SetHue;
    property Saturation: Single read FSaturation write SetSaturation;
    property Value: Single read FValue write SetValue;
    property VisualAid: TVisualAid read FVisualAid write SetVisualAid;
    property WebSafe: Boolean read FWebSafe write SetWebSafe;
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

implementation

uses
  Math, GR32_Backends, GR32_Math, GR32_ColorGradients, GR32_Blend,
  GR32_VectorUtils;

procedure RoundToWebSafe(var Color: TColor32);
begin
  with TColor32Entry(Color) do
  begin
    R := ((R + $19) div $33) * $33;
    G := ((G + $19) div $33) * $33;
    B := ((B + $19) div $33) * $33;
  end;
end;

procedure DrawCircle(Center: TFloatPoint; Radius: TFloat; Bitmap: TBitmap32;
  Value: Single);

  function BranchlessClipPositive(Value: Single): Single;
  begin
    Result := (Value + Abs(Value)) * 0.5;
  end;

var
  X, Y: Integer;
  ScnLne: PColor32Array;
  CombColor: TColor32;
  XStart: Single;
  YRange: array [0 .. 1] of Integer;
  XRange: array [0 .. 1] of Integer;
  SqrYDist: Single;
  SqrDist, Dist: Single;
  H, S, InvRadius: Single;
  SqrRadMinusOne: Single;
const
  CTwoPiInv = 1 / (2 * Pi);
begin
  // calculate affected scanlines
  YRange[0] := Round(Center.Y - Radius);
  YRange[1] := Round(Center.Y + Radius);

  with Bitmap do
  begin
    // check whether the bitmap needs to be drawn at all
    if (YRange[0] >= Height) or (YRange[1] < 0) or
      (Center.X - Radius >= Width) or (Center.X + Radius < 0) then
      Exit;

    // eventually limit range
    if YRange[0] < 0 then
      YRange[0] := 0;
    if YRange[1] >= Height then
      YRange[1] := Height - 1;

    SqrRadMinusOne := Sqr(BranchlessClipPositive(Radius - 1));
    InvRadius := 1 / Radius;

    for Y := YRange[0] to YRange[1] do
    begin
      // calculate squared vertical distance
      SqrYDist := Sqr(Y - Center.Y);

      XStart := Sqr(Radius) - SqrYDist;
      if XStart <= 0 then
        Continue
      else
        XStart := Sqrt(XStart) - 0.5;

      // calculate affected pixels within this scanline
      XRange[0] := Round(Center.X - XStart);
      XRange[1] := Round(Center.X + XStart);

      // eventually limit range
      if XRange[0] < 0 then
        XRange[0] := 0;
      if XRange[1] >= Width then
        XRange[1] := Width - 1;

      ScnLne := Scanline[Y];
      for X := XRange[0] to XRange[1] do
      begin
        // calculate squared distance
        SqrDist := Sqr(X - Center.X) + SqrYDist;

        H := 0.5 + ArcTan2(Y - Center.Y, X - Center.X) * CTwoPiInv;
        Dist := Sqrt(SqrDist);
        S := Dist * InvRadius;
        if S > 1 then
          S := 1;

        CombColor := HSVtoRGB(H, S, Value);
(*
        if FWebSafe then
          RoundToWebSafe(CombColor);
*)
        if SqrDist >= SqrRadMinusOne then
          ScaleAlpha(CombColor, (Radius - Dist));

        BlendMem(CombColor, ScnLne[X]);
        EMMS;
      end;

    end;
  end;
end;


{ THueSaturationCirclePolygonFiller }

constructor THueSaturationCirclePolygonFiller.Create(Center: TFloatPoint;
  Radius, Value: Single; WebSafe: Boolean = False);
begin
  FCenter := Center;
  FRadius := Max(1, Radius);
  FInvRadius := 1 / FRadius;
  FValue := Value;
  FWebSafe := WebSafe;

  inherited Create;
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

function THueSaturationCirclePolygonFiller.GetFillLine: TFillLineEvent;
begin
  if FWebSafe then
    Result := FillLineWebSafe
  else
    Result := FillLine;
end;

procedure THueSaturationCirclePolygonFiller.SetRadius(const Value: Single);
begin
  if FRadius <> Value then
  begin
    FRadius := Value;
    FInvRadius := 1 / FRadius;
  end;
end;


{ TCustomColorPickerHSV }

constructor TCustomColorPickerHSV.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csOpaque];
  FBuffer := TBitmap32.Create;

  FSelectedColor := clSalmon32;
  FVisualAid := [vaHueLine, vaSaturationCircle, vaSelection];
  RGBToHSV(FSelectedColor, FHue, FSaturation, FValue);

  { Setting a initial size here will cause the control to crash under LCL }
{$IFNDEF FPC}
  Height := 192;
  Width := 192;
{$ENDIF}
end;

destructor TCustomColorPickerHSV.Destroy;
begin
  FBuffer.Free;
  inherited;
end;

procedure TCustomColorPickerHSV.Invalidate;
begin
  FBufferValid := False;
  inherited;
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
      PolylineFS(FBuffer, Polygon, InvertFiller, True, 1.5);
    end;

    if vaHueLine in FVisualAid then
    begin
      SetLength(Polygon, 2);
      Polygon[0] := FCenter;
      Polygon[1] := FloatPoint(
        FCenter.X - FRadius * Cos(2 * Pi * FHue),
        FCenter.Y - FRadius * Sin(2 * Pi * FHue));
      PolylineFS(FBuffer, Polygon, InvertFiller, False, 1.5);
    end;

    if vaSelection in FVisualAid then
    begin
      Polygon := Circle(
        FCenter.X - FSaturation * FRadius * Cos(2 * Pi * FHue),
        FCenter.Y - FSaturation * FRadius * Sin(2 * Pi * FHue), 4, 8);
      PolygonFS(FBuffer, Polygon, FSelectedColor);
      PolylineFS(FBuffer, Polygon, InvertFiller, True, 1.5);
    end;
  finally
    InvertFiller.Free;
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
  PolygonFS(FBuffer, Polygon, clBlack32);

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

procedure TCustomColorPickerHSV.SetSelectedColor(const Value: TColor32);
begin
  if FSelectedColor <> Value then
  begin
    FSelectedColor := Value;
    RGBToHSV(FSelectedColor, FHue, FSaturation, FValue);
    Invalidate;
  end;
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

procedure TCustomColorPickerHSV.SetWebSafe(const Value: Boolean);
begin
  if FWebSafe <> Value then
  begin
    FWebSafe := Value;
    Invalidate;
  end;
end;

end.
