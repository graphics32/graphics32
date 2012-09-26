unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, GR32, GR32_Image, GR32_ColorGradients;

type
  TColorPoint = record
    X, Y: TFloat;
    Color: TColor32;
  end;

  TFrmGradientSampler = class(TForm)
    PaintBox32: TPaintBox32;
    MainMenu: TMainMenu;
    MnuFile: TMenuItem;
    MnuClose: TMenuItem;
    MnuGradient: TMenuItem;
    MnuGradientRadial: TMenuItem;
    MnuGradientConic: TMenuItem;
    MnuGradientDiamond: TMenuItem;
    MnuGradientXY: TMenuItem;
    MnuGradientXYSqrt: TMenuItem;
    MnuGradientLinear: TMenuItem;
    MnuGradientCustom: TMenuItem;
    MnuWrapMode: TMenuItem;
    MnuWrapModeClamp: TMenuItem;
    MnuWrapModeRepeat: TMenuItem;
    MnuWrapModeMirror: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MnuCloseClick(Sender: TObject);
    procedure MnuGradientConicClick(Sender: TObject);
    procedure MnuGradientDiamondClick(Sender: TObject);
    procedure MnuGradientLinearClick(Sender: TObject);
    procedure MnuGradientRadialClick(Sender: TObject);
    procedure MnuGradientXYClick(Sender: TObject);
    procedure MnuGradientXYSqrtClick(Sender: TObject);
    procedure MnuGradientCustomClick(Sender: TObject);
    procedure PaintBox32PaintBuffer(Sender: TObject);
    procedure PaintBox32MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox32MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox32MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MnuWrapModeClampClick(Sender: TObject);
    procedure MnuWrapModeRepeatClick(Sender: TObject);
    procedure MnuWrapModeMirrorClick(Sender: TObject);
  private
    FCenter: TFloatPoint;
    FWrapMode: TWrapMode;
    FGradCenter: TFloatPoint;
    FAngle, FRadius: TFloat;
    FStarVertices: Integer;
    FLastPos: TFloatPoint;
    FOutline: TArrayOfFloatPoint;
    FGradientSampler: TCustomGradientSampler;
    FTriangularGradientSampler: TTriangularGradientSampler;
  end;

  TMyGradient = class(TCustomLookUpTableGradientSampler)
  private
    FPolygon: PArrayOfFloatPoint;
    FRadius: TFloat;
    FScale: TFloat;
    procedure SetRadius(const Value: TFloat);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure UpdateInternals; override;
  public
    constructor Create; override;
    function GetSampleFloat(X: Single; Y: Single): TColor32; override;

    property Polygon: PArrayOfFloatPoint read FPolygon write FPolygon;
    property Radius: TFloat read FRadius write SetRadius;
  end;

var
  FrmGradientSampler: TFrmGradientSampler;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, GR32_Math, GR32_LowLevel, GR32_Polygons, GR32_Geometry,
  GR32_VectorUtils;

{ TMyGradient }

constructor TMyGradient.Create;
begin
  inherited;
  FRadius := 10;
end;

function TMyGradient.GetSampleFloat(X, Y: Single): TColor32;
var
  Pt: TFloatPoint;
  Index: Integer;
  Dist, MinDist, MaxDist: TFloat;
begin
  Pt := FloatPoint(X, Y);
  Dist := Distance(Pt, FPolygon^[0]) ;
  MinDist := Dist;
  MaxDist := Dist;
  for Index := 1 to High(FPolygon^) do
  begin
    Dist := Distance(Pt, FPolygon^[Index]);
    if Dist < MinDist then
      MinDist := Dist
    else
    if Dist > MaxDist then
      MaxDist := Dist;
  end;
  Result := LutPtr^[WrapProc(Round((MaxDist + MinDist) * FScale),
    LutMask)];
end;

procedure TMyGradient.SetRadius(const Value: TFloat);
begin
  if FRadius <> Value then
  begin
    FRadius := Value;
    FInitialized := False;
  end;
end;

procedure TMyGradient.UpdateInternals;
begin
  inherited;
  FScale := LutMask / FRadius;
end;

procedure TMyGradient.AssignTo(Dest: TPersistent);
begin
  inherited;
end;


{ TFrmGradientSampler }

procedure TFrmGradientSampler.FormCreate(Sender: TObject);
var
  Index: Integer;
begin
  FGradientSampler := TDiamondGradientSampler.Create;
  FCenter := FloatPoint(0.5 * PaintBox32.Width, 0.5 * PaintBox32.Height);
  FGradCenter := FCenter;
  FAngle := 0.4;
  FRadius := 50;
  FWrapMode := wmMirror;
  FStarVertices := 5;
  FOutline := Star(FCenter, 180, FStarVertices);

  FTriangularGradientSampler := TTriangularGradientSampler.Create;
  for Index := 0 to 2 do
  begin
    FTriangularGradientSampler.Point[Index] := FloatPoint(
      PaintBox32.Width * Random, PaintBox32.Height * Random);
    FTriangularGradientSampler.Color[Index] := SetAlpha(Random($FFFFFF), $FF);
  end;

  PaintBox32.Invalidate;
end;

procedure TFrmGradientSampler.FormDestroy(Sender: TObject);
begin
  FTriangularGradientSampler.Free;
  FGradientSampler.Free;
end;

procedure TFrmGradientSampler.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    27: Close;
    52..57:
      begin
        FStarVertices := Key - 48;
        FOutline := Star(FCenter, 180, FStarVertices);
        PaintBox32.Invalidate;
      end;
  end;
end;

procedure TFrmGradientSampler.MnuCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmGradientSampler.MnuGradientRadialClick(Sender: TObject);
var
  OldGradientSampler: TCustomGradientSampler;
begin
  OldGradientSampler := FGradientSampler;
  FGradientSampler := TRadialGradientSampler.Create;
  FGradientSampler.Assign(OldGradientSampler);
  OldGradientSampler.Free;
  MnuGradientRadial.Checked := True;
  PaintBox32.Invalidate;
end;

procedure TFrmGradientSampler.MnuGradientConicClick(Sender: TObject);
var
  OldGradientSampler: TCustomGradientSampler;
begin
  OldGradientSampler := FGradientSampler;
  FGradientSampler := TConicGradientSampler.Create;
  FGradientSampler.Assign(OldGradientSampler);
  OldGradientSampler.Free;
  MnuGradientConic.Checked := True;
  PaintBox32.Invalidate;
end;

procedure TFrmGradientSampler.MnuGradientCustomClick(Sender: TObject);
var
  OldGradientSampler: TCustomGradientSampler;
begin
  OldGradientSampler := FGradientSampler;
  FGradientSampler := TMyGradient.Create;
  FGradientSampler.Assign(OldGradientSampler);
  OldGradientSampler.Free;
  MnuGradientCustom.Checked := True;
  PaintBox32.Invalidate;
end;

procedure TFrmGradientSampler.MnuGradientDiamondClick(Sender: TObject);
var
  OldGradientSampler: TCustomGradientSampler;
begin
  OldGradientSampler := FGradientSampler;
  FGradientSampler := TDiamondGradientSampler.Create;
  FGradientSampler.Assign(OldGradientSampler);
  OldGradientSampler.Free;
  MnuGradientDiamond.Checked := True;
  PaintBox32.Invalidate;
end;

procedure TFrmGradientSampler.MnuGradientXYClick(Sender: TObject);
var
  OldGradientSampler: TCustomGradientSampler;
begin
  OldGradientSampler := FGradientSampler;
  FGradientSampler := TXYGradientSampler.Create;
  FGradientSampler.Assign(OldGradientSampler);
  OldGradientSampler.Free;
  MnuGradientXY.Checked := True;
  PaintBox32.Invalidate;
end;

procedure TFrmGradientSampler.MnuGradientXYSqrtClick(Sender: TObject);
var
  OldGradientSampler: TCustomGradientSampler;
begin
  OldGradientSampler := FGradientSampler;
  FGradientSampler := TXYSqrtGradientSampler.Create;
  FGradientSampler.Assign(OldGradientSampler);
  OldGradientSampler.Free;
  MnuGradientXYSqrt.Checked := True;
  PaintBox32.Invalidate;
end;

procedure TFrmGradientSampler.MnuWrapModeClampClick(Sender: TObject);
begin
  FWrapMode := wmClamp;
  MnuWrapModeClamp.Checked := True;
  PaintBox32.Invalidate;
end;

procedure TFrmGradientSampler.MnuWrapModeMirrorClick(Sender: TObject);
begin
  FWrapMode := wmMirror;
  MnuWrapModeMirror.Checked := True;
  PaintBox32.Invalidate;
end;

procedure TFrmGradientSampler.MnuWrapModeRepeatClick(Sender: TObject);
begin
  FWrapMode := wmRepeat;
  MnuWrapModeRepeat.Checked := True;
  PaintBox32.Invalidate;
end;

procedure TFrmGradientSampler.MnuGradientLinearClick(Sender: TObject);
var
  OldGradientSampler: TCustomGradientSampler;
begin
  OldGradientSampler := FGradientSampler;
  FGradientSampler := TXGradientSampler.Create;
  FGradientSampler.Assign(OldGradientSampler);
  OldGradientSampler.Free;
  MnuGradientLinear.Checked := True;
  PaintBox32.Invalidate;
end;

procedure TFrmGradientSampler.PaintBox32MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
begin
  if (ssShift in Shift) then
  begin
    for Index := 0 to 2 do
    begin
      FTriangularGradientSampler.Point[Index] := FloatPoint(
        PaintBox32.Width * Random, PaintBox32.Height * Random);
      FTriangularGradientSampler.Color[Index] := SetAlpha(Random($FFFFFF), $FF);
    end;

    PaintBox32.Invalidate;
    Exit;
  end;

  PaintBox32.OnMouseMove := PaintBox32MouseMove;
  if ssRight in Shift then
  begin
    if ssShift in Shift then
      FCenter := FloatPoint(X, Y)
    else
      FGradCenter := FloatPoint(X, Y);
    PaintBox32.Invalidate;
  end;
  FLastPos := FloatPoint(X, Y);
end;

procedure TFrmGradientSampler.PaintBox32MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssRight in Shift then
  begin
    if ssShift in Shift then
      FCenter := FloatPoint(X, Y)
    else
      FGradCenter := FloatPoint(X, Y);
    PaintBox32.Invalidate;
  end else
  if ssLeft in Shift then
  begin
    if (Y = FGradCenter.Y) and (X = FGradCenter.X) then
    begin
      FRadius := 0;
    end
    else
    begin
      FAngle := FAngle - ArcTan2(Y - FGradCenter.Y, X - FGradCenter.X) +
        ArcTan2(FLastPos.Y - FGradCenter.Y, FLastPos.X - FGradCenter.X);
      FRadius := FRadius * (Hypot(Y - FGradCenter.Y, X - FGradCenter.X) /
        Distance(FLastPos, FGradCenter));
    end;
    PaintBox32.Invalidate;
  end;
  FLastPos := FloatPoint(X, Y);
end;

procedure TFrmGradientSampler.PaintBox32MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  PaintBox32.OnMouseMove := nil;
end;

procedure TFrmGradientSampler.PaintBox32PaintBuffer(Sender: TObject);
var
  Renderer: TPolygonRenderer32;
  SamplerFiller: TSamplerFiller;
  X, Y: Integer;
begin
  PaintBox32.Buffer.Clear(clWhite32);

  FTriangularGradientSampler.PrepareSampling;
  with PaintBox32.Buffer do
    for Y := 0 to Height - 1 do
      for X := 0 to Width - 1 do
        Pixel[X, Y] := FTriangularGradientSampler.GetSampleInt(X, Y);

  Renderer := TPolygonRenderer32VPR.Create(PaintBox32.Buffer);
  try
    Renderer.Color := clWhite32;

    if FGradientSampler is TCustomCenterRadiusLutGradientSampler then
      TCustomCenterRadiusLutGradientSampler(FGradientSampler).Radius := FRadius;

    if FGradientSampler is TCustomCenterRadiusAngleLutGradientSampler then
      with TCustomCenterRadiusAngleLutGradientSampler(FGradientSampler) do
        Angle := FAngle;

    if FGradientSampler is TConicGradientSampler then
      TConicGradientSampler(FGradientSampler).Angle := FAngle;

    if FGradientSampler is TCustomCenterLutGradientSampler then
      TCustomCenterLutGradientSampler(FGradientSampler).Center := FGradCenter;

    FGradientSampler.Gradient.StartColor := clLime32;
    FGradientSampler.Gradient.EndColor := clGreen32;
    FGradientSampler.WrapMode := FWrapMode;

    SamplerFiller := TSamplerFiller.Create(FGradientSampler);
    try
      Renderer.Filler := SamplerFiller;

      if FGradientSampler is TMyGradient then
        with TMyGradient(FGradientSampler) do
        begin
          Polygon := @FOutline;
          Radius := Self.FRadius;
        end;

      Renderer.PolygonFS(FOutline);
    finally
      SamplerFiller.Free;
    end;

    Renderer.Filler := nil;
    Renderer.Color := clRed32;
    Renderer.PolyPolygonFS(BuildPolyPolyline(PolyPolygon(FOutline), True, 5,
      jsRound, esRound));
  finally
    Renderer.Free;
  end;
end;

end.

