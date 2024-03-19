unit MainUnit;

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
 * The Original Code is Gradient Sampler Example
 *
 * The Initial Developer(s) of the Original Code is:
 * Christian-W. Budde <Christian@savioursofsoul.de>
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2012
 * the Initial Developer. All Rights Reserved.
 *
 *
 * ***** END LICENSE BLOCK ***** *)

interface

uses
  {$IFDEF FPC} LCLIntf, LResources, Buttons, {$ENDIF} SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, Menus, ExtCtrls,
  GR32, GR32_Image, GR32_ColorGradients;

type
  TMesh = record
    Point: TFloatPoint;
    Velocity: TFloatPoint;
    Color: TColor32;
    HueChange: Single;
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
    Animation: TTimer;
    MnuBackground: TMenuItem;
    MnuBackgroundGradientTriangular: TMenuItem;
    MnuBackgroundGradientVoronoi: TMenuItem;
    MnuBackgroundGradientShepards: TMenuItem;
    MnuBackgroundGradientCustomIDW: TMenuItem;
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
    procedure MnuWrapModeClampClick(Sender: TObject);
    procedure MnuWrapModeRepeatClick(Sender: TObject);
    procedure MnuWrapModeMirrorClick(Sender: TObject);
    procedure PaintBox32PaintBuffer(Sender: TObject);
    procedure PaintBox32MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox32MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox32MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure AnimationTimer(Sender: TObject);
    procedure PaintBox32DblClick(Sender: TObject);
    procedure MnuBackgroundGradientVoronoiClick(Sender: TObject);
    procedure MnuBackgroundGradientTriangularClick(Sender: TObject);
    procedure MnuBackgroundGradientShepardsClick(Sender: TObject);
    procedure MnuBackgroundGradientCustomIDWClick(Sender: TObject);
  private
    FCenter: TFloatPoint;
    FWrapMode: TWrapMode;
    FGradCenter: TFloatPoint;
    FAngle, FRadius: TFloat;
    FStarAngle: TFloat;
    FStarVertices: Integer;
    FLastPos: TFloatPoint;
    FOutline: TArrayOfFloatPoint;
    FStartColor: TColor32;
    FEndColor: TColor32;
    FGradientSampler: TCustomGradientSampler;
    FBackgroundGradientSampler: TCustomSparsePointGradientSampler;
    FMesh: array of TMesh;
  public
    procedure UpdateBackgroundGradientSampler;
  end;

  TMyGradient = class(TCustomGradientLookUpTableSampler)
  private
    FPolygon: PArrayOfFloatPoint;
    FRadius: TFloat;
    FScale: TFloat;
    procedure SetRadius(const Value: TFloat);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure UpdateInternals; override;
  public
    constructor Create(WrapMode: TWrapMode = wmMirror); override;
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
  Math,
  Types,
  GR32_Math,
  GR32_LowLevel,
  GR32_Polygons,
  GR32_Geometry,
  GR32_VectorUtils;

{ TMyGradient }

constructor TMyGradient.Create(WrapMode: TWrapMode = wmMirror);
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
  FStarAngle := 0;
  FOutline := Star(FCenter, 180, FStarVertices, FStarAngle);

  FBackgroundGradientSampler := TBarycentricGradientSampler.Create;
  SetLength(FMesh, FBackgroundGradientSampler.Count);
  for Index := 0 to High(FMesh) do
  begin
    FMesh[Index].Point := FloatPoint(PaintBox32.Width * Random,
      PaintBox32.Height * Random);
    FMesh[Index].Velocity := FloatPoint(2 * Random - 1, 2 * Random - 1);
    FMesh[Index].Color := SetAlpha(Random($FFFFFF), $FF);
    FMesh[Index].HueChange := 0.001 * (2 * Random - 1);
  end;
  FStartColor := SetAlpha(Random($FFFFFF), $FF);
  FEndColor := SetAlpha(Random($FFFFFF), $FF);

  UpdateBackgroundGradientSampler;

  PaintBox32.Invalidate;
end;

procedure TFrmGradientSampler.UpdateBackgroundGradientSampler;
var
  Index: Integer;
begin
  for Index := 0 to FBackgroundGradientSampler.Count - 1 do
  begin
    FBackgroundGradientSampler.Point[Index] := FMesh[Index].Point;
    FBackgroundGradientSampler.Color[Index] := FMesh[Index].Color;
  end;
end;

procedure TFrmGradientSampler.FormDestroy(Sender: TObject);
begin
  FBackgroundGradientSampler.Free;
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

procedure TFrmGradientSampler.MnuBackgroundGradientCustomIDWClick(
  Sender: TObject);
var
  Index: Integer;
begin
  if not MnuBackgroundGradientCustomIDW.Checked then
  begin
    MnuBackgroundGradientCustomIDW.Checked := True;
    FBackgroundGradientSampler.Free;
    FBackgroundGradientSampler := TInvertedDistanceWeightingSampler.Create;
    TInvertedDistanceWeightingSampler(FBackgroundGradientSampler).Power := 8;
    with TCustomArbitrarySparsePointGradientSampler(FBackgroundGradientSampler) do
      for Index := 0 to High(FMesh) do
        Add(FMesh[Index].Point, FMesh[Index].Color);
    PaintBox32.Invalidate;
  end;
end;

procedure TFrmGradientSampler.MnuBackgroundGradientShepardsClick(Sender: TObject);
var
  Index: Integer;
begin
  if not MnuBackgroundGradientShepards.Checked then
  begin
    MnuBackgroundGradientShepards.Checked := True;
    FBackgroundGradientSampler.Free;
    FBackgroundGradientSampler := TInvertedDistanceWeightingSampler.Create;
    with TCustomArbitrarySparsePointGradientSampler(FBackgroundGradientSampler) do
      for Index := 0 to High(FMesh) do
        Add(FMesh[Index].Point, FMesh[Index].Color);
    PaintBox32.Invalidate;
  end;
end;

procedure TFrmGradientSampler.MnuBackgroundGradientTriangularClick(Sender: TObject);
begin
  if not MnuBackgroundGradientTriangular.Checked then
  begin
    MnuBackgroundGradientTriangular.Checked := True;
    FBackgroundGradientSampler.Free;
    FBackgroundGradientSampler := TBarycentricGradientSampler.Create;
    UpdateBackgroundGradientSampler;
    PaintBox32.Invalidate;
  end;
end;

procedure TFrmGradientSampler.MnuBackgroundGradientVoronoiClick(Sender: TObject);
var
  Index: Integer;
begin
  if not MnuBackgroundGradientVoronoi.Checked then
  begin
    MnuBackgroundGradientVoronoi.Checked := True;
    FBackgroundGradientSampler.Free;
    FBackgroundGradientSampler := TVoronoiSampler.Create;
    with TCustomArbitrarySparsePointGradientSampler(FBackgroundGradientSampler) do
      for Index := 0 to High(FMesh) do
        Add(FMesh[Index].Point, FMesh[Index].Color);
    PaintBox32.Invalidate;
  end;
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

procedure TFrmGradientSampler.PaintBox32DblClick(Sender: TObject);
begin
  Animation.Enabled := not Animation.Enabled;
end;

procedure TFrmGradientSampler.PaintBox32MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
begin
  if (ssCtrl in Shift) then
  begin
    Animation.Enabled := not Animation.Enabled;
    Exit;
  end;

  if (ssShift in Shift) then
  begin
    for Index := 0 to 2 do
    begin
      FMesh[Index].Point := FloatPoint(PaintBox32.Width * Random,
        PaintBox32.Height * Random);
      FMesh[Index].Velocity := FloatPoint(2 * Random - 1, 2 * Random - 1);
      FMesh[Index].Color := SetAlpha(Random($FFFFFF), $FF);
      FMesh[Index].HueChange := 0.001 * (2 * Random - 1);
    end;
    UpdateBackgroundGradientSampler;

    FStartColor := SetAlpha(Random($FFFFFF), $FF);
    FEndColor := SetAlpha(Random($FFFFFF), $FF);

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

  FBackgroundGradientSampler.PrepareSampling;
  with PaintBox32.Buffer do
    for Y := 0 to Height - 1 do
      for X := 0 to Width - 1 do
        Pixel[X, Y] := FBackgroundGradientSampler.GetSampleInt(X, Y);

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

    FGradientSampler.Gradient.StartColor := FStartColor;
    FGradientSampler.Gradient.EndColor := FEndColor;
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
    Renderer.Color := clBlack32;
    Renderer.PolyPolygonFS(BuildPolyPolyline(PolyPolygon(FOutline), True, 5,
      jsRound, esRound));
  finally
    Renderer.Free;
  end;
end;

procedure TFrmGradientSampler.AnimationTimer(Sender: TObject);
var
  Index: Integer;
  H, S, L: Single;
begin
  FAngle := FAngle + 0.01;
  FStarAngle := FStarAngle + 0.01;
  FOutline := Star(FCenter, 180, FStarVertices, FStarAngle);

  for Index := 0 to FBackgroundGradientSampler.Count - 1 do
  begin
    FMesh[Index].Point.X := FMesh[Index].Point.X + FMesh[Index].Velocity.X;
    if FMesh[Index].Point.X < 0 then
    begin
      FMesh[Index].Point.X := -FMesh[Index].Point.X;
      FMesh[Index].Velocity.X := -FMesh[Index].Velocity.X;
    end;

    if FMesh[Index].Point.X >= PaintBox32.Width then
    begin
      FMesh[Index].Point.X := 2 * PaintBox32.Width - FMesh[Index].Point.X;
      FMesh[Index].Velocity.X := -FMesh[Index].Velocity.X;
    end;

    FMesh[Index].Point.Y := FMesh[Index].Point.Y + FMesh[Index].Velocity.Y;
    if FMesh[Index].Point.Y < 0 then
    begin
      FMesh[Index].Point.Y := -FMesh[Index].Point.Y;
      FMesh[Index].Velocity.Y := -FMesh[Index].Velocity.Y;
    end;

    FMesh[Index].Point.Y := FMesh[Index].Point.Y + FMesh[Index].Velocity.Y;
    if FMesh[Index].Point.Y >= PaintBox32.Height then
    begin
      FMesh[Index].Point.Y := 2 * PaintBox32.Height - FMesh[Index].Point.Y;
      FMesh[Index].Velocity.Y := -FMesh[Index].Velocity.Y;
    end;
    RGBtoHSL(FMesh[Index].Color, H, S, L);
    FMesh[Index].Color := HSLtoRGB(H + FMesh[Index].HueChange, S, L);
  end;
  UpdateBackgroundGradientSampler;

  PaintBox32.Invalidate;
end;

end.
