unit MainUnit;

interface

{$I GR32.inc}

uses
  {$IFDEF FPC}LCLIntf, {$ELSE}Windows, {$ENDIF} SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, GR32, GR32_Image, GR32_Polygons;

type
  TScatterPoint = record
    X, Y, Z: TFloat;
    Color: TColor32;
  end;

  TFmScatterPlot = class(TForm)
    Image32: TImage32;
    procedure FormCreate(Sender: TObject);
    procedure Image32PaintStage(Sender: TObject; Buffer: TBitmap32;
      StageNum: Cardinal);
    procedure Image32Click(Sender: TObject);
  private
    FPoints: array of TScatterPoint;
    FRadius: TFloat;
    FSelection: TFloat;
    FCircle: TArrayOfFloatPoint;
    FBounds: array [0..1] of TFloat;
    FRenderer: TPolygonRenderer32VPR;
    procedure Generate;
    procedure Draw;
  public
    procedure ApplicationIdleHandler(Sender: TObject; var Done: Boolean);
  end;

var
  FmScatterPlot: TFmScatterPlot;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, GR32_Math, GR32_LowLevel, GR32_VectorUtils;

procedure TFmScatterPlot.FormCreate(Sender: TObject);
var
  NumPoints: Integer;
begin
  Image32.Bitmap.SetSize(Image32.Width, Image32.Height);

  FRenderer := TPolygonRenderer32VPR.Create;
  FRenderer.Bitmap := Image32.Bitmap;

  FRadius := 2;
  FSelection := 0.5;
  FBounds[0] := 0.1;
  FBounds[1] := 0.2;

  FCircle := Circle(0, 0, FRadius, 8);

  Application.OnIdle := ApplicationIdleHandler;

  if TryStrToInt(ParamStr(1), NumPoints) then
    NumPoints := EnsureRange(NumPoints, 1, 20000)
  else
    NumPoints := 10000;

  SetLength(FPoints, NumPoints);
  Generate;
  Draw;
end;

procedure TFmScatterPlot.Generate;
var
  Index: Cardinal;
  Radius: TFloatPoint;
  Z, Dist: TFloat;
  ScatterPnt, Pnt: TFloatPoint;
begin
  Radius.X := Image32.Width / 3.5;
  Radius.Y := Image32.Height / 3.5;

  for Index := 0 to Length(FPoints) - 1 do
  begin
    Z := Random;
    FPoints[Index].Z := Z;

    GR32_Math.SinCos(2 * Pi * Z, Radius.X, Radius.Y, Pnt.Y, Pnt.X);

    Dist := 0.5 * Radius.X * Random;

    GR32_Math.SinCos(2 * Pi * Random, ScatterPnt.Y, ScatterPnt.X);

    FPoints[Index].X := 0.5 * Image32.Width + Pnt.X + ScatterPnt.X * Dist;
    FPoints[Index].Y := 0.5 * Image32.Height + Pnt.Y + ScatterPnt.Y * Dist;
    FPoints[Index].Color := HSLtoRGB(Z, 0.7, 0.5);
  end;
end;

procedure TFmScatterPlot.Image32Click(Sender: TObject);
begin
  Generate;
  Draw;
end;

procedure TFmScatterPlot.Image32PaintStage(Sender: TObject; Buffer: TBitmap32;
  StageNum: Cardinal);
begin
//  Buffer.Clear($FFFFFFFF);
end;

procedure TFmScatterPlot.ApplicationIdleHandler(Sender: TObject;
  var Done: Boolean);
var
  Index: Cardinal;
begin
  for Index := 0 to Length(FPoints) - 1 do
  begin
    FPoints[Index].X := FPoints[Index].X + FSelection * (Random - 0.5);
    FPoints[Index].Y := FPoints[Index].Y + FSelection * (Random - 0.5);
    FPoints[Index].Z := FPoints[Index].Z + FSelection * (0.01 * Random - 0.005);

    if FPoints[Index].Z < 0.0 then
      FPoints[Index].Z := 0.0;

    if FPoints[Index].Z > 1.0 then
      FPoints[Index].Z := 1.0;
  end;

  FBounds[0] := FBounds[0] + 0.001 * Random;
  FBounds[1] := FBounds[1] + 0.001 * Random;

  FSelection := EnsureRange(FSelection + 0.02 * Random - 0.01, 0, 1);

  if FBounds[0] > 1 then
    FBounds[0] := 0;
  if FBounds[1] > 1 then
    FBounds[1] := 0;
  Draw;
end;

procedure TFmScatterPlot.Draw;
var
  Index: Cardinal;
  Alpha: TFloat;

begin
  Image32.Bitmap.Clear($FFFFFFFF);

  for Index := 0 to Length(FPoints) - 1 do
  begin
    Alpha := 1.0;
    if FBounds[1] < FBounds[0] then
    begin
      if (FPoints[Index].Z < FBounds[0]) and (FPoints[Index].Z > FBounds[1]) then
      begin
        if FBounds[0] - FPoints[Index].Z < FPoints[Index].Z - FBounds[1] then
          Alpha := 1.0 - (FBounds[0] - FPoints[Index].Z) * FSelection * 100.0
        else
          Alpha := 1.0 - (FPoints[Index].Z - FBounds[1]) * FSelection * 100.0;
      end;
    end
    else
    begin
      if FPoints[Index].Z < FBounds[0] then
        Alpha := 1.0 - (FBounds[0] - FPoints[Index].Z) * FSelection * 100.0;

      if FPoints[Index].Z > FBounds[1] then
        Alpha := 1.0 - (FPoints[Index].Z - FBounds[1]) * FSelection * 100.0;
    end;

    if Alpha < 0.0 then
      Continue;

    if Alpha > 1.0 then
      Alpha := 1.0;

    FRenderer.Color := SetAlpha(FPoints[Index].Color, Round(Alpha * $FF));
    FRenderer.PolygonFS(TranslatePolygon(FCircle, FPoints[Index].X,
      FPoints[Index].Y));
  end;
end;

end.
