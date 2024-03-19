unit MainUnit;

interface

{$I GR32.inc}

uses
  {$IFDEF FPC}LCLIntf, {$ELSE}Windows, {$ENDIF} Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, GR32, GR32_Image,
  GR32_RangeBars;

type
  TFrmGammaCorrection = class(TForm)
    GbrContrast: TGaugeBar;
    GbrGamma: TGaugeBar;
    GbrThickness: TGaugeBar;
    LblContrast: TLabel;
    LblContrastValue: TLabel;
    LblGamma: TLabel;
    LblGammaValue: TLabel;
    LblThickness: TLabel;
    LblThicknessValue: TLabel;
    PaintBox32: TPaintBox32;
    PnControl: TPanel;
    procedure FormShow(Sender: TObject);
    procedure GbrContrastChange(Sender: TObject);
    procedure GbrGammaChange(Sender: TObject);
    procedure GbrThicknessChange(Sender: TObject);
    procedure PaintBox32PaintBuffer(Sender: TObject);
  end;

var
  FrmGammaCorrection: TFrmGammaCorrection;

implementation

uses
  Types,
  GR32_LowLevel,
  GR32_Gamma,
  GR32_VectorUtils,
  GR32_Polygons;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFrmGammaCorrection.FormShow(Sender: TObject);
begin
  GbrContrastChange(Sender);
  GbrGammaChange(Sender);
  GbrThicknessChange(Sender);
end;

procedure TFrmGammaCorrection.GbrContrastChange(Sender: TObject);
begin
  LblContrastValue.Caption := IntToStr(GbrContrast.Position);
  PaintBox32.Invalidate;
end;

procedure TFrmGammaCorrection.GbrGammaChange(Sender: TObject);
var
  GammaValue: Double;
begin
  GammaValue := 0.001 * GbrGamma.Position;
  LblGammaValue.Caption := FloatToStrF(GammaValue, ffFixed, 4, 3);
  SetGamma(GammaValue);
  PaintBox32.Invalidate;
end;

procedure TFrmGammaCorrection.GbrThicknessChange(Sender: TObject);
begin
  LblThicknessValue.Caption := FloatToStrF(0.01 * GbrThickness.Position,
    ffFixed, 3, 3);
  PaintBox32.Invalidate;
end;

procedure TFrmGammaCorrection.PaintBox32PaintBuffer(Sender: TObject);
var
  Renderer: TPolygonRenderer32VPR;
  W, H: Integer;
  Thickness: TFloat;
  Color: TColor32;
  Outline: TArrayOfFloatPoint;
  Radius: TFloatPoint;
  Index, Contrast, DeltaY: Byte;
  StartPnt: TFloatPoint;
begin
  W := PaintBox32.Width;
  H := PaintBox32.Height;
  Thickness := 0.01 * GbrThickness.Position;
  Radius := FloatPoint(W / 3, H / 3);
  Renderer := TPolygonRenderer32VPR.Create;
  try
    Renderer.Bitmap := PaintBox32.Buffer;

    Contrast := $FF - GbrContrast.Position;

    Color := Gray32(Contrast);
    PaintBox32.Buffer.FillRect(0, 0, Trunc(W) div 2, Trunc(H), Color);

    Color := Gray32($FF - Contrast);
    PaintBox32.Buffer.FillRect(Trunc(W) div 2, 0, Trunc(W), Trunc(H),
      Color);

    Color := Color32($FF, Contrast, Contrast);
    PaintBox32.Buffer.FillRect(0, 0, Trunc(W), Trunc(H) div 2, Color);

    Renderer.Color := Color32($50, $7F, $50);
    StartPnt := FloatPoint((W - 256) * 0.5, 50);

    SetLength(Outline, 256);
    for Index := 0 to High(Byte) do
    begin
      DeltaY := GAMMA_ENCODING_TABLE[Index];

      Outline[Index] := FloatPoint(StartPnt.X + Index, StartPnt.Y + 255 - DeltaY)
    end;
    Renderer.PolygonFS(BuildPolyline(Outline, Thickness));

    Renderer.Color := Color32($FF, 0, 0);
    Outline := Ellipse(FloatPoint(W * 0.5, H * 0.5), Radius, 150);
    Renderer.PolyPolygonFS(BuildPolyPolyLine(PolyPolygon(Outline), True,
      Thickness));

    Renderer.Color := Color32(0, $FF, 0);
    Outline := Ellipse(FloatPoint(W * 0.5, H * 0.5),
      FloatPoint(Radius.X - 5, Radius.Y - 5), 150);
    Renderer.PolyPolygonFS(BuildPolyPolyLine(PolyPolygon(Outline), True,
      Thickness));

    Renderer.Color := Color32(0, 0, $FF);
    Outline := Ellipse(FloatPoint(W * 0.5, H * 0.5),
      FloatPoint(Radius.X - 10, Radius.Y - 10), 150);
    Renderer.PolyPolygonFS(BuildPolyPolyLine(PolyPolygon(Outline), True,
      Thickness));

    Renderer.Color := clBlack32;
    Outline := Ellipse(FloatPoint(W * 0.5, H * 0.5),
      FloatPoint(Radius.X - 15, Radius.Y - 15), 150);
    Renderer.PolyPolygonFS(BuildPolyPolyLine(PolyPolygon(Outline), True,
      Thickness));

    Renderer.Color := clWhite32;
    Outline := Ellipse(FloatPoint(W * 0.5, H * 0.5),
      FloatPoint(Radius.X - 20, Radius.Y - 20), 150);
    Renderer.PolyPolygonFS(BuildPolyPolyLine(PolyPolygon(Outline), True,
      Thickness));
  finally
    Renderer.Free;
  end;
end;

end.
