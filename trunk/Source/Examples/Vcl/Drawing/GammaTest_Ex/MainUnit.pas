unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GR32, GR32_Image, Vcl.StdCtrls, GR32_RangeBars, Vcl.ExtCtrls;

type
  TFrmGammaTest = class(TForm)
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
    procedure PaintBox32PaintBuffer(Sender: TObject);
    procedure GbrContrastChange(Sender: TObject);
    procedure GbrGammaChange(Sender: TObject);
    procedure GbrThicknessChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FrmGammaTest: TFrmGammaTest;

implementation

uses
  GR32_LowLevel, GR32_VectorUtils, GR32_Polygons, GR32_Paths;

{$R *.dfm}

procedure TFrmGammaTest.FormShow(Sender: TObject);
begin
  GbrContrastChange(Sender);
  GbrGammaChange(Sender);
  GbrThicknessChange(Sender);
end;

procedure TFrmGammaTest.GbrContrastChange(Sender: TObject);
begin
  LblContrastValue.Caption := '';
  PaintBox32.Invalidate;
end;

procedure TFrmGammaTest.GbrGammaChange(Sender: TObject);
begin
  LblGammaValue.Caption := FloatToStrF(0.01 * GbrGamma.Position, ffFixed, 3,
    3);
  SetGamma(0.01 * GbrGamma.Position);
  PaintBox32.Invalidate;
end;

procedure TFrmGammaTest.GbrThicknessChange(Sender: TObject);
begin
  LblThicknessValue.Caption := FloatToStrF(0.01 * GbrThickness.Position,
    ffFixed, 3, 3);
  PaintBox32.Invalidate;
end;

procedure TFrmGammaTest.PaintBox32PaintBuffer(Sender: TObject);
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
  Renderer := TPolygonRenderer32LCD.Create;
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
      DeltaY := GAMMA_TABLE[Index];

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
      FloatPoint(Radius.X - 15, Radius.Y - 15), 150);
    Renderer.PolyPolygonFS(BuildPolyPolyLine(PolyPolygon(Outline), True,
      Thickness));
  finally
    Renderer.Free;
  end;
end;

initialization

  SetGamma(1);

end.
