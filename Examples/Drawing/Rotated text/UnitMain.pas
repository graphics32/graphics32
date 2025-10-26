unit UnitMain;

interface

uses
  {$IFDEF FPC} LCLType, LResources, LMessages, {$ELSE} Winapi.Windows, Winapi.Messages, {$ENDIF}
  Graphics, Controls, Forms, ExtCtrls, Classes,
  GR32_Image;

type
  TForm48 = class(TForm)
    PaintBox32: TPaintBox32;
    TimerRotate: TTimer;
    procedure PaintBox32PaintBuffer(Sender: TObject);
    procedure PaintBox32Click(Sender: TObject);
    procedure TimerRotateTimer(Sender: TObject);
  private
    FAnimationOffset: Single;
  public
  end;

var
  Form48: TForm48;

implementation

{$R *.dfm}

uses
  Types,
  GR32,
  GR32_Math,
  GR32_Paths,
  GR32_Brushes,
  GR32_Polygons,
  GR32_Transforms;

procedure TForm48.PaintBox32Click(Sender: TObject);
begin
  TimerRotate.Enabled := not TimerRotate.Enabled;
end;

procedure TForm48.PaintBox32PaintBuffer(Sender: TObject);
var
  Bitmap: TBitmap32;
  Canvas: TCanvas32;
  FillBrush: TSolidBrush;
  StrokeBrush: TStrokeBrush;
  Transformation: TAffineTransformation;
  i: integer;
  Center: TFloatPoint;
  FontSize: Single;
  Offset: Single;
  Angle: Single;
  Alpha: integer;
  Hue: Single;
const
  Steps = 50;
  MaxSteps = 300;
  MinFontSize = 1;
  MaxFontSize = 72;
  FontSizeStep = (MaxFontSize-MinFontSize) / MaxSteps;
  OffsetStart = 10;
  OffsetStep = 80 / Steps;
begin
  Bitmap := TPaintBox32(Sender).Buffer;

  Bitmap.BeginLockUpdate; // No need to waste time on update notifications

  Bitmap.Font.Style := [fsBold, fsItalic];
  Bitmap.Font.Size := MaxFontSize;

  Bitmap.Clear(0);

  Canvas := TCanvas32.Create(Bitmap);
  try
    FillBrush := TSolidBrush(Canvas.Brushes.Add(TSolidBrush));
    FillBrush.FillMode := pfNonZero;

    // Normally you'd not want a stroke brush for text.
    // We use it here just so we can see the outline of the text.
    StrokeBrush := TStrokeBrush(Canvas.Brushes.Add(TStrokeBrush));
    StrokeBrush.StrokeWidth := 1.0;

    Transformation := TAffineTransformation.Create;
    try

      Canvas.Transformation := Transformation;

      Center := FloatPoint(TPaintBox32(Sender).Width / 2, TPaintBox32(Sender).Height / 2);

      FontSize := MinFontSize;
      Offset := OffsetStart;

      i := MaxSteps;

      while (i > 0) and (FontSize >= MinFontSize) do
      begin

        Angle := FMod(FAnimationOffset * 200 + i / Steps * 360, 360); // [0..360]
        Alpha := Round(255 * i / MaxSteps); // [0..255]
        Hue := FMod(FAnimationOffset * 20 + i / MaxSteps, 1); // [0..1]

        FillBrush.Visible := (Alpha > 16);
        if FillBrush.Visible then
          FillBrush.FillColor := HSLtoRGB(Hue, 0.7, 0.5, Alpha);

        StrokeBrush.Visible := (255 - Alpha > 64);
        if StrokeBrush.Visible then
          StrokeBrush.FillColor := HSLtoRGB(Hue+0.01, 0.7, 0.5, 255 - Alpha);

        Transformation.Clear;

        // Scale font size
        Transformation.Scale(FontSize / MaxFontSize);
        // Offset from center
        Transformation.Translate(Offset, 0);
        // Rotate
        Transformation.Rotate(Angle);
        // Move to center
        Transformation.Translate(Center.X, Center.Y);

        Canvas.RenderText(0, 0, 'Graphics32');

        FontSize := FontSize + FontSizeStep;
        Offset := Offset + OffsetStep;

        Dec(i);
      end;

    finally
      Transformation.Free;
    end;

  finally
    Canvas.Free;
  end;

  Bitmap.EndLockUpdate;
end;

procedure TForm48.TimerRotateTimer(Sender: TObject);
begin
  FAnimationOffset := FMod(FAnimationOffset + 0.0005, 1);
  PaintBox32.Repaint;
end;

end.
