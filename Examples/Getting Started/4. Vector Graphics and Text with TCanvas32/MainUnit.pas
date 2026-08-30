unit MainUnit;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GR32, GR32_Image, GR32_Paths, GR32_Brushes, GR32_ColorGradients, GR32_Polygons;

type
  TFormCanvas32 = class(TForm)
    Image32: TImage32;
    procedure FormCreate(Sender: TObject);
  public
    procedure DrawGradientText(Bitmap: TBitmap32);
  end;

var
  FormCanvas32: TFormCanvas32;

implementation

{$R *.dfm}

procedure TFormCanvas32.DrawGradientText(Bitmap: TBitmap32);
var
  Canvas: TCanvas32;
  FillBrush: TSolidBrush;
  StrokeBrush: TStrokeBrush;
  Filler: TLinearGradientPolygonFiller;
begin
  Bitmap.SetSize(400, 150);
  Bitmap.Clear(Color32(30, 30, 30, 255));

  // Configure font settings on the target bitmap
  Bitmap.Font.Name := 'Cooper Black';
  Bitmap.Font.Size := 40;

  Canvas := TCanvas32.Create(Bitmap);
  try
    // 1. Configure solid fill brush with a linear gradient filler
    FillBrush := TSolidBrush(Canvas.Brushes.Add(TSolidBrush));
    FillBrush.FillMode := pfNonZero;

    Filler := TLinearGradientPolygonFiller.Create;
    try
      Filler.SimpleGradient(FloatPoint(0, 45), Color32(255, 120, 0),
                            FloatPoint(0, 95), Color32(255, 0, 128));
      FillBrush.Filler := Filler;

      // 2. Configure stroke brush for white outline
      StrokeBrush := TStrokeBrush.Create(Canvas.Brushes);
      StrokeBrush.FillColor := clWhite32;
      StrokeBrush.StrokeWidth := 1.5;

      // Render vector text using both brushes on TCanvas32
      Canvas.RenderText(20, 45, 'Graphics32');
    finally
      Filler.Free;
    end;
  finally
    Canvas.Free;
  end;
end;

procedure TFormCanvas32.FormCreate(Sender: TObject);
begin
  Image32.SetupBitmap;
  DrawGradientText(Image32.Bitmap);
end;

end.
