unit MainUnit;

{$I GR32.inc}

interface

uses
  {$IFNDEF FPC}Windows, {$ELSE} LCLIntf, LCLType, LMessages, {$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GR32, GR32_Image, GR32_Polygons, GR32_ColorGradients;

type
  TFrmAntiAliasingTest = class(TForm)
    PaintBox32: TPaintBox32;
    procedure PaintBox32PaintBuffer(Sender: TObject);
  end;

var
  FrmAntiAliasingTest: TFrmAntiAliasingTest;

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
  GR32_Gamma,
  GR32_VectorUtils;

procedure TFrmAntiAliasingTest.PaintBox32PaintBuffer(Sender: TObject);

  function ArrayOfFloat(Values: array of TFloat): TArrayOfFloat;
  var
    Index: Integer;
  begin
    SetLength(Result, Length(Values));
    for Index := 0 to High(Values) do
      Result[Index] := Values[Index];
  end;

  function IndexToColor32(Index: Integer): TColor32;
  begin
    Result := Color32(
       $FF * (Index mod 2),         // red
      ($FF * (Index mod 3)) shr 1,  // green
      ($FF * (Index mod 5)) shr 2   // blue
    );
  end;

var
  W, H: Integer;
  Center: TFloatPoint;
  Points: TArrayOfFloatPoint;
  Outline: TArrayOfFloatPoint;
  Temp, MinCenter: TFloat;
  Index: Integer;
  LinGrad: TLinearGradientPolygonFiller;
  Renderer: TPolygonRenderer32;

const
  CDeg2Rad = Pi / 180;

  procedure DrawCircleLine;
  var
    Index: Integer;
    Offset, Angle: TFloatPoint;
  begin
    // setup an efficient quadrature oscillator algorithm
    Angle.X := 0;
    Angle.Y := 1;
    GR32_Math.SinCos(2 * CDeg2Rad, Offset.X, Offset.Y);
    Offset.X := -Offset.X;

    // set color
    Renderer.Color := $33FFFFFF;

    // keep the same center
    Points[0] := FloatPoint(Center.X, Center.Y);

    for Index := 180 downto 1 do
    begin
      // specify line end around the circle
      Points[1] := FloatPoint(Center.X + MinCenter * Angle.X,
        Center.Y + MinCenter * Angle.Y);

      if Index < 90 then
        Renderer.PolyPolygonFS(BuildPolyPolyline(BuildDashedLine(Points,
          ArrayOfFloat([Index, Index])), False, 1))
      else
        Renderer.PolygonFS(BuildPolyline(Points, 1));

      Temp := Angle.Y * Offset.Y - Angle.X * Offset.X;
      Angle.X := Angle.X * Offset.Y + Angle.Y * Offset.X;
      Angle.Y := Temp;
    end;
  end;

  procedure DrawTriangles;
  var
    Index: Integer;
  begin
    // Triangles
    SetLength(Points, 3);
    Renderer.Filler := LinGrad;
    for Index := 1 to 13 do
    begin
      Points[0] := FloatPoint(W - 150, H - 20 - Index * (Index + 1.5));
      Points[1] := FloatPoint(W -  20, H - 20 - Index * (Index + 1));
      Points[2] := FloatPoint(W -  20, H - 20 - Index * (Index + 2));

      LinGrad.SimpleGradient(Points[0], clWhite32, Points[1], IndexToColor32(Index));
      Renderer.PolygonFS(Points);
    end;
  end;

begin
  Paintbox32.Buffer.Clear($FF000000);

  // cache width (W) and height (H) for faster access
  W := Paintbox32.Width;
  H := Paintbox32.Height;

  // calculate the center point for a faster access
  Center.X := 0.5 * W;
  Center.Y := 0.5 * H;

  // initialize some variables for a fast quadrature oscillator algorithm
  MinCenter := Min(Center.X, Center.Y);

  SetLength(Points, 2);

  Renderer := TPolygonRenderer32VPR.Create;
  Renderer.Bitmap := PaintBox32.Buffer;
  try
    DrawCircleLine;

    // Top patterns
    LinGrad := TLinearGradientPolygonFiller.Create;
    LinGrad.Gradient.StartColor := clWhite32;
    for Index := 1 to 20 do
    begin
      Renderer.Color := clWhite32;
      Renderer.Filler := nil;

      // integral point sizes 1..20
      Outline := Circle(20.5 + Index * (Index + 1), 20.5, 0.5 * Index, 8 + Index);
      Renderer.PolygonFS(Outline);

      // fractional point sizes 0..2
      Outline := Circle(18.5 + 4 * Index, 33.5, 0.05 * Index, 8);
      Renderer.PolygonFS(Outline);

      // fractional point positioning
      Outline := Circle(18.4 + 4.1 * Index, 27.4 + 0.1 * Index, 0.5, 8);
      Renderer.PolygonFS(Outline);

      Renderer.Filler := LinGrad;
      Points[0] := FloatPoint(20 + Index * (Index + 1), 40.5);
      Points[1] := FloatPoint(20 + Index * (Index + 1) + (Index - 1) * 4, 100.5);
      LinGrad.SimpleGradient(Points[0], clWhite32, Points[1], IndexToColor32(Index));
      Outline := BuildPolyline(Points, Index, jsRound, esRound);
      Renderer.PolygonFS(Outline);

      // fractional line lengths H (red/blue)
      LinGrad.Gradient.StartColor := $FF0000FF;
      LinGrad.Gradient.EndColor := $FFFF0000;

      Points[0] := FloatPoint(17.5 + 4 * Index, 107);
      Points[1] := FloatPoint(17.5 + 4.15 * Index, 107);
      LinGrad.SetPoints(Points[0], Points[1]);
      Renderer.PolygonFS(BuildPolyline(Points, 1));

      // fractional line lengths V (red/blue)
      Points[0] := FloatPoint(18 + 4 * Index, 112.5);
      Points[1] := FloatPoint(18 + 4.15 * Index, 112.5 + Index * 0.15);
      LinGrad.SetPoints(Points[0], Points[1]);
      Renderer.PolygonFS(BuildPolyline(Points, 1));

      // fractional line positioning (red)
      Points[0] := FloatPoint(21.5, 120 + (Index - 1) * 3.1);
      Points[1] := FloatPoint(52.5, 120 + (Index - 1) * 3.1);
      LinGrad.SimpleGradient(Points[0], $FFFF0000, Points[1], clWhite32);
      Renderer.PolygonFS(BuildPolyline(Points, 1));

      // fractional line width 2..0 (green)
      Points[0] := FloatPoint(52.5, 118 + Index * 3);
      Points[1] := FloatPoint(83.5, 118 + Index * 3);
      LinGrad.SimpleGradient(Points[0], $FF00FF00, Points[1], clWhite32);
      Outline := BuildPolyline(Points, 2 - (Index - 1) * 0.1, jsRound, esRound);
      Renderer.PolygonFS(Outline);

      // stippled fractional width 2..0 (blue)
      Points[0] := FloatPoint(83.5, 119 + Index * 3);
      Points[1] := FloatPoint(114.5, 119 + Index * 3);
      LinGrad.SimpleGradient(Points[0], $FF0000FF, Points[1], clWhite32);
      Renderer.PolyPolygonFS(BuildPolyPolyline(BuildDashedLine(Points,
        ArrayOfFloat([3, 3])), False, 2 - (Index - 1) * 0.1));

      Renderer.Color := clWhite32;
      Renderer.Filler := nil;

      // integral line width, horz aligned (mipmap test)
      if Index < 10 then
      begin
        Points[0] := FloatPoint(125.5, 119.5 + (Index + 2) * (Index * 0.5));
        Points[1] := FloatPoint(135.5, 119.5 + (Index + 2) * (Index * 0.5));
        Outline := BuildPolyline(Points, Index, jsRound, esRound);
        Renderer.PolygonFS(Outline);
      end;

      // fractional line positioning, 1 px H
      Points[0] := FloatPoint(17.5 + 4.1 * Index - 0.1, 186);
      Points[1] := FloatPoint(18.5 + 4.1 * Index - 0.1, 186);
      Renderer.PolygonFS(BuildPolyline(Points, 1));

      // fractional line width 0..2, 1 px H
      Points[0] := FloatPoint(17.5 + 4 * Index, 192);
      Points[1] := FloatPoint(18.5 + 4 * Index, 192);
      Renderer.PolygonFS(BuildPolyline(Points, 0.1 * Index));
    end;

    DrawTriangles;
  finally
    Renderer.Free;
  end;
end;

end.
