unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GR32, GR32_Image, GR32_Polygons, GR32_ColorGradients;

type
  TFrmAntiAliasingTest = class(TForm)
    PaintBox32: TPaintBox32;
    procedure PaintBox32PaintBuffer(Sender: TObject);
  public
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
  Math, GR32_VectorUtils;

procedure TFrmAntiAliasingTest.PaintBox32PaintBuffer(Sender: TObject);
var
  W, H: Integer;
  Center: TFloatPoint;
  Offset, Angle: TFloatPoint;
  Points: TArrayOfFloatPoint;
  Outline: TArrayOfFloatPoint;
  MinCenter: TFloat;
  Temp: TFloat;
  Index: Integer;
  LinGrad: TLinearGradientLookupTablePolygonFiller;
const
  CDeg2Rad = Pi / 180;

  function ArrayOfFloat(Values: array of TFloat): TArrayOfFloat;
  var
    Index: Integer;
  begin
    SetLength(Result, Length(Values));
    for Index := 0 to High(Values) do
      Result[Index] := Values[Index];
  end;

  function IndexToColor32: TColor32;
  begin
    Result := Color32($FF * (Index mod 2), ($FF * (Index mod 3)) shr 1,
      ($FF * (Index mod 5)) shr 2);
  end;

begin
  Paintbox32.Buffer.Clear($FF000000);

  W := Paintbox32.Width;
  H := Paintbox32.Height;
  Center.X := 0.5 * W;
  Center.Y := 0.5 * H;

  Angle.X := 0;
  Angle.Y := 1;
  MinCenter := Min(Center.X, Center.Y);
  SinCos(2 * CDeg2Rad, Offset.X, Offset.Y);
  Offset.X := -Offset.X;

  SetLength(Points, 2);

  for Index := 180 downto 1 do
  begin
    Points[0] := FloatPoint(Center.X + MinCenter * Angle.X,
      Center.Y + MinCenter * Angle.Y);
    Points[1] := FloatPoint(Center.X, Center.Y);

    if Index < 90 then
      DashLineFS(Paintbox32.Buffer, Points, ArrayOfFloat([Index, Index]),
        $33FFFFFF)
    else
      PolyLineFS(Paintbox32.Buffer, Points, $33FFFFFF);

    Temp := Angle.Y * Offset.Y - Angle.X * Offset.X;
    Angle.X := Angle.X * Offset.Y + Angle.Y * Offset.X;
    Angle.Y := Temp;
  end;

  // Top patterns
  LinGrad := TLinearGradientLookupTablePolygonFiller.Create;
  LinGrad.Gradient.StartColor := clWhite32;
  for Index := 1 to 20 do
  begin
    // integral point sizes 1..20
    Outline := Circle(20.5 + Index * (Index + 1), 20.5, 0.5 * Index, 8 + Index);
    PolygonFS(Paintbox32.Buffer, Outline, clWhite32);

    // fractional point sizes 0..2
    Outline := Circle(18.5 + 4 * Index, 33.5, 0.05 * Index, 8);
    PolygonFS(Paintbox32.Buffer, Outline, clWhite32);

    // fractional point positioning
    Outline := Circle(18.4 + 4.1 * Index, 27.4 + 0.1 * Index, 0.5, 8);
    PolygonFS(Paintbox32.Buffer, Outline, clWhite32);

    Points[0] := FloatPoint(20 + Index * (Index + 1), 40.5);
    Points[1] := FloatPoint(20 + Index * (Index + 1) + (Index - 1) * 4, 100.5);
    LinGrad.SimpleGradient(Points[0], clWhite32, Points[1], IndexToColor32);
    PolyLineFS(Paintbox32.Buffer, Points, LinGrad, False, Index, jsRound,
      esRound);

    // fractional line lengths H (red/blue)
    LinGrad.Gradient.StartColor := $FF0000FF;
    LinGrad.Gradient.EndColor := $FFFF0000;

    Points[0] := FloatPoint(17.5 + 4 * Index, 107);
    Points[1] := FloatPoint(17.5 + 4.15 * Index, 107);
    LinGrad.SetPoints(Points[0], Points[1]);
    PolyLineFS(Paintbox32.Buffer, Points, LinGrad);

    // fractional line lengths V (red/blue)
    Points[0] := FloatPoint(18 + 4 * Index, 112.5);
    Points[1] := FloatPoint(18 + 4.15 * Index, 112.5 + Index * 0.15);
    LinGrad.SetPoints(Points[0], Points[1]);
    PolyLineFS(Paintbox32.Buffer, Points, LinGrad);

    // fractional line positioning (red)
    Points[0] := FloatPoint(21.5, 120 + (Index - 1) * 3.1);
    Points[1] := FloatPoint(52.5, 120 + (Index - 1) * 3.1);
    LinGrad.SimpleGradient(Points[0], $FFFF0000, Points[1], clWhite32);
    PolyLineFS(Paintbox32.Buffer, Points, LinGrad);

    // fractional line width 2..0 (green)
    Points[0] := FloatPoint(52.5, 118 + Index * 3);
    Points[1] := FloatPoint(83.5, 118 + Index * 3);
    LinGrad.SimpleGradient(Points[0], $FF00FF00, Points[1], clWhite32);
    PolyLineFS(Paintbox32.Buffer, Points, LinGrad, False,
      2.0 - (Index - 1) * 0.1, jsRound, esRound);

    // stippled fractional width 2..0 (blue)
    Points[0] := FloatPoint(83.5, 119 + Index * 3);
    Points[1] := FloatPoint(114.5, 119 + Index * 3);
    LinGrad.SimpleGradient(Points[0], $FF0000FF, Points[1], clWhite32);
    DashLineFS(Paintbox32.Buffer, Points, ArrayOfFloat([3, 3]), LinGrad,
      False, 2.0 - (Index - 1) * 0.1);

    // integral line width, horz aligned (mipmap test)
    if Index < 10 then
    begin
      Points[0] := FloatPoint(125.5, 119.5 + (Index + 2) * (Index * 0.5));
      Points[1] := FloatPoint(135.5, 119.5 + (Index + 2) * (Index * 0.5));
      PolyLineFS(Paintbox32.Buffer, Points, clWhite32, False, Index, jsRound,
        esRound);
    end;

    // fractional line width 0..2, 1 px H
    Points[0] := FloatPoint(17.5 + 4 * Index, 192);
    Points[1] := FloatPoint(18.5 + 4 * Index, 192);
    PolyLineFS(Paintbox32.Buffer, Points, clWhite32, False, 0.1 * Index);

    // fractional line positioning, 1 px H
    Points[0] := FloatPoint(17.5 + 4.1 * Index - 0.1, 186);
    Points[1] := FloatPoint(18.5 + 4.1 * Index - 0.1, 186);
    PolyLineFS(Paintbox32.Buffer, Points, clWhite32);
  end;

  // Triangles
  SetLength(Points, 3);
  for Index := 1 to 13 do
  begin
    Points[0] := FloatPoint(W - 150, H - 20 - Index * (Index + 1.5));
    Points[1] := FloatPoint(W - 20, H - 20 - Index * (Index + 1));
    Points[2] := FloatPoint(W - 20, H - 20 - Index * (Index + 2));

    LinGrad.SimpleGradient(Points[0], clWhite32, Points[1], IndexToColor32);

    PolygonFS(Paintbox32.Buffer, Points, LinGrad);
  end;
end;

initialization
  SetGamma(1 / 1.6);

end.
