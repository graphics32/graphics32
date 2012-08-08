unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GR32, GR32_Image, GR32_Polygons, GR32_Paths, GR32_Brushes;

type
  TFormBezier = class(TForm)
    PaintBox32: TPaintBox32;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox32PaintBuffer(Sender: TObject);
    procedure PaintBox32DblClick(Sender: TObject);
    procedure PaintBox32MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox32MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox32MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    FCanvas32: TCanvas32;
    FStroke: TStrokeBrush;
    FFill: TSolidBrush;
    FCurrentIndex: Integer;
    FPoints: array [0..5] of TFloatPoint;
  public
    { Public-Deklarationen }
  end;

var
  FormBezier: TFormBezier;

implementation

{$R *.dfm}


{ TFormBezier }

procedure TFormBezier.FormCreate(Sender: TObject);
begin
  FCanvas32 := TCanvas32.Create(PaintBox32.Buffer);
  FStroke := TStrokeBrush.Create(FCanvas32.Brushes);
  FFill := TSolidBrush.Create(FCanvas32.Brushes);

  FPoints[0] := FloatPoint(Random * PaintBox32.Width, Random * PaintBox32.Height);
  FPoints[1] := FloatPoint(Random * PaintBox32.Width, Random * PaintBox32.Height);
  FPoints[2] := FloatPoint(Random * PaintBox32.Width, Random * PaintBox32.Height);
  FPoints[3] := FloatPoint(Random * PaintBox32.Width, Random * PaintBox32.Height);
  FPoints[4] := FloatPoint(Random * PaintBox32.Width, Random * PaintBox32.Height);
  FPoints[5] := FloatPoint(Random * PaintBox32.Width, Random * PaintBox32.Height);

  FCurrentIndex := -1;
  FStroke.StrokeWidth := 2;
  FStroke.FillMode := pfWinding;

  FFill.FillColor := $80000080;
  FFill.FillMode := pfWinding;
  FFill.Visible := False;
end;

procedure TFormBezier.FormDestroy(Sender: TObject);
begin
  FCanvas32.Free;
end;

function CubicInterpolation(const Fractional: TFloat;
  const Data0, Data1, Data2, Data3: TFloat): TFloat;
begin
  Result := Data1 + 0.5 * Fractional * (Data2 - Data0 + Fractional *
    (4 * Data2 + 2 * Data0 - 5 * Data1 - Data3 + Fractional *
    (3 * (Data1 - Data2) - Data0 + Data3)));
end;

procedure TFormBezier.PaintBox32DblClick(Sender: TObject);
begin
  FPoints[0] := FloatPoint(Random * PaintBox32.Width, Random * PaintBox32.Height);
  FPoints[1] := FloatPoint(Random * PaintBox32.Width, Random * PaintBox32.Height);
  FPoints[2] := FloatPoint(Random * PaintBox32.Width, Random * PaintBox32.Height);
  FPoints[3] := FloatPoint(Random * PaintBox32.Width, Random * PaintBox32.Height);
  FPoints[4] := FloatPoint(Random * PaintBox32.Width, Random * PaintBox32.Height);
  FPoints[5] := FloatPoint(Random * PaintBox32.Width, Random * PaintBox32.Height);
  PaintBox32.Invalidate;
end;

procedure TFormBezier.PaintBox32MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
begin
  FCurrentIndex := -1;
  for Index := 0 to Length(FPoints) - 1 do
  begin
    if Sqr(FPoints[Index].X - X) + Sqr(FPoints[Index].Y - Y)  < 25 then
    begin
      FCurrentIndex := Index;
      Exit;
    end;
  end;
end;

procedure TFormBezier.PaintBox32MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if FCurrentIndex >= 0 then
  begin
    FPoints[FCurrentIndex] := FloatPoint(X, Y);
    PaintBox32.Invalidate;
  end;
end;

procedure TFormBezier.PaintBox32MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FCurrentIndex := -1;
end;

procedure TFormBezier.PaintBox32PaintBuffer(Sender: TObject);
var
  Index: Integer;
  Val: Double;
  Fractional: Double;
  Indices: array [0..3] of Integer;
begin
  PaintBox32.Buffer.Clear($FFFFFFFF);

  FStroke.FillColor := $80000080;
  FCanvas32.Path.BeginPath;
  FCanvas32.Path.MoveTo(FPoints[0]);
  for Index := 1 to Length(FPoints) - 1 do
    FCanvas32.Path.LineTo(FPoints[Index]);
  FCanvas32.Path.ClosePath;
  FCanvas32.Path.EndPath;

  FFill.Visible := True;
  FStroke.Visible := False;
  for Index := 0 to Length(FPoints) - 1 do
    FCanvas32.Path.Circle(FPoints[Index].X, FPoints[Index].Y, 5, 32);

  FFill.Visible := False;
  FStroke.Visible := True;
  FStroke.FillColor := $FF000000;
  FCanvas32.Path.BeginPath;
  FCanvas32.Path.MoveTo(FPoints[0]);

  Val := 0;
  while Val < Length(FPoints) do
  begin
    Indices[0] := (Length(FPoints) + Trunc(Val) - 2 + 1) mod Length(FPoints);
    Indices[1] := (Indices[0] + 1) mod Length(FPoints);
    Indices[2] := (Indices[1] + 1) mod Length(FPoints);
    Indices[3] := (Indices[2] + 1) mod Length(FPoints);

    Fractional := Frac(Val);

    FCanvas32.Path.LineTo(
      CubicInterpolation(Fractional, FPoints[Indices[0]].X,
        FPoints[Indices[1]].X, FPoints[Indices[2]].X, FPoints[Indices[3]].X),
      CubicInterpolation(Fractional, FPoints[Indices[0]].Y,
        FPoints[Indices[1]].Y, FPoints[Indices[2]].Y, FPoints[Indices[3]].Y));
    Val := Val + 0.05;
  end;
  FCanvas32.Path.ClosePath;
  FCanvas32.Path.EndPath;
end;

end.
