unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, GR32, GR32_Image, Vcl.StdCtrls, GR32_RangeBar;

type
  TBenchmarkThread = class(TThread)
  private
    FPoints: TArrayOfFloatPoint;
    FLineWidth: TFloat;
    FVerticesPerS: Double;
  protected
    procedure Benchmark(LineWidth: TFloat; Count, Repetitions: Integer);
    procedure Execute; override;
  public
    procedure BuildPoints;
    procedure DrawResults;
  end;

  TFrmBuildLineBenchmark = class(TForm)
    Image32: TImage32;
    MemoOutput: TMemo;
    BtnBenchmark: TButton;
    BtnClear: TButton;
    BtnDraw: TButton;
    GaugeBar32: TGaugeBar32;
    LblLineWidth: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BtnBenchmarkClick(Sender: TObject);
    procedure BtnClearClick(Sender: TObject);
    procedure BtnDrawClick(Sender: TObject);
    procedure GaugeBar32Change(Sender: TObject);
    procedure Image32Click(Sender: TObject);
    procedure Image32Resize(Sender: TObject);
  private
    FBenchmarkThread: TBenchmarkThread;
    FLineWidth: TFloat;
    procedure StopBenchmark;
  protected
    procedure DrawPolyline(LineWidth: TFloat; Count: Integer);
  public
  end;

var
  FrmBuildLineBenchmark: TFrmBuildLineBenchmark;

implementation

{$R *.dfm}

uses
  Math, GR32_VectorUtils, GR32_Polygons, GR32_System;

{ TBenchmarkThread }

procedure TBenchmarkThread.Benchmark(LineWidth: TFloat; Count,
  Repetitions: Integer);
var
  Index: Integer;
  NanoSeconds: Double;
  VerticesPerS: Double;
begin
  SetLength(FPoints, Count);

  Synchronize(BuildPoints);

  with TPerfTimer.Create do
  try
    // initial benchmark
    Start;
    BuildPolyline(FPoints, LineWidth, jsRound, esRound);
    NanoSeconds := ReadValue;
    FVerticesPerS := 1000000 * Count / NanoSeconds;

    // repetitions
    for Index := 1 to Repetitions - 1 do
    begin
      Start;
      BuildPolyline(FPoints, LineWidth, jsRound, esRound);
      NanoSeconds := ReadValue;

      VerticesPerS := 1000000 * Count / NanoSeconds;
      if VerticesPerS > FVerticesPerS then
        FVerticesPerS := VerticesPerS;
      if Terminated then Exit;
    end;
  finally
    Free;
  end;

  if Terminated then
    Exit;

  FLineWidth := LineWidth;
  Synchronize(DrawResults);
end;

procedure TBenchmarkThread.BuildPoints;
var
  Index: Integer;
begin
  with FrmBuildLineBenchmark.Image32 do
    for Index := 0 to High(FPoints) do
      FPoints[Index] := FloatPoint(FLineWidth + Random * (Width -
        2 * FLineWidth), FLineWidth + Random * (Height - 2 * FLineWidth));
end;

procedure TBenchmarkThread.DrawResults;
begin
  FrmBuildLineBenchmark.MemoOutput.Lines.Add('LineWidth = ' +
    FloatToStrF(FLineWidth, ffGeneral, 3, 3) + ' - ' +
    FloatToStrF(FVerticesPerS, ffGeneral, 8, 3) + ' Vertices/s');

  with TPolygonRenderer32VPR.Create(FrmBuildLineBenchmark.Image32.Bitmap) do
  try
    Color := SetAlpha(Random($FFFFFF), $A0 + Random($3F));
    PolygonFS(BuildPolyline(FPoints, FLineWidth, jsRound, esRound));
  finally
    Free;
  end;

  FrmBuildLineBenchmark.Image32.Invalidate;
end;

procedure TBenchmarkThread.Execute;
begin
  inherited;

  RandSeed := $7BADAFFE;

  FrmBuildLineBenchmark.MemoOutput.Lines.Add('--- Benchmark Started ---');

  if not Terminated then
    Benchmark(1.5, 100, 9999);
  if not Terminated then
    Benchmark(2.5, 100, 9999);
  if not Terminated then
    Benchmark(3.5, 100, 9999);
  if not Terminated then
    Benchmark(5.1, 100, 9999);
  if not Terminated then
    Benchmark(7.2, 100, 9999);
  if not Terminated then
    Benchmark(10, 100, 9999);
  if not Terminated then
    Benchmark(15, 100, 9999);
end;


{ TFrmBuildLineBenchmark }

procedure TFrmBuildLineBenchmark.FormShow(Sender: TObject);
var
  Index: Integer;
  Points: TArrayOfFloatPoint;
  Angle, IncAngle: TFloat;
begin
  FLineWidth := 10;

  RandSeed := $3BADAFFE;

  with Image32 do
  begin
    SetLength(Points, 3);
(*
    for Index := 0 to 10 do
    begin
      Points[0] := FloatPoint(3 * (Index + 1) * 2 * FLineWidth, 2 * FLineWidth);
      Points[1] := FloatPoint(1.3 * (Index + 1) * 2 * FLineWidth,
        Min(5 * FLineWidth, 0.5 * Height));
      Points[2] := FloatPoint(3 * (Index + 1) * 2 * FLineWidth,
        Min(10 * FLineWidth, Height - 2 * FLineWidth));

      with TPolygonRenderer32VPR.Create(Bitmap) do
      try
        Color := HSLtoRGB(0.2 * Index, 1, 0.5);
        PolygonFS(BuildPolyline(Points, FLineWidth, jsRound, esRound));
      finally
        Free;
      end;
    end;
*)

    for Index := 10 downto 0 do
    begin
(*
      Points[0] := FloatPoint(2 * FLineWidth, 3 * (Index + 1) * 2 * FLineWidth);
      Points[1] := FloatPoint(0.5 * Width, 1.1 * (Index + 1) * 2 * FLineWidth);
      Points[2] := FloatPoint(Width - 2 * FLineWidth, 3 * (Index + 1) * 2 * FLineWidth);

      with TPolygonRenderer32VPR.Create(Bitmap) do
      try
        Color := clBlack32;
        PolygonFS(BuildPolyline(Points, FLineWidth, jsRound, esRound));
      finally
        Free;
      end;
*)
    end;

    SetLength(Points, 20);
    IncAngle := 0.1;
    Angle := 0;
    if Random < 0  then
    begin
      Points[0] := FloatPoint(0.5 * Width - FLineWidth, FLineWidth);
      for Index := 1 to High(Points) do
      begin
        Points[Index] := FloatPoint(Points[Index - 1].X - 1.5 * FLineWidth *
          (0.7 + Index * Cos(Angle)), Points[Index - 1].Y + 1.5 * FLineWidth *
          Index * Sin(Angle));
        Angle := Angle + IncAngle;
        IncAngle := IncAngle + 0.14;
      end;
    end
    else
    begin
      Points[0] := FloatPoint(FLineWidth, FLineWidth);
      for Index := 1 to High(Points) do
      begin
        Points[Index] := FloatPoint(Points[Index - 1].X + 1.5 * FLineWidth *
          (0.7 + Index * Cos(Angle)), Points[Index - 1].Y + 1.5 * FLineWidth *
          Index * Sin(Angle));
        Angle := Angle + IncAngle;
        IncAngle := IncAngle + 0.14;
      end;
    end;

    Points := BuildPolyline(Points, FLineWidth, jsRound, esRound);
    Points := ScalePolygon(Points, 2, 2);

    with TPolygonRenderer32VPR.Create(Bitmap) do
    try
      Color := clBlack32;
      PolygonFS(Points);
    finally
      Free;
    end;

    for Index := 1 to High(Points) do
      Bitmap.PixelFS[Points[Index].X, Points[Index].Y] := clRed32;
  end;
end;

procedure TFrmBuildLineBenchmark.GaugeBar32Change(Sender: TObject);
begin
  FLineWidth := 0.1 * GaugeBar32.Position;
  LblLineWidth.Caption := 'Width: ' + FloatToStrF(FLineWidth, ffGeneral, 3, 3);
end;

procedure TFrmBuildLineBenchmark.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  StopBenchmark;
end;

procedure TFrmBuildLineBenchmark.BtnBenchmarkClick(Sender: TObject);
begin
  StopBenchmark;

  FBenchmarkThread := TBenchmarkThread.Create(True);
  FBenchmarkThread.Priority := tpTimeCritical;
  FBenchmarkThread.Start;
end;

procedure TFrmBuildLineBenchmark.StopBenchmark;
begin
  if Assigned(FBenchmarkThread) then
  begin
    FBenchmarkThread.Terminate;
    FBenchmarkThread.WaitFor;
    FreeAndNil(FBenchmarkThread)
  end;
end;

procedure TFrmBuildLineBenchmark.BtnClearClick(Sender: TObject);
begin
  MemoOutput.Lines.Add('Clearing in progress...');
  StopBenchmark;
  Image32.Bitmap.Clear(SetAlpha(Color32(Self.Color), $FF));
  MemoOutput.Clear;
end;

procedure TFrmBuildLineBenchmark.BtnDrawClick(Sender: TObject);
begin
  DrawPolyline(FLineWidth, 30);
end;

procedure TFrmBuildLineBenchmark.DrawPolyline(LineWidth: TFloat;
  Count: Integer);
var
  Index: Integer;
  Points: TArrayOfFloatPoint;
begin
  with Image32 do
  begin
    SetLength(Points, Count);

    // add points
    for Index := 0 to High(Points) do
      Points[Index] := FloatPoint(LineWidth + Random * (Width - 2 * LineWidth),
        LineWidth + Random * (Height - 2 * LineWidth));

    with TPolygonRenderer32VPR.Create(Bitmap) do
    try
      Color := SetAlpha(Random($FFFFFF), $A0 + Random($3F));
      Points := BuildPolyline(Points, LineWidth, jsRound, esRound);
      PolygonFS(Points);
    finally
      Free;
    end;
  end;
end;

procedure TFrmBuildLineBenchmark.Image32Click(Sender: TObject);
begin
  DrawPolyline(FLineWidth, 30);
end;

procedure TFrmBuildLineBenchmark.Image32Resize(Sender: TObject);
begin
  Image32.SetupBitmap(True, Color32(Self.Color));
end;

end.
