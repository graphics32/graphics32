program Benchmark.Blur;

{$APPTYPE CONSOLE}

{$I GR32.inc}

uses
  SysUtils,
  Spring.Benchmark,
  GR32,
  GR32_LowLevel,
  GR32_Blurs,
  GR32.Blur;

//------------------------------------------------------------------------------

procedure BenchmarkBlur(Blur: TBlur32Proc; const state: TState); overload;
begin
  var Width: integer:= state[0];
  var Height: integer := state[1];
  var Radius: TFloat := state[2];

  var BitmapSource := TBitmap32.Create;
  var BitmapDest := TBitmap32.Create;

  BitmapSource.SetSize(Width, Height);
  BitmapDest.SetSize(Width, Height);

  for var _ in state do
  begin
    Blur(BitmapSource, BitmapDest, Radius);
  end;

  state.Counters['Rate'] := Counter(Width*Height, [kIsRate, kIsIterationInvariant]);

  BitmapSource.Free;
  BitmapDest.Free;
end;

type
  TBitmap32Cracker = class(TCustomBitmap32);

procedure BenchmarkBlur(Blur: TBlurInplace32Proc; const state: TState); overload;
begin
  var Width: integer:= state[0];
  var Height: integer := state[1];
  var Radius: TFloat := state[2];

  var BitmapSource := TBitmap32.Create;

  BitmapSource.SetSize(Width, Height);

  for var _ in state do
  begin
    Blur(BitmapSource, Radius);
  end;

  state.Counters['Rate'] := Counter(Width*Height, [kIsRate, kIsIterationInvariant]);

  BitmapSource.Free;
end;

//------------------------------------------------------------------------------

procedure NoBlur(ASource, ADest: TBitmap32; Radius: TFloat);
begin
  TBitmap32Cracker(ASource).CopyMapTo(ADest);
end;

//------------------------------------------------------------------------------

procedure BenchmarkNoBlur32(const state: TState);
begin
  BenchmarkBlur(NoBlur, state);
end;

procedure BenchmarkBlur32(const state: TState);
begin
  if (Assigned(Blur32Proc)) then
    BenchmarkBlur(Blur32Proc, state)
  else
  if (Assigned(BlurInplace32Proc)) then
    BenchmarkBlur(BlurInplace32Proc, state)
  else
    raise Exception.Create('Missing Blur32 implementation');
end;

procedure BenchmarkOldGaussianBlur(const state: TState);
begin
  BenchmarkBlur(GaussianBlur, state);
end;

procedure BenchmarkOldFastBlur(const state: TState);
begin
  BenchmarkBlur(FastBlur, state);
end;

//------------------------------------------------------------------------------

const
  Sizes: array of integer = [256, 1024, 4096, 8192];
  Radii: array of integer = [2, 8, 32, 64];
  Implementations: array[0..3] of
    record
      Proc: TFunction;
      Name: string;
      Slow: boolean;
    end = (
      (Proc: BenchmarkNoBlur32; Name: 'MemCopy (no blur)'; Slow: False),
      (Proc: BenchmarkBlur32; Name: 'Blur32'; Slow: False),
      (Proc: BenchmarkOldGaussianBlur; Name: 'GaussianBlur'; Slow: True),
      (Proc: BenchmarkOldFastBlur; Name: 'FastBlur'; Slow: False)
    );

procedure Main;
begin
  Spring.Benchmark.benchmark_format_args := False;

  for var Width in Sizes do
  for var Height in Sizes do
  for var Radius in Radii do
  for var Implement in Implementations do
  begin
    if (Implement.Slow) and ((Width > 2048) or (Height > 2048) or (Radius > 32)) then
      continue;

    var bm := Spring.Benchmark.Benchmark(Implement.Proc, Implement.Name+Format('/%d x %d, radius: %d', [Width, Height, Radius]));

    bm.Args([Width, Height, Radius]);

    bm.TimeUnit(kMillisecond);
  end;

  Spring.Benchmark.Benchmark_Main;
end;

//------------------------------------------------------------------------------

begin
  try
    Main;
    WriteLn('Done');
    ReadLn;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

