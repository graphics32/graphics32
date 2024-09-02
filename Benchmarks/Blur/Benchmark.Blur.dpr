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

procedure Benchmark(BenchmarkFunc: TFunction; const Name: string);
begin
  Spring.Benchmark.Benchmark(BenchmarkFunc, Name).RangeMultiplier(8).Ranges([Range(16, 4096), Range(16, 4096), Range(4, 64)]).TimeUnit(kMillisecond);
end;

//------------------------------------------------------------------------------

begin
  Benchmark(BenchmarkNoBlur32, 'MemCopy (no blur)');
  Benchmark(BenchmarkBlur32, 'Blur32');
  Benchmark(BenchmarkOldGaussianBlur, 'GaussianBlur');
  Benchmark(BenchmarkOldFastBlur, 'FastBlur');

  Spring.Benchmark.Benchmark_Main;
end.

