program Benchmark.Transpose;

{$APPTYPE CONSOLE}

{$I GR32.inc}

uses
  Spring.Benchmark,
  GR32,
  GR32.Transpose;

//------------------------------------------------------------------------------

procedure BenchmarkTranspose(Transposer: TTranspose32; const state: TState);
begin
  var Width := state[0];
  var Height := state[1];

  var BitmapSource := TBitmap32.Create;
  var BitmapDest := TBitmap32.Create;

  BitmapSource.SetSize(Width, Height);
  BitmapDest.SetSize(Height, Width);

  for var _ in state do
  begin
    Transposer(BitmapSource.Bits, BitmapDest.Bits, Width, Height);
  end;

  state.Counters['Rate'] := Counter(Width*Height, [kIsRate, kIsIterationInvariant]);

  BitmapSource.Free;
  BitmapDest.Free;
end;

//------------------------------------------------------------------------------

procedure BenchmarkReferenceTranspose32(const state: TState);
begin
  BenchmarkTranspose(@ReferenceTranspose32, state);
end;

procedure BenchmarkCacheObliviousTranspose32(const state: TState);
begin
  BenchmarkTranspose(@CacheObliviousTranspose32, state);
end;

procedure BenchmarkSuperDuperTranspose32(const state: TState);
begin
  BenchmarkTranspose(@SuperDuperTranspose32, state);
end;

//------------------------------------------------------------------------------

procedure Benchmark(BenchmarkFunc: TFunction; const Name: string);
begin
  Spring.Benchmark.Benchmark(BenchmarkFunc, Name).RangeMultiplier(4).Ranges([Range(1024+1, 8192+13), Range(128, 5120)]).TimeUnit(kMillisecond);
end;

//------------------------------------------------------------------------------

begin
  Benchmark(BenchmarkReferenceTranspose32, 'ReferenceTranspose32');
  Benchmark(BenchmarkCacheObliviousTranspose32, 'CacheObliviousTranspose32');
  Benchmark(BenchmarkSuperDuperTranspose32, 'SuperDuperTranspose32');

  Spring.Benchmark.Benchmark_Main;
end.

