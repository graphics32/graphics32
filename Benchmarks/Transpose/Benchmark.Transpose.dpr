program Benchmark.Transpose;

{$APPTYPE CONSOLE}

{$I GR32.inc}

uses
  SysUtils,
  Spring.Benchmark,
  GR32,
  GR32_Bindings,
  GR32_LowLevel,
  GR32.Transpose;

//------------------------------------------------------------------------------

procedure BenchmarkTranspose(const state: TState);
begin
  var Transposer := TTranspose32(state[0]);
  var Width := state[1];
  var Height := state[2];

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

procedure NoTransposeMoveLongword(Src, Dst: Pointer; SrcWidth, SrcHeight: integer);
begin
  MoveLongword(Src^, Dst^, SrcWidth * SrcHeight);
end;

procedure NoTransposeMove(Src, Dst: Pointer; SrcWidth, SrcHeight: integer);
begin
  Move(Src^, Dst^, SrcWidth * SrcHeight*SizeOf(TColor32));
end;

//------------------------------------------------------------------------------

procedure Main;
begin
  Spring.Benchmark.benchmark_format_args := False;

  TransposeRegistry[@@_Transpose32].Add(@NoTransposeMoveLongword, [isReference]).Name := 'MoveLongword (no transpose)';
  TransposeRegistry[@@_Transpose32].Add(@NoTransposeMove, [isReference]).Name := 'Move (no transpose)';

  var Binding := TransposeRegistry.FindBinding('_Transpose32');
  Assert(Binding <> nil);

  const Sizes: array of integer = [256, 512, 1024, 2048, 4096];

  for var Width in Sizes do
  for var Height in Sizes do
  begin

    for var Implement in Binding do
    begin
      var bm := Spring.Benchmark.Benchmark(BenchmarkTranspose, Implement.Name+Format('/%d+1 x %d', [Width, Height]));

      bm.Args([Int64(Implement.Proc), Width+1, Height]);

      bm.TimeUnit(kMillisecond);
    end;
  end;

  Spring.Benchmark.Benchmark_Main;
end;

begin
  try
    Main;
    WriteLn('Done');
    ReadLn;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

  Spring.Benchmark.Benchmark_Main;
end.

