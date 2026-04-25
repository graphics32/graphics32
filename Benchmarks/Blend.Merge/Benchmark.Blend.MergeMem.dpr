program Benchmark.Blend.MergeMem;

{$APPTYPE CONSOLE}

{$I GR32.inc}

uses
  SysUtils,
  Spring.Benchmark,
  GR32,
  GR32_Blend,
  GR32.Blend.Pascal,
  GR32.Blend.Assembler,
  GR32.Blend.SSE2;

procedure BM_BlendMem(const state: TState);
begin
  var BlendMemProc := TBlendMem(state[0]);

  for var _ in state do
  begin
    for var F := 0 to $2FFF do
    begin
      var ColorFG: TColor32 := F or (F shl 16);
      for var B := 0 to $FF do
      begin
        var ColorBG: TColor32 := B or (B shl 8) or (B shl 16);
        BlendMemProc(ColorFG, ColorBG);
      end;
    end;
  end;
end;

procedure BM_BlendReg(const state: TState);
begin
  var BlendRegProc: TBlendReg := TBlendReg(state[0]);

  for var _ in state do
  begin
    for var F := 0 to $2FFF do
    begin
      var ColorFG: TColor32 := F or (F shl 16);
      for var B := 0 to $FF do
      begin
        var ColorBG: TColor32 := B or (B shl 8) or (B shl 16);
        BlendRegProc(ColorFG, ColorBG);
      end;
    end;
  end;
end;


procedure Main;
begin
  Spring.Benchmark.benchmark_format_args := False;

  var Binding := BlendRegistry.FindBinding('MergeMem');
  Assert(Binding <> nil);

  for var Implement in Binding do
  begin
    var bm := Spring.Benchmark.Benchmark(BM_BlendMem, Implement.Name).Args([Int64(Implement.Proc)]);
    bm.Iterations(1000);
    bm.TimeUnit(kMillisecond);
  end;

  Binding := BlendRegistry.FindBinding('MergeReg');
  Assert(Binding <> nil);

  for var Implement in Binding do
  begin
    var bm := Spring.Benchmark.Benchmark(BM_BlendReg, Implement.Name).Args([Int64(Implement.Proc)]);
    bm.Iterations(1000);
    bm.TimeUnit(kMillisecond);
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
end.

