program Benchmark.Blend.Generic;

{$APPTYPE CONSOLE}

{$I GR32.inc}

uses
  SysUtils,
  StrUtils,
  Spring.Benchmark,
  GR32,
  GR32_Blend,
  GR32.Blend.Pascal,
  GR32.Blend.Assembler,
  GR32.Blend.SSE2;

//------------------------------------------------------------------------------
// TBlendMem
//------------------------------------------------------------------------------
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
        var ColorBG: TColor32 := B or (B shl 8) or (B shl 16) or (B shl 24);
        BlendMemProc(ColorFG, ColorBG);
      end;
    end;
  end;
end;

procedure BM_BlendMemExit(const state: TState);
begin
  var BlendMemProc := TBlendMem(state[0]);

  for var _ in state do
  begin
    for var F_A: byte in [0, $80, $FF] do
    for var F := 0 to $2FF do
    begin
      var ColorFG: TColor32 := F or (F shl 16) or (F_A shl 24);
      for var B_A: byte in [0, $80, $FF] do
      for var B := 0 to $FF do
      begin
        var ColorBG: TColor32 := B or (B shl 8) or (B shl 16) or (B_A shl 24);
        BlendMemProc(ColorFG, ColorBG);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
// TBlendReg
//------------------------------------------------------------------------------
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
        var ColorBG: TColor32 := B or (B shl 8) or (B shl 16) or (B shl 24);
        BlendRegProc(ColorFG, ColorBG);
      end;
    end;
  end;
end;

procedure BM_BlendRegExit(const state: TState);
begin
  var BlendRegProc: TBlendReg := TBlendReg(state[0]);

  for var _ in state do
  begin
    for var F_A: byte in [0, $80, $FF] do
    for var F := 0 to $2FF do
    begin
      var ColorFG: TColor32 := F or (F shl 16) or (F_A shl 24);
      for var B_A: byte in [0, $80, $FF] do
      for var B := 0 to $FF do
      begin
        var ColorBG: TColor32 := B or (B shl 8) or (B shl 16) or (B_A shl 24);
        BlendRegProc(ColorFG, ColorBG);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
// TBlendLine
//------------------------------------------------------------------------------
procedure BM_BlendLine(const state: TState);
const
  Count = $3000;
begin
  var BlendLineProc := TBlendLine(state[0]);

  var ColorFG: PColor32Array;
  GetMem(ColorFG, Count*SizeOf(TColor32));

  for var F := 0 to Count-1 do
    ColorFG[F] := TColor32(F or (F shl 16));

  var ColorBG: PColor32Array;
  GetMem(ColorBG, Count*SizeOf(TColor32));

  for var B := 0 to Count-1 do
    ColorBG[B] := TColor32(($FFFF-B) or (($FFFF-B) shl 16));

  for var _ in state do
  begin
    for var n := 0 to 999 do
      BlendLineProc(PColor32(ColorFG), PColor32(ColorBG), Count);
  end;

  FreeMem(ColorBG);
  FreeMem(ColorFG);
end;

procedure BM_BlendLineExit(const state: TState);
const
  Count = $3000;
begin
  var BlendLineProc := TBlendLine(state[0]);

  var ColorFG: PColor32Array;
  GetMem(ColorFG, Count*SizeOf(TColor32));

  for var F := 0 to Count-1 do
    ColorFG[F] := TColor32(F or (F shl 16));

  var ColorBG: PColor32Array;
  GetMem(ColorBG, Count*SizeOf(TColor32));

  for var B := 0 to Count-1 do
    ColorBG[B] := TColor32(($FFFF-B) or (($FFFF-B) shl 16));

  for var _ in state do
  begin
    for var F_A: byte in [0, $80, $FF] do
      for var B_A: byte in [0, $80, $FF] do
      begin
        state.PauseTiming;
        begin

          for var F := 0 to Count-1 do
            ColorFG[F] := ColorFG[F] and $00FFFFFF or (F_A shl 24);

          for var B := 0 to Count-1 do
            ColorBG[B] := ColorBG[B] and $00FFFFFF or (B_A shl 24);

        end;
        state.ResumeTiming;

        BlendLineProc(PColor32(ColorFG), PColor32(ColorBG), Count);
      end;
  end;

  FreeMem(ColorBG);
  FreeMem(ColorFG);
end;

//------------------------------------------------------------------------------
// TBlendReg (F is color, B is weight)
//------------------------------------------------------------------------------
procedure BM_BlendRegWeight(const state: TState);
begin
  var BlendRegProc: TBlendReg := TBlendReg(state[0]);

  for var _ in state do
  begin
    for var F := 0 to $2FFF do
    begin
      var ColorFG: TColor32 := F or (F shl 16);
      for var B := 0 to $FF do
      begin
        BlendRegProc(ColorFG, B);
      end;
    end;
  end;
end;


procedure Main;
begin
  Spring.Benchmark.benchmark_format_args := False;
  Spring.Benchmark.benchmark_strict_parameters := False; // We need to be able to handle our own command line parameters

  var MinTime: integer := 10;
  var s: string;
  if (FindCmdLineSwitch('benchmark', s, True, [clstValueAppended])) then
    MinTime := StrToIntDef(s, MinTime);

  var Target: string := '';
  FindCmdLineSwitch('target', Target, True, [clstValueAppended]);

  (*
  ** BlendMem/MergeMem
  *)
  for var Name in ['BlendMem', 'MergeMem'] do
  begin
    if (Target <> '') and (not SameText(Target, Name)) then
      continue;

    var Binding := BlendRegistry.FindBinding(Name);
    Assert(Binding <> nil);

    for var Implement in Binding do
    begin
      var bm := Spring.Benchmark.Benchmark(BM_BlendMem, Implement.Name).Args([Int64(Implement.Proc)]);
      bm.MinTime(MinTime); // seconds
      bm.TimeUnit(kMillisecond);
    end;

    for var Implement in Binding do
    begin
      var bm := Spring.Benchmark.Benchmark(BM_BlendMemExit, Implement.Name+'(early exit)').Args([Int64(Implement.Proc)]);
      bm.MinTime(MinTime); // seconds
      bm.TimeUnit(kMillisecond);
    end;
  end;

  (*
  ** BlendLine/MergeLine
  *)
  for var Name in ['BlendLine', 'MergeLine'] do
  begin
    if (Target <> '') and (not SameText(Target, Name)) then
      continue;

    var Binding := BlendRegistry.FindBinding(Name);
    Assert(Binding <> nil);

    for var Implement in Binding do
    begin
      var bm := Spring.Benchmark.Benchmark(BM_BlendLine, Implement.Name).Args([Int64(Implement.Proc)]);
      bm.MinTime(MinTime); // seconds
      bm.TimeUnit(kMillisecond);
    end;

    for var Implement in Binding do
    begin
      var bm := Spring.Benchmark.Benchmark(BM_BlendLineExit, Implement.Name+'(early exit)').Args([Int64(Implement.Proc)]);
      bm.MinTime(MinTime); // seconds
      bm.TimeUnit(kMillisecond);
    end;
  end;

  (*
  ** BlendReg/MergeReg
  *)
  for var Name in [
    'BlendReg', 'MergeReg',
    'ColorAdd', 'ColorSub', 'ColorDiv', 'ColorModulate', 'ColorMax', 'ColorMin', 'ColorDifference',
    'ColorAverage', 'ColorExclusion', 'ColorScreen', 'ColorDodge', 'ColorBurn',
    'BlendColorAdd', 'BlendColorModulate'
    ] do
  begin
    if (Target <> '') and (not SameText(Target, Name)) then
      continue;

    var Binding := BlendRegistry.FindBinding(Name);
    Assert(Binding <> nil);

    for var Implement in Binding do
    begin
      var bm := Spring.Benchmark.Benchmark(BM_BlendReg, Implement.Name).Args([Int64(Implement.Proc)]);
      bm.MinTime(MinTime); // seconds
      bm.TimeUnit(kMillisecond);
    end;

    for var Implement in Binding do
    begin
      var bm := Spring.Benchmark.Benchmark(BM_BlendRegExit, Implement.Name+'(early exit)').Args([Int64(Implement.Proc)]);
      bm.MinTime(MinTime); // seconds
      bm.TimeUnit(kMillisecond);
    end;
  end;

  (*
  ** ColorScale
  *)
  for var Name in [
    'ColorScale', 'LightenReg'
    ] do
  begin
    if (Target <> '') and (not SameText(Target, Name)) then
      continue;

    var Binding := BlendRegistry.FindBinding(Name);
    Assert(Binding <> nil);

    for var Implement in Binding do
    begin
      var bm := Spring.Benchmark.Benchmark(BM_BlendRegWeight, Implement.Name).Args([Int64(Implement.Proc)]);
      bm.MinTime(MinTime); // seconds
      bm.TimeUnit(kMillisecond);
    end;
  end;

  Spring.Benchmark.Benchmark_Main;
end;

begin
  try
    Main;
    WriteLn('Done');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  ReadLn;
end.

