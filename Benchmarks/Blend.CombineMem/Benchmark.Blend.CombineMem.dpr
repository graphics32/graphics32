program Benchmark.Blend.CombineMem;

{$APPTYPE CONSOLE}

{$I GR32.inc}

uses
  Spring.Benchmark,
  GR32,
  GR32_Blend,
  GR32.Blend.Pascal,
  GR32.Blend.&ASM,
  GR32.Blend.MMX,
  GR32.Blend.SSE2;

procedure BM_CombineMem(const state: TState);
begin
  var CombineMemProc := TCombineMem(state[0]);

  for var _ in state do
  begin
    for var Weight := 0 to 255 do
      for var F := 0 to $2FFF do
      begin
        var ColorFG: TColor32 := F or (F shl 16);
        for var B := 0 to $FF do
        begin
          var ColorBG: TColor32 := B or (B shl 8) or (B shl 16);
          CombineMemProc(ColorFG, ColorBG, Weight);
{$IFNDEF OMIT_MMX}
          EMMS_MMX;
{$ENDIF OMIT_MMX}
        end;
      end;
  end;
end;


begin
  Spring.Benchmark.Benchmark(BM_CombineMem, 'CombineMem_Pas_Table').Arg(Int64(@CombineMem_Pas_Table)).TimeUnit(kMillisecond);
  Spring.Benchmark.Benchmark(BM_CombineMem, 'CombineMem_Pas_Div255').Arg(Int64(@CombineMem_Pas_Div255)).TimeUnit(kMillisecond);
  Spring.Benchmark.Benchmark(BM_CombineMem, 'CombineMem_Pas_Retro').Arg(Int64(@CombineMem_Pas_Retro)).TimeUnit(kMillisecond);

  Spring.Benchmark.Benchmark(BM_CombineMem, 'CombineMem_ASM').Arg(Int64(@CombineMem_ASM)).TimeUnit(kMillisecond);

{$IFNDEF OMIT_MMX}
  Spring.Benchmark.Benchmark(BM_CombineMem, 'CombineMem_MMX').Arg(Int64(@CombineMem_MMX)).TimeUnit(kMillisecond);
{$ENDIF OMIT_MMX}

  Spring.Benchmark.Benchmark(BM_CombineMem, 'CombineMem_SSE2.Old').Arg(Int64(@CombineMem_SSE2_Table)).TimeUnit(kMillisecond);
  Spring.Benchmark.Benchmark(BM_CombineMem, 'CombineMem_SSE2.New').Arg(Int64(@CombineMem_SSE2_128)).TimeUnit(kMillisecond);
  Spring.Benchmark.Benchmark(BM_CombineMem, 'CombineMem_SSE41_8081').Arg(Int64(@CombineMem_SSE41_8081)).TimeUnit(kMillisecond);

  Spring.Benchmark.Benchmark(BM_CombineMem, 'CombineMem_SSE41_Kadaif').Arg(Int64(@CombineMem_SSE41_Kadaif)).TimeUnit(kMillisecond);

  Spring.Benchmark.Benchmark_Main;
end.

