program Benchmark.Mirror;

{$APPTYPE CONSOLE}

{$I GR32.inc}

uses
  SysUtils,
  Spring.Benchmark;

function Mirror_Pas(Value, Max: Integer): Integer;
begin
  if Value >= 0 then
    Result := Value
  else
    Result := -Value;

  while (Result > Max) do
    Result := Abs(Max + Max - Result);
end;

function Mirror_Asm(Value, Max: Integer): Integer;
asm
{$IFDEF TARGET_x64}
        MOV       EAX, ECX      // Value
{$ENDIF}
        // EAX: Value
        // EDX: Max

        // Max2 := 2*Max
        LEA       ECX, [EDX+EDX]

        // Result := Value

@Loop:
        // Result := Abs(Result)
        TEST      EAX, EAX
        JNL       @Positive
        NEG       EAX


@Positive:
        // while (Result > Max) do
        CMP       EAX, EDX
        JLE       @Exit

        // Result := 2*Max - Result
        NEG       EAX
        ADD       EAX, ECX

        JMP       @Loop
@Exit:
end;

//------------------------------------------------------------------------------

procedure BenchmarkMirrorPas(const state: TState);
begin
  for var _ in state do
  begin
    for var Max := 0 to 5000 do
      for var Value := 0 to 5*Max do
        Mirror_Pas(Value*10, (Max*10)+1);
  end;
end;

procedure BenchmarkMirrorAsm(const state: TState);
begin
  for var _ in state do
  begin
    for var Max := 0 to 5000 do
      for var Value := 0 to 5*Max do
        Mirror_Asm(Value*10, (Max*10)+1);
  end;
end;

//------------------------------------------------------------------------------

procedure Benchmark(BenchmarkFunc: TFunction; const Name: string);
begin
  Spring.Benchmark.Benchmark(BenchmarkFunc, Name).TimeUnit(kMillisecond);
end;

//------------------------------------------------------------------------------

procedure Main;
begin
  Spring.Benchmark.benchmark_format_args := False;

  Benchmark(BenchmarkMirrorAsm, 'Mirror (Asm)');
  Benchmark(BenchmarkMirrorPas, 'Mirror (Pascal)');

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

