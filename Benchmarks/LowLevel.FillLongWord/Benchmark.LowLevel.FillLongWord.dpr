program Benchmark.LowLevel.FillLongWord;

{$APPTYPE CONSOLE}

{$I GR32.inc}

uses
  SysUtils,
  Spring.Benchmark,
  GR32,
  GR32_System,
  GR32_Bindings,
  GR32_LowLevel;

//------------------------------------------------------------------------------
//
//      Retired implementations (too slow)
//
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// FillLongword_SSE2
//------------------------------------------------------------------------------
{$if (not defined(PUREPASCAL)) and (not defined(OMIT_SSE2))}
procedure FillLongword_SSE2(var X; Count: Integer; Value: Longword); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
        // EAX = X;   EDX = Count;   ECX = Value

        TEST       EDX, EDX        // if Count = 0 then
        JZ         @Exit           //   Exit

        PUSH       EDI             // push EDI on stack
        MOV        EDI, EAX        // Point EDI to destination

        CMP        EDX, 32
        JL         @SmallLoop

        AND        EAX, 3          // get aligned count
        TEST       EAX, EAX        // check if X is not dividable by 4
        JNZ        @SmallLoop      // otherwise perform slow small loop

        MOV        EAX, EDI
        SHR        EAX, 2          // bytes to count
        AND        EAX, 3          // get aligned count
        ADD        EAX,-4
        NEG        EAX             // get count to advance
        JZ         @SetupMain
        SUB        EDX, EAX        // subtract aligning start from total count

@AligningLoop:
        MOV        [EDI], ECX
        ADD        EDI, 4
        DEC        EAX
        JNZ        @AligningLoop

@SetupMain:
        MOV        EAX, EDX        // EAX = remaining count
        SHR        EAX, 2
        SHL        EAX, 2
        SUB        EDX, EAX        // EDX = remaining count
        SHR        EAX, 2

        MOVD       XMM0, ECX
        PUNPCKLDQ  XMM0, XMM0
        PUNPCKLDQ  XMM0, XMM0
@SSE2Loop:
        MOVDQA     [EDI], XMM0
        ADD        EDI, 16
        DEC        EAX
        JNZ        @SSE2Loop

@SmallLoop:
        MOV        EAX,ECX
        MOV        ECX,EDX

        REP        STOSD           // Fill count dwords

@ExitPOP:
        POP        EDI

@Exit:
{$ENDIF}

{$IFDEF TARGET_x64}
        // RCX = X;   RDX = Count;   R8 = Value

        TEST       RDX, RDX        // if Count = 0 then
        JZ         @Exit           //   Exit

        MOV        R9, RCX         // Point R9 to destination

        CMP        RDX, 32
        JL         @SmallLoop

        AND        RCX, 3          // get aligned count
        TEST       RCX, RCX        // check if X is not dividable by 4
        JNZ        @SmallLoop      // otherwise perform slow small loop

        MOV        RCX, R9
        SHR        RCX, 2          // bytes to count
        AND        RCX, 3          // get aligned count
        ADD        RCX,-4
        NEG        RCX             // get count to advance
        JZ         @SetupMain
        SUB        RDX, RCX        // subtract aligning start from total count

@AligningLoop:
        MOV        [R9], R8D
        ADD        R9, 4
        DEC        RCX
        JNZ        @AligningLoop

@SetupMain:
        MOV        RCX, RDX        // RCX = remaining count
        SHR        RCX, 2
        SHL        RCX, 2
        SUB        RDX, RCX        // RDX = remaining count
        SHR        RCX, 2

        MOVD       XMM0, R8D
        PUNPCKLDQ  XMM0, XMM0
        PUNPCKLDQ  XMM0, XMM0
@SSE2Loop:
        MOVDQA     [R9], XMM0
        ADD        R9, 16
        DEC        RCX
        JNZ        @SSE2Loop

        TEST       RDX, RDX
        JZ         @Exit
@SmallLoop:
        MOV        [R9], R8D
        ADD        R9, 4
        DEC        RDX
        JNZ        @SmallLoop
@Exit:
{$ENDIF}
end;
{$ifend}

//------------------------------------------------------------------------------
// FillLongword_AVX2_NT (Non-temporal streaming)
//------------------------------------------------------------------------------
{$if (not defined(PUREPASCAL)) and (not defined(OMIT_SSE2))}
procedure FillLongword_AVX2_NT(var X; Count: Integer; Value: Longword); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
        TEST       EDX, EDX
        JLE        @Exit
        PUSH       EDI
        MOV        EDI, EAX

        MOVD       XMM0, ECX
        VPBROADCASTD YMM0, XMM0

        CMP        EDX, 32
        JL         @Remainder

        MOV        EAX, EDI
        AND        EAX, 31
        JZ         @SetupMain
        SHR        EAX, 2
        NEG        EAX
        ADD        EAX, 8
        SUB        EDX, EAX

@AlignLoop:
        MOV        [EDI], ECX
        ADD        EDI, 4
        DEC        EAX
        JNZ        @AlignLoop

@SetupMain:
        MOV        EAX, EDX
        SHR        EAX, 5
        JZ         @Remainder

@Loop:
        VMOVNTDQ   [EDI], YMM0
        VMOVNTDQ   [EDI + 32], YMM0
        VMOVNTDQ   [EDI + 64], YMM0
        VMOVNTDQ   [EDI + 96], YMM0
        ADD        EDI, 128
        DEC        EAX
        JNZ        @Loop
        SFENCE

@Remainder:
        VZEROUPPER
        AND        EDX, 31
        JZ         @ExitPOP

@RemainderLoop:
        MOV        [EDI], ECX
        ADD        EDI, 4
        DEC        EDX
        JNZ        @RemainderLoop

@ExitPOP:
        POP        EDI
@Exit:
{$ENDIF}

{$IFDEF TARGET_x64}
        TEST       RDX, RDX
        JLE        @Exit
        MOV        R9, RCX

        MOVD       XMM0, R8D
        VPBROADCASTD YMM0, XMM0

        CMP        RDX, 32
        JL         @Remainder

        MOV        RAX, R9
        AND        RAX, 31
        JZ         @SetupMain
        SHR        RAX, 2
        NEG        RAX
        ADD        RAX, 8
        SUB        RDX, RAX

@AlignLoop:
        MOV        [R9], R8D
        ADD        R9, 4
        DEC        RAX
        JNZ        @AlignLoop

@SetupMain:
        MOV        RAX, RDX
        SHR        RAX, 5
        JZ         @Remainder

@Loop:
        VMOVNTDQ   [R9], YMM0
        VMOVNTDQ   [R9 + 32], YMM0
        VMOVNTDQ   [R9 + 64], YMM0
        VMOVNTDQ   [R9 + 96], YMM0
        ADD        R9, 128
        DEC        RAX
        JNZ        @Loop
        SFENCE

@Remainder:
        VZEROUPPER
        AND        RDX, 31
        JZ         @Exit

@RemainderLoop:
        MOV        [R9], R8D
        ADD        R9, 4
        DEC        RDX
        JNZ        @RemainderLoop

@Exit:
{$ENDIF}
end;
{$ifend}


//------------------------------------------------------------------------------
//
//      Candidate implementations (being tested)
//
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// FillLongword_AVX512
//------------------------------------------------------------------------------
// TODO : I don't have any hardware that supports AVX512
//------------------------------------------------------------------------------
{$if (not defined(PUREPASCAL)) and (not defined(OMIT_SSE2))}
{$if defined(FPC) or (CompilerVersion >= 37.0)}
procedure FillLongword_AVX512(var X; Count: Integer; Value: Longword); {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
        TEST       EDX, EDX
        JLE        @Exit
        PUSH       EDI
        MOV        EDI, EAX

        MOVD       XMM0, ECX
        VPBROADCASTD ZMM0, XMM0

        CMP        EDX, 64
        JL         @Remainder

        MOV        EAX, EDI
        AND        EAX, 63
        JZ         @SetupMain
        SHR        EAX, 2
        NEG        EAX
        ADD        EAX, 16
        SUB        EDX, EAX

@AlignLoop:
        MOV        [EDI], ECX
        ADD        EDI, 4
        DEC        EAX
        JNZ        @AlignLoop

@SetupMain:
        MOV        EAX, EDX
        SHR        EAX, 6              // 64 dwords (256 bytes) per iteration
        JZ         @Remainder

@Loop:
        VMOVDQA32  [EDI], ZMM0
        VMOVDQA32  [EDI + 64], ZMM0
        VMOVDQA32  [EDI + 128], ZMM0
        VMOVDQA32  [EDI + 192], ZMM0
        ADD        EDI, 256
        DEC        EAX
        JNZ        @Loop

@Remainder:
        VZEROUPPER
        AND        EDX, 63
        JZ         @ExitPOP

@RemainderLoop:
        MOV        [EDI], ECX
        ADD        EDI, 4
        DEC        EDX
        JNZ        @RemainderLoop

@ExitPOP:
        POP        EDI
@Exit:
{$ENDIF}

{$IFDEF TARGET_x64}
        TEST       RDX, RDX
        JLE        @Exit
        MOV        R9, RCX

        MOVD       XMM0, R8D
        VPBROADCASTD ZMM0, XMM0

        CMP        RDX, 64
        JL         @Remainder

        MOV        RAX, R9
        AND        RAX, 63
        JZ         @SetupMain
        SHR        RAX, 2
        NEG        RAX
        ADD        RAX, 16
        SUB        RDX, RAX

@AlignLoop:
        MOV        [R9], R8D
        ADD        R9, 4
        DEC        RAX
        JNZ        @AlignLoop

@SetupMain:
        MOV        RAX, RDX
        SHR        RAX, 6
        JZ         @Remainder

@Loop:
        VMOVDQA32  [R9], ZMM0
        VMOVDQA32  [R9 + 64], ZMM0
        VMOVDQA32  [R9 + 128], ZMM0
        VMOVDQA32  [R9 + 192], ZMM0
        ADD        R9, 256
        DEC        RAX
        JNZ        @Loop

@Remainder:
        VZEROUPPER
        AND        RDX, 63
        JZ         @Exit

@RemainderLoop:
        MOV        [R9], R8D
        ADD        R9, 4
        DEC        RDX
        JNZ        @RemainderLoop

@Exit:
{$ENDIF}
end;
{$ifend}
{$ifend}


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

const
  // Benchmark sizes from 8*8 (64) to 1024*1024 (1,048,576) dwords
  Sizes: array of Integer = [
    8 * 8,       // 64
    16 * 16,     // 256
    32 * 32,     // 1,024
    64 * 64,     // 4,096
    128 * 128,   // 16,384
    256 * 256,   // 65,536
    512 * 512,   // 262,144
    1024 * 1024  // 1,048,576
  ];

type
  TFillLongwordProc = procedure(var X; Count: Integer; Value: Longword);

procedure BM_FillLongWord(const state: TState);
var
  Buffer: TArray<Longword>;
begin
  var Proc := TFillLongwordProc(state[0]);
  var Size := state[1];

  SetLength(Buffer, Size + 32);

  for var _ in state do
  begin
    // Benchmark across various byte alignments (0..15 dwords offset)
    for var Offset := 0 to 15 do
    begin
      Proc(Buffer[Offset], Size, $12345678);
    end;
  end;

  state.Counters['Rate'] := Counter(Size, [kIsRate, kIsIterationInvariant]);
end;

procedure Main;
begin
  Spring.Benchmark.benchmark_format_args := False;

  var Binding := LowLevelRegistry.FindBinding('FillLongWord');
  Assert(Binding <> nil);

  for var Size in Sizes do
  begin
    for var Implement in Binding do
    begin
      var InstructionSupport := Implement.InstructionSupport;
      if (isReference in InstructionSupport) then
        InstructionSupport := InstructionSupport - [isReference] + [isPascal];

      if (InstructionSupport * GR32_System.CPU.InstructionSupport = []) then
        continue;

      var bm := Spring.Benchmark.Benchmark(BM_FillLongWord, Implement.Name + '/Size:' + Size.ToString).Args([Int64(Implement.Proc), Size]);
      bm.TimeUnit(kMicrosecond);
    end;
  end;

  Spring.Benchmark.Benchmark_Main;
end;

procedure RegisterBindings;
begin
  {$WARN SYMBOL_EXPERIMENTAL OFF}

  var Binding := LowLevelRegistry.FindBinding('FillLongWord');

{$if (not defined(PUREPASCAL)) and (not defined(OMIT_SSE2))}

  Binding.Add(@FillLongword_SSE2, [isSSE2]).Name := 'FillLongword_SSE2';
  Binding.Add(@FillLongword_AVX2_NT, [isAVX2]).Name := 'FillLongword_AVX2_NT';
{$if defined(FPC) or (CompilerVersion >= 37.0)}
  Binding.Add(@FillLongword_AVX512, [isAVX512f]).Name := 'FillLongword_AVX512';
{$ifend}

{$ifend}

  LowLevelRegistry.RebindAll;
end;

begin
  try
    RegisterBindings;
    Main;
    WriteLn('Done');
    ReadLn;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      ReadLn;
    end;
  end;
end.
