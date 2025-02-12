unit TestBindings;

interface

{$include GR32.inc}
{$define TestCacheObliviousTranspose32}

uses
  Classes, Types,
  TestFramework,
  GR32,
  GR32_Bindings;

type
  TTestBindings = class(TTestCase)
  private
    FRegistry: TFunctionRegistry;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNewRegistry;
    procedure TestAddByID;
    procedure TestAddByRef;
    procedure TestRebindTiebreak;
  end;

implementation

uses
  GR32_System,
  GR32.CPUID,
  SysUtils,
  Math;

{$RANGECHECKS OFF}

type
  TProc = procedure(TestCase: TTestCase);

  TProcs = array[0..7] of TProc;

procedure ProcPascal(TestCase: TTestCase);
begin
end;

procedure ProcPascal2(TestCase: TTestCase);
begin
end;

procedure ProcPascal3(TestCase: TTestCase);
begin
end;

procedure ProcAsm(TestCase: TTestCase);
begin
end;

procedure ProcSSE(TestCase: TTestCase);
begin
end;

procedure ProcSSE2(TestCase: TTestCase);
begin
end;

const
  FlagTest = 1;

function FunctionPriorityPascal(const Info: IFunctionInfo): Integer;
begin
  Result := DefaultPriorityProc(Info); // This takes CPU and priority into account
  // Now handle Pascal flag
  if (isPascal in Info.InstructionSupport) then
    Dec(Result, 1024);
end;


procedure TTestBindings.SetUp;
begin
  inherited;

  if (FRegistry = nil) then
    FRegistry := NewRegistry
  else
    FRegistry.Clear;
end;

procedure TTestBindings.TearDown;
begin
  inherited;

  if (FRegistry <> nil) then
    FRegistry.Clear;
end;

procedure TTestBindings.TestNewRegistry;
begin
  var RegistryAnonymous := NewRegistry;
  CheckNotNull(RegistryAnonymous);
  CheckEquals('', RegistryAnonymous.Name);

  var RegistryTest1 := NewRegistry('Test');
  CheckNotNull(RegistryTest1);
  CheckEquals('Test', RegistryTest1.Name);
  CheckTrue(RegistryTest1 <> RegistryAnonymous);

  var RegistryTest2 := NewRegistry(RegistryTest1.Name);
  CheckNotNull(RegistryTest2);
  CheckEquals(RegistryTest1.Name, RegistryTest2.Name);
  CheckTrue(RegistryTest1 <> RegistryTest2);
end;

procedure TTestBindings.TestAddByID;
begin
  var Procs := Default(TProcs);

  for var i := 0 to High(Procs) do
    FRegistry.RegisterBinding(i, @@Procs[i]);

  FRegistry.Add(0, @ProcAsm,     [isAssembler]);                         //
  FRegistry.Add(0, @ProcPascal,  [isPascal],     2,              0);     // Priority, Flags,
  FRegistry.Add(0, @ProcPascal2, [isPascal],     3,              0);     // Priority, Flags,
  FRegistry.Add(0, @ProcPascal3, [isPascal],     0);                     // Priority

  FRegistry.Add(1, @ProcSSE2,   [isSSE2],        0);                    // Priority
  FRegistry.Add(1, @ProcSSE,    [isSSE],         2,             FlagTest);// Priority, Flags,

  FRegistry.Add(2, @ProcSSE2,    [isSSE2],       0,             FlagTest);// Priority, Flags,
  FRegistry.Add(2, @ProcSSE,     [isSSE],        2,             FlagTest);// Priority, Flags,
  FRegistry.Add(2, @ProcPascal,  [isPascal],     3);                    // Priority

  var SaveCPU := GR32_System.CPU;
  try

    CPU.InstructionSupport := [isPascal, isAssembler, isSSE];

    FRegistry.RebindAll(nil, True);

    CheckNotNull(PPointer(@@Procs[0])^);
    CheckNotNull(PPointer(@@Procs[1])^);
    CheckNotNull(PPointer(@@Procs[2])^);
    for var i := 3 to High(Procs) do
      CheckNull(PPointer(@@Procs[i])^);

    // Expect ASM; Better priority
    CheckSame(@ProcAsm, PPointer(@@Procs[0])^);
    // Expect SSE; CPU support
    CheckSame(@ProcSSE, PPointer(@@Procs[1])^);
    CheckSame(@ProcSSE, PPointer(@@Procs[2])^);

    // Rebind to give Pascal priority
    FRegistry.RebindAll(FunctionPriorityPascal);
    // Expect Pascal
    CheckSame(@ProcPascal3, PPointer(@@Procs[0])^);
    // Expect SSE; No pascal variant
    CheckSame(@ProcSSE, PPointer(@@Procs[1])^);
    // Expect Pascal
    CheckSame(@ProcPascal, PPointer(@@Procs[2])^);

  finally
    GR32_System.CPU := SaveCPU;
  end;
end;

procedure TTestBindings.TestAddByRef;
begin
  // Almost the same as TestAddByID; We have just replaced the ID with a pointer to the delegate in Add

  var Procs := Default(TProcs);

  for var i := 0 to High(Procs) do
    FRegistry.RegisterBinding(@@Procs[i]);

  FRegistry.Add(@@Procs[0], @ProcAsm,    [isAssembler]);                 //
  FRegistry.Add(@@Procs[0], @ProcPascal, [isPascal],     2,      0);     // Priority, Flags,
  FRegistry.Add(@@Procs[0], @ProcPascal2,[isPascal],     3,      0);     // Priority, Flags,
  FRegistry.Add(@@Procs[0], @ProcPascal3,[isPascal],     0);             // Priority

  // TInstructionSupport
  FRegistry.Add(@@Procs[1], @ProcSSE2,   [isSSE2],       0);             // Priority
  FRegistry.Add(@@Procs[1], @ProcSSE,    [isSSE],        2,     FlagTest);// Priority, Flags,

  FRegistry.Add(@@Procs[2], @ProcSSE2,   [isSSE2],       0,     FlagTest);// Priority, Flags,
  FRegistry.Add(@@Procs[2], @ProcSSE,    [isSSE],        2,     FlagTest);// Priority, Flags,
  FRegistry.Add(@@Procs[2], @ProcPascal, [isPascal],     3);            // Priority

  var SaveCPU := CPU;
  try

    CPU.InstructionSupport := [isPascal, isAssembler, isSSE];

    FRegistry.RebindAll(nil, True);

    CheckNotNull(PPointer(@@Procs[0])^);
    CheckNotNull(PPointer(@@Procs[1])^);
    CheckNotNull(PPointer(@@Procs[2])^);
    for var i := 3 to High(Procs) do
      CheckNull(PPointer(@@Procs[i])^);

    // Expect ASM; Better priority
    CheckSame(@ProcAsm, PPointer(@@Procs[0])^);
    // Expect SSE; CPU support
    CheckSame(@ProcSSE, PPointer(@@Procs[1])^);
    CheckSame(@ProcSSE, PPointer(@@Procs[2])^);


    // Rebind to give Pascal priority
    FRegistry.RebindAll(FunctionPriorityPascal);

    // Expect Pascal
    CheckSame(@ProcPascal3, PPointer(@@Procs[0])^);
    // Expect SSE; No pascal variant
    CheckSame(@ProcSSE, PPointer(@@Procs[1])^);
    // Expect Pascal
    CheckSame(@ProcPascal, PPointer(@@Procs[2])^);

  finally
    CPU := SaveCPU;
  end;
end;

procedure TTestBindings.TestRebindTiebreak;
begin
  var Proc: TProc := nil;
  FRegistry.RegisterBinding(@@Proc);

  FRegistry.Add(@@Proc, @ProcPascal, [isPascal]);
  FRegistry.Add(@@Proc, @ProcAsm,    [isAssembler]);
  FRegistry.Add(@@Proc, @ProcSSE2,   [isSSE2]);
  FRegistry.Add(@@Proc, @ProcSSE,    [isSSE]);

  var SaveCPU := CPU;
  try

    CPU.InstructionSupport := [isPascal, isAssembler, isSSE, isSSE2];

    FRegistry.Bindings[@@Proc].Rebind;

    // All have same priority; Always select best CPU feature (SSE2 in this case)
    CheckSame(@ProcSSE2, PPointer(@@Proc)^);

  finally
    CPU := SaveCPU;
  end;
end;

initialization
  RegisterTest(TTestBindings.Suite);
end.


