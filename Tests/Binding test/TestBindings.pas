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
  published
    procedure TestNewRegistry;
    procedure TestAddByID;
    procedure TestAddByRef;
  end;

implementation

uses
  SysUtils,
  Math,
  GR32_System;

{$RANGECHECKS OFF}


{ TTestBindings }


{ TTestBindings }

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

procedure ProcMMX(TestCase: TTestCase);
begin
end;

procedure ProcSSE(TestCase: TTestCase);
begin
end;

procedure ProcSSE2(TestCase: TTestCase);
begin
end;

const
  FlagNone = 0;
  FlagAsm = 1;
  FlagPascal = 2;
  FlagTest = 4;

function FunctionPriorityPascal(Info: PFunctionInfo): Integer;
begin
  Result := DefaultPriorityProc(Info); // This takes CPU and priority into account
  // Now handle Flag
  if (Info.Flags and FlagPascal <> 0) then
    Dec(Result, 1024);
end;

procedure TTestBindings.TestAddByID;
begin
  var Registry := NewRegistry;

  var Procs := Default(TProcs);

  for var i := 0 to High(Procs) do
    Registry.RegisterBinding(i, @@Procs[i]);

  Registry.Add(0, @ProcAsm, FlagAsm); // Flags
  Registry.Add(0, @ProcPascal, FlagPascal, 2); // Flags, Priority
  Registry.Add(0, @ProcPascal2, FlagPascal, 3); // Flags, Priority
  Registry.Add(0, @ProcPascal3, FlagPascal, 1); // Flags, Priority

  // TCPUFeature
  Registry.Add(1, @ProcMMX, [ciMMX]);
  Registry.Add(1, @ProcSSE2, [ciSSE2], FlagTest); // Flags
  Registry.Add(1, @ProcSSE, [ciSSE], FlagTest, 2); // Flags, Priority

  // TInstructionSupport
  Registry.Add(2, @ProcMMX, [isMMX]);
  Registry.Add(2, @ProcSSE2, [isSSE2], FlagTest); // Flags
  Registry.Add(2, @ProcSSE, [isSSE], FlagTest, 2); // Flags, Priority
  const NoSupport: TInstructionSupport = [];
  Registry.Add(2, @ProcPascal, NoSupport, FlagPascal, 3); // Flags, Priority

  var SaveCPU := CPU;
  try

    CPU.InstructionSupport := [isSSE];

    Registry.RebindAll(True);

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
    Registry.RebindAll(FunctionPriorityPascal);
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

procedure TTestBindings.TestAddByRef;
begin
  // Almost the same as TestAddByID; We have just replaced the ID with a pointer to the delegate in Add

  var Registry := NewRegistry;

  var Procs := Default(TProcs);

  for var i := 0 to High(Procs) do
    Registry.RegisterBinding(@@Procs[i]);

  Registry.Add(@@Procs[0], @ProcAsm, FlagAsm); // Flags
  Registry.Add(@@Procs[0], @ProcPascal, FlagPascal, 2); // Flags, Priority
  Registry.Add(@@Procs[0], @ProcPascal2, FlagPascal, 3); // Flags, Priority
  Registry.Add(@@Procs[0], @ProcPascal3, FlagPascal, 1); // Flags, Priority

  // TCPUFeature; Not support with this variant

  // TInstructionSupport
  Registry.Add(@@Procs[1], @ProcMMX, [isMMX]);
  Registry.Add(@@Procs[1], @ProcSSE2, [isSSE2], FlagTest); // Flags
  Registry.Add(@@Procs[1], @ProcSSE, [isSSE], FlagTest, 2); // Flags, Priority

  Registry.Add(@@Procs[2], @ProcMMX, [isMMX]);
  Registry.Add(@@Procs[2], @ProcSSE2, [isSSE2], FlagTest); // Flags
  Registry.Add(@@Procs[2], @ProcSSE, [isSSE], FlagTest, 2); // Flags, Priority
  const NoSupport: TInstructionSupport = [];
  Registry.Add(@@Procs[2], @ProcPascal, NoSupport, FlagPascal, 3); // Flags, Priority

  var SaveCPU := CPU;
  try

    CPU.InstructionSupport := [isSSE];

    Registry.RebindAll(True);

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
    Registry.RebindAll(FunctionPriorityPascal);

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

initialization
  RegisterTest(TTestBindings.Suite);
end.


