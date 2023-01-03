unit FileTestFramework;

interface

uses
  TestFramework;

// Adapted from "A DUnit Folder Iterator Extension"
// https://www.uweraabe.de/Blog/2012/03/17/a-dunit-folder-iterator-extension/
type
  TFileTestCaseClass = class of TFileTestCase;

  TFileTestCase = class(TTestCase)
  private
    FTestFileName: string;
  public
    constructor Create(const AMethodName, ATestFileName: string); reintroduce; overload; virtual;
    class function HandlesFiles(const AFilename: string): boolean; virtual;
    function GetName: string; override;
    property TestFileName: string read FTestFileName;
  end;

  TFolderTestSuite = class(TTestSuite)
  private
    FBaseFolder: string;
    FFileMask: string;
    FRecursive: Boolean;
  protected
    procedure AddMethodTests(TestClass: TTestCaseClass; const NameOfMethod: string); overload; virtual;
    procedure ProcessFile(Suite: ITestSuite; TestClass: TFileTestCaseClass; const NameOfMethod, FileName: string); virtual;
    procedure ProcessFolder(Suite: ITestSuite; TestClass: TFileTestCaseClass; const NameOfMethod, Path, FileMask: string;
        Recursive: Boolean); overload; virtual;
    procedure ProcessBaseFolder(Suite: ITestSuite; TestClass: TFileTestCaseClass; const NameOfMethod: string); virtual;
  public
    constructor Create(const AName: string; TestClass: TFileTestCaseClass; const ABaseFolder, AFileMask: string; ARecursive: Boolean); overload;
    procedure AddTests(testClass: TTestCaseClass); override;
    property BaseFolder: string read FBaseFolder;
    property FileMask: string read FFileMask;
    property Recursive: Boolean read FRecursive;
  end;

implementation

uses
  IOUtils;

constructor TFolderTestSuite.Create(const AName: string; TestClass: TFileTestCaseClass; const ABaseFolder, AFileMask: string; ARecursive: Boolean);
begin
  inherited Create(AName);

  FBaseFolder := ABaseFolder;
  FFileMask := AFileMask;
  FRecursive := ARecursive;

  AddTests(TestClass);
end;

procedure TFolderTestSuite.AddMethodTests(TestClass: TTestCaseClass; const NameOfMethod: string);
begin
  if TestClass.InheritsFrom(TFileTestCase) then
  begin
    var suite := TTestSuite.Create(NameOfMethod);
    AddSuite(suite);
    ProcessBaseFolder(suite, TFileTestCaseClass(TestClass), NameOfMethod);
  end else
  begin
    AddTest(TestClass.Create(NameOfMethod));
  end;
end;

procedure TFolderTestSuite.AddTests(testClass: TTestCaseClass);
var
  MethodEnumerator:  TMethodEnumerator;
begin
  { call on the method enumerator to get the names of the test
    cases in the testClass }
  MethodEnumerator := nil;
  try
    MethodEnumerator := TMethodEnumerator.Create(testClass);
    { make sure we add each test case  to the list of tests }
    for var MethodIter := 0 to MethodEnumerator.Methodcount-1 do
    begin
      var NameOfMethod := MethodEnumerator.nameOfMethod[MethodIter];
      AddMethodTests(testClass, NameOfMethod);
    end;
  finally
    MethodEnumerator.free;
  end;
end;

procedure TFolderTestSuite.ProcessBaseFolder(Suite: ITestSuite; TestClass: TFileTestCaseClass; const NameOfMethod: string);
begin
  ProcessFolder(Suite, TestClass, NameOfMethod, BaseFolder, FileMask, Recursive);
end;

procedure TFolderTestSuite.ProcessFile(Suite: ITestSuite; TestClass: TFileTestCaseClass; const NameOfMethod, FileName: string);
begin
  if (TestClass.HandlesFiles(FileName)) then
    Suite.AddTest(TestClass.Create(NameOfMethod, FileName));
end;

procedure TFolderTestSuite.ProcessFolder(Suite: ITestSuite; TestClass: TFileTestCaseClass; const NameOfMethod, Path, FileMask: string; Recursive: Boolean);
begin
  for var Filename in TDirectory.GetFiles(Path, FileMask) do
    ProcessFile(suite, TestClass, NameOfMethod, Filename);

  if Recursive then
  begin
    for var Folder in TDirectory.GetDirectories(Path) do
    begin
      var TestSuite := TTestSuite.Create(TPath.GetFileName(Folder));
      Suite.AddSuite(TestSuite);
      ProcessFolder(TestSuite, TestClass, NameOfMethod, Folder, FileMask, true);
    end;
  end;
end;

constructor TFileTestCase.Create(const AMethodName, ATestFileName: string);
begin
  inherited Create(AMethodName);
  FTestFileName := ATestFileName;
end;

function TFileTestCase.GetName: string;
begin
  Result := TPath.GetFileNameWithoutExtension(TestFileName);
end;

class function TFileTestCase.HandlesFiles(const AFilename: string): boolean;
begin
  Result := True;
end;

end.

