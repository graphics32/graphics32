unit TestCheckParams;

interface

uses
  TestFramework, GR32, GR32_Filters, SysUtils;

type
  TTestCheckParams = class(TTestCase)
  private
    FSrc, FDst: TBitmap32;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNilSource;
    procedure TestNilDestination;
    procedure TestSameSourceAndDestination;
    procedure TestResizeDestination;
    procedure TestNoResizeDestination;
  end;

implementation

{ TTestCheckParams }

procedure TTestCheckParams.SetUp;
begin
  FSrc := TBitmap32.Create;
  FDst := TBitmap32.Create;
end;

procedure TTestCheckParams.TearDown;
begin
  FSrc.Free;
  FDst.Free;
end;

procedure TTestCheckParams.TestNilSource;
begin
  ExpectedException := Exception;
  CheckParams(FDst, nil);
end;

procedure TTestCheckParams.TestNilDestination;
begin
  ExpectedException := Exception;
  CheckParams(nil, FSrc);
end;

procedure TTestCheckParams.TestSameSourceAndDestination;
begin
  FSrc.SetSize(10, 10);
  CheckFalse(CheckParams(FSrc, FSrc), 'Should return false when Src = Dst');
end;

procedure TTestCheckParams.TestResizeDestination;
begin
  FSrc.SetSize(20, 20);
  FDst.SetSize(10, 10);

  CheckTrue(CheckParams(FDst, FSrc, True), 'Should return true when Dst was resized');

  CheckEquals(20, FDst.Width);
  CheckEquals(20, FDst.Height);
end;

procedure TTestCheckParams.TestNoResizeDestination;
begin
  FSrc.SetSize(20, 20);
  FDst.SetSize(10, 10);

  CheckFalse(CheckParams(FDst, FSrc, False), 'Should return false when ResizeDst = False');

  CheckEquals(10, FDst.Width);
  CheckEquals(10, FDst.Height);
end;

initialization
  RegisterTest(TTestCheckParams.Suite);
end.
