unit TestInvert;

interface

uses
  TestFramework, GR32, GR32_Filters;

type
  TTestInvert = class(TTestCase)
  private
    FSrc, FDst: TBitmap32;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInPlaceAll;
    procedure TestSourceToDestRGB;
    procedure TestSingleComponent;
  end;

implementation

{ TTestInvert }

procedure TTestInvert.SetUp;
begin
  FSrc := TBitmap32.Create;
  FDst := TBitmap32.Create;
end;

procedure TTestInvert.TearDown;
begin
  FSrc.Free;
  FDst.Free;
end;

procedure TTestInvert.TestInPlaceAll;
begin
  FSrc.SetSize(1, 1);
  FSrc.Pixel[0, 0] := $AA112233;

  Invert(FSrc);

  CheckEquals($55EEDDCC, FSrc.Pixel[0, 0]);
end;

procedure TTestInvert.TestSourceToDestRGB;
begin
  FSrc.SetSize(1, 1);
  FSrc.Pixel[0, 0] := $AA112233;

  Invert(FDst, FSrc, [ccRed, ccGreen, ccBlue]);

  CheckEquals($AAEEDDCC, FDst.Pixel[0, 0]);
end;

procedure TTestInvert.TestSingleComponent;
begin
  FSrc.SetSize(1, 1);
  FSrc.Pixel[0, 0] := $AA112233;

  Invert(FDst, FSrc, [ccGreen]);

  CheckEquals($AA11DD33, FDst.Pixel[0, 0]);
end;

initialization
  RegisterTest(TTestInvert.Suite);
end.
