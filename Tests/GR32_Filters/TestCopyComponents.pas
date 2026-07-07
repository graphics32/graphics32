unit TestCopyComponents;

interface

uses
  TestFramework, GR32, GR32_Filters, Types;

type
  TTestCopyComponents = class(TTestCase)
  private
    FSrc, FDst: TBitmap32;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSimpleCopy;
    procedure TestRectCopy;
    procedure TestSingleComponent;
    procedure TestMultiComponent;
  end;

implementation

{ TTestCopyComponents }

procedure TTestCopyComponents.SetUp;
begin
  FSrc := TBitmap32.Create;
  FDst := TBitmap32.Create;
end;

procedure TTestCopyComponents.TearDown;
begin
  FSrc.Free;
  FDst.Free;
end;

procedure TTestCopyComponents.TestSimpleCopy;
begin
  FSrc.SetSize(10, 10);
  FSrc.FillRect(0, 0, 10, 10, $AABBCCDD);
  FDst.SetSize(5, 5);
  FDst.FillRect(0, 0, 5, 5, $00000000);

  CopyComponents(FDst, FSrc, [ccRed, ccGreen, ccBlue, ccAlpha]);

  CheckEquals(10, FDst.Width);
  CheckEquals(10, FDst.Height);
  CheckEquals($AABBCCDD, FDst.Pixel[5, 5]);
end;

procedure TTestCopyComponents.TestRectCopy;
begin
  FSrc.SetSize(10, 10);
  FSrc.FillRect(0, 0, 10, 10, $FFFFFFFF);
  FDst.SetSize(10, 10);
  FDst.FillRect(0, 0, 10, 10, $00000000);

  CopyComponents(FDst, 2, 2, FSrc, Rect(0, 0, 5, 5), [ccRed, ccGreen, ccBlue, ccAlpha]);

  // Inside rect
  CheckEquals($FFFFFFFF, FDst.Pixel[2, 2]);
  CheckEquals($FFFFFFFF, FDst.Pixel[6, 6]);
  // Outsize rect
  CheckEquals($00000000, FDst.Pixel[1, 1]);
  CheckEquals($00000000, FDst.Pixel[7, 7]);
end;

procedure TTestCopyComponents.TestSingleComponent;
begin
  FSrc.SetSize(1, 1);
  FSrc.Pixel[0, 0] := $AABBCCDD;
  FDst.SetSize(1, 1);
  FDst.Pixel[0, 0] := $11223344;

  CopyComponents(FDst, FSrc, [ccRed]);

  CheckEquals($11BB3344, FDst.Pixel[0, 0]);
end;

procedure TTestCopyComponents.TestMultiComponent;
begin
  FSrc.SetSize(1, 1);
  FSrc.Pixel[0, 0] := $AABBCCDD;
  FDst.SetSize(1, 1);
  FDst.Pixel[0, 0] := $11223344;

  CopyComponents(FDst, FSrc, [ccAlpha, ccBlue]);

  CheckEquals($AA2233DD, FDst.Pixel[0, 0]);
end;

initialization
  RegisterTest(TTestCopyComponents.Suite);
end.
