unit TestInvertRGB;

interface

uses
  TestFramework, GR32, GR32_Filters;

type
  TTestInvertRGB = class(TTestCase)
  private
    FSrc, FDst: TBitmap32;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInPlace;
    procedure TestSourceToDest;
  end;

implementation

{ TTestInvertRGB }

procedure TTestInvertRGB.SetUp;
begin
  FSrc := TBitmap32.Create;
  FDst := TBitmap32.Create;
end;

procedure TTestInvertRGB.TearDown;
begin
  FSrc.Free;
  FDst.Free;
end;

procedure TTestInvertRGB.TestInPlace;
begin
  FSrc.SetSize(1, 1);
  FSrc.Pixel[0, 0] := $AA112233;

  InvertRGB(FSrc);

  CheckEquals($AAEEDDCC, FSrc.Pixel[0, 0]);
end;

procedure TTestInvertRGB.TestSourceToDest;
begin
  FSrc.SetSize(1, 1);
  FSrc.Pixel[0, 0] := $AA112233;

  InvertRGB(FDst, FSrc);

  CheckEquals($AAEEDDCC, FDst.Pixel[0, 0]);
end;

initialization
  RegisterTest(TTestInvertRGB.Suite);
end.
