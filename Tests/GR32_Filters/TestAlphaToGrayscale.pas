unit TestAlphaToGrayscale;

interface

uses
  TestFramework, GR32, GR32_Filters;

type
  TTestAlphaToGrayscale = class(TTestCase)
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

{ TTestAlphaToGrayscale }

procedure TTestAlphaToGrayscale.SetUp;
begin
  FSrc := TBitmap32.Create;
  FDst := TBitmap32.Create;
end;

procedure TTestAlphaToGrayscale.TearDown;
begin
  FSrc.Free;
  FDst.Free;
end;

procedure TTestAlphaToGrayscale.TestInPlace;
begin
  FSrc.SetSize(1, 1);
  FSrc.Pixel[0, 0] := $AA112233;

  AlphaToGrayscale(FSrc);

  CheckEquals($AAAAAA, FSrc.Pixel[0, 0] and $FFFFFF);

  // Implementation doesn't change alpha in single param version according to docs,
  // but let's check what it actually does.
  CheckEquals($AA, TColor32Entry(FSrc.Pixel[0, 0]).A);
end;

procedure TTestAlphaToGrayscale.TestSourceToDest;
begin
  FSrc.SetSize(1, 1);
  FSrc.Pixel[0, 0] := $AA112233;

  AlphaToGrayscale(FDst, FSrc);

  CheckEquals(1, FDst.Width);
  CheckEquals($AAAAAA, FDst.Pixel[0, 0] and $FFFFFF);
end;

initialization
  RegisterTest(TTestAlphaToGrayscale.Suite);
end.
