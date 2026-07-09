unit TestColorToGrayscale;

interface

uses
  TestFramework, GR32, GR32_Filters;

type
  TTestColorToGrayscale = class(TTestCase)
  private
    FSrc, FDst: TBitmap32;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInPlace;
    procedure TestSourceToDest;
    procedure TestPreserveAlpha;
  end;

implementation

{ TTestColorToGrayscale }

procedure TTestColorToGrayscale.SetUp;
begin
  FSrc := TBitmap32.Create;
  FDst := TBitmap32.Create;
end;

procedure TTestColorToGrayscale.TearDown;
begin
  FSrc.Free;
  FDst.Free;
end;

procedure TTestColorToGrayscale.TestInPlace;
begin
  FSrc.SetSize(1, 1);
  FSrc.Pixel[0, 0] := $00FF0000; // Red

  ColorToGrayscale(FSrc);

  // Red intensity is around 76 (0.299 * 255)
  var Gray: Byte := FSrc.Pixel[0, 0] and $FF;
  Check(Gray > 0, 'Should have some intensity');
  CheckEquals(Gray, (FSrc.Pixel[0, 0] shr 8) and $FF);
  CheckEquals(Gray, (FSrc.Pixel[0, 0] shr 16) and $FF);
  CheckEquals($FF, FSrc.Pixel[0, 0] shr 24); // Default alpha is $FF
end;

procedure TTestColorToGrayscale.TestSourceToDest;
begin
  FSrc.SetSize(1, 1);
  FSrc.Pixel[0, 0] := $0000FF00; // Green

  ColorToGrayscale(FDst, FSrc);

  CheckEquals(1, FDst.Width);
  var Gray: Byte := FDst.Pixel[0, 0] and $FF;
  Check(Gray > 0, 'Should have some intensity');
end;

procedure TTestColorToGrayscale.TestPreserveAlpha;
begin
  FSrc.SetSize(1, 1);
  FSrc.Pixel[0, 0] := $AA0000FF; // Blue with Alpha $AA

  ColorToGrayscale(FDst, FSrc, True);

  CheckEquals($AA, FDst.Pixel[0, 0] shr 24);
end;

initialization
  RegisterTest(TTestColorToGrayscale.Suite);
end.
