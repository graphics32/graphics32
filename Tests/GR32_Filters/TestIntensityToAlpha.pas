unit TestIntensityToAlpha;

interface

uses
  TestFramework, GR32, GR32_Filters;

type
  TTestIntensityToAlpha = class(TTestCase)
  private
    FSrc, FDst: TBitmap32;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIntensityToAlpha;
  end;

implementation

{ TTestIntensityToAlpha }

procedure TTestIntensityToAlpha.SetUp;
begin
  FSrc := TBitmap32.Create;
  FDst := TBitmap32.Create;
end;

procedure TTestIntensityToAlpha.TearDown;
begin
  FSrc.Free;
  FDst.Free;
end;

procedure TTestIntensityToAlpha.TestIntensityToAlpha;
begin
  FSrc.SetSize(1, 1);
  FSrc.Pixel[0, 0] := $00FFFFFF; // White
  FDst.SetSize(1, 1);
  FDst.Pixel[0, 0] := $00000000;

  IntensityToAlpha(FDst, FSrc);

  CheckEquals($FF, TColor32Entry(FDst.Pixel[0, 0]).A);

  FSrc.Pixel[0, 0] := $00000000; // Black
  IntensityToAlpha(FDst, FSrc);
  CheckEquals($00, TColor32Entry(FDst.Pixel[0, 0]).A);
end;

initialization
  RegisterTest(TTestIntensityToAlpha.Suite);
end.
