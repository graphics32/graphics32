unit TestApplyLUT;

interface

uses
  TestFramework, GR32, GR32_Filters;

type
  TTestApplyLUT = class(TTestCase)
  private
    FSrc, FDst: TBitmap32;
    FLUT: TLUT8;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestApplyLUT_Resize;
    procedure TestApplyLUT_NoResize;
    procedure TestPreserveAlpha_Resize;
    procedure TestPreserveAlpha_NoResize;
  end;

implementation

{ TTestApplyLUT }

procedure TTestApplyLUT.SetUp;
var
  I: Integer;
begin
  FSrc := TBitmap32.Create;
  FDst := TBitmap32.Create;
  for I := 0 to 255 do
    FLUT[I] := 255 - I; // Inverting LUT
end;

procedure TTestApplyLUT.TearDown;
begin
  FSrc.Free;
  FDst.Free;
end;

procedure TTestApplyLUT.TestApplyLUT_Resize;
begin
  FSrc.SetSize(1, 1);
  FSrc.Pixel[0, 0] := $AA112233;
  FDst.SetSize(2, 2); // ApplyLUT will resize
  FDst.Pixel[0, 0] := $01020304;

  ApplyLUT(FDst, FSrc, FLUT);

  CheckEquals($FFEEDDCC, FDst.Pixel[0, 0]); // Alpha should be $FF by default
end;

procedure TTestApplyLUT.TestApplyLUT_NoResize;
begin
  FSrc.SetSize(1, 1);
  FSrc.Pixel[0, 0] := $AA112233;
  FDst.SetSize(1, 1); // ApplyLUT will not resize
  FDst.Pixel[0, 0] := $01020304;
  ApplyLUT(FDst, FSrc, FLUT);
  CheckEquals($FFEEDDCC, FDst.Pixel[0, 0]); // Alpha should be $FF by default

  ApplyLUT(FDst, FSrc, FLUT);

  CheckEquals($FFEEDDCC, FDst.Pixel[0, 0]); // Alpha should be $FF by default
end;

procedure TTestApplyLUT.TestPreserveAlpha_Resize;
begin
  FSrc.SetSize(1, 1);
  FSrc.Pixel[0, 0] := $AA112233;
  FDst.SetSize(2, 2); // ApplyLUT will resize
  FDst.Pixel[0, 0] := $01020304;

  ApplyLUT(FDst, FSrc, FLUT, True);

  CheckEquals($AAEEDDCC, FDst.Pixel[0, 0]);
end;

procedure TTestApplyLUT.TestPreserveAlpha_NoResize;
begin
  FSrc.SetSize(1, 1);
  FSrc.Pixel[0, 0] := $AA112233;
  FDst.SetSize(1, 1); // ApplyLUT will not resize
  FDst.Pixel[0, 0] := $01020304;

  ApplyLUT(FDst, FSrc, FLUT, True);

  CheckEquals($AAEEDDCC, FDst.Pixel[0, 0]);
end;

initialization
  RegisterTest(TTestApplyLUT.Suite);
end.
