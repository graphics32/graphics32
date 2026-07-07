unit TestApplyBitmask;

interface

uses
  TestFramework, GR32, GR32_Filters, Types;

type
  TTestApplyBitmask = class(TTestCase)
  private
    FSrc, FDst: TBitmap32;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInPlaceXor;
    procedure TestSourceToDestAnd;
    procedure TestSourceToDestOr;
  end;

implementation

{ TTestApplyBitmask }

procedure TTestApplyBitmask.SetUp;
begin
  FSrc := TBitmap32.Create;
  FDst := TBitmap32.Create;
end;

procedure TTestApplyBitmask.TearDown;
begin
  FSrc.Free;
  FDst.Free;
end;

procedure TTestApplyBitmask.TestInPlaceXor;
begin
  FSrc.SetSize(1, 1);
  FSrc.Pixel[0, 0] := $AA112233;

  ApplyBitmask(FSrc, FSrc.BoundsRect, $00FF00FF, loXOR);

  CheckEquals($AAEE22CC, FSrc.Pixel[0, 0]);
end;

procedure TTestApplyBitmask.TestSourceToDestAnd;
begin
  FSrc.SetSize(1, 1);
  FSrc.Pixel[0, 0] := $AA112233;
  FDst.SetSize(1, 1);
  FDst.Pixel[0, 0] := $00000000;

  ApplyBitmask(FDst, 0, 0, FSrc, FSrc.BoundsRect, $FF00FF00, loAND);

  CheckEquals($AA002200, FDst.Pixel[0, 0]);
end;

procedure TTestApplyBitmask.TestSourceToDestOr;
begin
  FSrc.SetSize(1, 1);
  FSrc.Pixel[0, 0] := $AA112233;
  FDst.SetSize(1, 1);
  FDst.Pixel[0, 0] := $00000000;

  ApplyBitmask(FDst, 0, 0, FSrc, FSrc.BoundsRect, $00FF0000, loOR);

  CheckEquals($AAFF2233, FDst.Pixel[0, 0]);
end;

initialization
  RegisterTest(TTestApplyBitmask.Suite);
end.
