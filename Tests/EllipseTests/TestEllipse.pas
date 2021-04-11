unit TestEllipse;

interface

uses
  TestFrameWork, GR32;

type
  TTestEllipse = class(TTestCase)
  published
    procedure EllipseOfMultipleSizes;
    procedure BlendingEllipses;
    procedure MergingEllipses;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  private
    Have, Want: TBitmap32;
    procedure WantRed(const SetRed: array of Integer);
  end;

implementation

uses
  Bitmap32CompareDialogUnit;

procedure TTestEllipse.SetUp;
begin
  Have := TBitmap32.Create;
  Want := TBitmap32.Create;
end;

procedure TTestEllipse.TearDown;
begin
  Have.Free;
  Want.Free;
end;

procedure TTestEllipse.WantRed(const SetRed: array of Integer);
var
  I: Integer;
begin
  for I := 0 to High(SetRed) do
    if SetRed[I] <> 0 then
      Want.Pixel[I mod Want.Width, I div Want.Width] := clRed32;
end;

procedure TTestEllipse.EllipseOfMultipleSizes;
const
  MaxSize = 15;
var
  X, Y, W, H: Integer;
begin
  Want.LoadFromFile('gold_ellipses_in_all_sizes.bmp');
  Have.SetSize(Want.Width, Want.Height);

  Y := 1;
  for H := 1 to MaxSize do
  begin
    X := 1;
    for W := 1 to MaxSize do
    begin
      Have.FillEllipse(X, Y, X + W, Y + H, clRed32);
      Inc(X, W + 1);
    end;
    Inc(Y, H + 1);
  end;

  Have.FillEllipse(1, 1, 1, 1, clRed32);

  CheckBitmapsEqual(Want, Have);
end;

procedure TTestEllipse.BlendingEllipses;
begin
  Want.LoadFromFile('gold_blend_ellipses.bmp');

  Have.SetSize(26, 17);
  Have.CombineMode := cmBlend;
  Have.FillEllipseT(1, 1, 16, 16, $80FF0000);
  Have.FillEllipseT(10, 1, 25, 16, $800000FF);

  CheckBitmapsEqual(Want, Have);
end;

procedure TTestEllipse.MergingEllipses;
begin
  Want.LoadFromFile('gold_merge_ellipses.bmp');

  Have.SetSize(26, 17);
  Have.CombineMode := cmMerge;
  Have.FillEllipseT(1, 1, 16, 16, $80FF0000);
  Have.FillEllipseT(10, 1, 25, 16, $800000FF);

  CheckBitmapsEqual(Want, Have);
end;

initialization

TestFrameWork.RegisterTest(TTestEllipse.Suite);

end.
