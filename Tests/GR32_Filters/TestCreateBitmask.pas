unit TestCreateBitmask;

interface

uses
  TestFramework, GR32, GR32_Filters;

type
  TTestCreateBitmask = class(TTestCase)
  published
    procedure TestCreateBitmask;
  end;

implementation

{ TTestCreateBitmask }

procedure TTestCreateBitmask.TestCreateBitmask;
begin
  CheckEquals($FF000000, CreateBitmask([ccAlpha]));
  CheckEquals($00FF0000, CreateBitmask([ccRed]));
  CheckEquals($0000FF00, CreateBitmask([ccGreen]));
  CheckEquals($000000FF, CreateBitmask([ccBlue]));
  CheckEquals($FFFFFFFF, CreateBitmask([ccAlpha, ccRed, ccGreen, ccBlue]));
  CheckEquals($00FF00FF, CreateBitmask([ccRed, ccBlue]));
end;

initialization
  RegisterTest(TTestCreateBitmask.Suite);
end.
