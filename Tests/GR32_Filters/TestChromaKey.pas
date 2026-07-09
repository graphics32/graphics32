unit TestChromaKey;

interface

uses
  TestFramework, GR32, GR32_Filters;

type
  TTestChromaKey = class(TTestCase)
  private
    FBitmap: TBitmap32;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestChromaKey;
  end;

implementation

{ TTestChromaKey }

procedure TTestChromaKey.SetUp;
begin
  FBitmap := TBitmap32.Create;
end;

procedure TTestChromaKey.TearDown;
begin
  FBitmap.Free;
end;

procedure TTestChromaKey.TestChromaKey;
begin
  FBitmap.SetSize(2, 1);
  FBitmap.Pixel[0, 0] := $FF112233;
  FBitmap.Pixel[1, 0] := $FFFFFFFF;

  ChromaKey(FBitmap, $FF112233);

  CheckEquals($00112233, FBitmap.Pixel[0, 0]);
  CheckEquals($FFFFFFFF, FBitmap.Pixel[1, 0]);
end;

initialization
  RegisterTest(TTestChromaKey.Suite);
end.
