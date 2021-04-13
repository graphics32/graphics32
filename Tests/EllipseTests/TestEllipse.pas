unit TestEllipse;

interface

uses
  TestFrameWork, GR32;

{$DEFINE RUN_BENCHMARKS}

type
  TTestEllipse = class(TTestCase)
  published
    procedure FillEllipse_WorksForArbitrarySizes;
    procedure FillEllipse_OnZeroSizedBitmapDoesNothing;
    procedure FillEllipse_HandlesBackendBitsBeingNil;
    procedure FillEllipse_InMeasuringModeDrawsNothing;
    procedure FillEllipse_InMeasuringModeChangesBoundingRectangle;

    procedure FillEllipseT_WorksForArbitrarySizes;
    procedure FillEllipseT_CanBlendEllipses;
    procedure FillEllipseT_CanMergeEllipses;
    procedure FillEllipseT_WithFullOpacityBehavesLikeFillEllipse;
    procedure FillEllipseT_WithFullTransparencyDrawsNothing;
    procedure FillEllipseT_HandlesBackendBitsBeingNil;
    procedure FillEllipseT_InMeasuringModeDrawsNothing;
    procedure FillEllipseT_InMeasuringModeChangesBoundingRectangle;

    procedure FillEllipseS_WorksForArbitrarySizes;
    procedure FillEllipseS_ClipsEllipses;
    procedure FillEllipseS_OnZeroSizedBitmapDoesNothing;
    procedure FillEllipseS_HandlesBackendBitsBeingNil;
    procedure FillEllipseS_DoesNotDrawInvalidEllipses;
    procedure FillEllipseS_InMeasuringModeDrawsNothing;
    procedure FillEllipseS_InMeasuringModeChangesBoundingRectangle;
    procedure FillEllipseS_MeasuresOnlyClippedRectangle;
    procedure FillEllipseS_HasOverloadTakingRectangle;

    procedure FillEllipseTS_WorksForArbitrarySizes;
    procedure FillEllipseTS_ClipsBlendedEllipses;
    procedure FillEllipseTS_ClipsMrgedEllipses;
    procedure FillEllipseTS_OnZeroSizedBitmapDoesNothing;
    procedure FillEllipseTS_WithFullOpacityBehavesLike_FillEllipseS;
    procedure FillEllipseTS_WithFullTransparencyDrawsNothing;
    procedure FillEllipseTS_HandlesBackendBitsBeingNil;
    procedure FillEllipseTS_DoesNotDrawInvalidEllipses;
    procedure FillEllipseTS_InMeasuringModeDrawsNothing;
    procedure FillEllipseTS_InMeasuringModeChangesBoundingRectangle;
    procedure FillEllipseTS_MeasuresOnlyClippedRectangle;
    procedure FillEllipseTS_HasOverloadTakingRectangle;

{$IFDEF RUN_BENCHMARKS} published {$ELSE} private {$ENDIF}
    procedure FillRect_Benchmark;
    procedure FillEllipse_Benchmark;
    procedure FillEllipseT_Benchmark;
    procedure FillEllipseS_Benchmark;
    procedure FillEllipseTS_Benchmark;
    procedure TCavas32_Ellipse_Benchmark;

  private
    // This test case show the difference between the TCanvas32.Ellipse version
    // (anti-aliased) and FillEllipse. Make it published to see the difference.
    procedure Compare_FillEllipse_And_TCanvas32_Ellipse;

  protected
    procedure SetUp; override;
    procedure TearDown; override;
  private
    Have, Want: TBitmap32;
    ChangeCount: Integer;
    ChangeArea: TRect;
    ChangeInfo: Cardinal;
    procedure RememberLastChangeEvent(Sender: TObject; const Area: TRect;
      const Info: Cardinal);
  end;

implementation

uses
  Bitmap32CompareDialogUnit, System.Classes, System.Diagnostics, System.SysUtils,
  System.Types, GR32_Paths, GR32_Brushes, GR32_Polygons;

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

procedure TTestEllipse.FillEllipse_WorksForArbitrarySizes;
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

  CheckBitmapsEqual(Want, Have);
end;

procedure TTestEllipse.FillEllipse_OnZeroSizedBitmapDoesNothing;
begin
  Want.SetSize(0, 0);
  Have.SetSize(0, 0);
  Have.FillEllipse(0, 0, 0, 0, clRed32);
  CheckBitmapsEqual(Want, Have);
end;

type
  // NilBackend is only there to have a backend whose Bits property is nil.
  NilBackend = class(TCustomBackend)
  protected
    procedure Changing; override;
{$IFDEF BITS_GETTER}
    function GetBits: PColor32Array; override;
{$ENDIF}
    procedure InitializeSurface(NewWidth, NewHeight: Integer;
      ClearBuffer: Boolean); override;
    procedure FinalizeSurface; override;
  public
    constructor Create; overload; override;
    constructor Create(Owner: TCustomBitmap32); reintroduce; overload; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    function Empty: Boolean; override;
    procedure ChangeSize(out Width, Height: Integer; NewWidth, NewHeight: Integer;
      ClearBuffer: Boolean = True); override;
  end;

procedure NilBackend.Changing;
begin
end;

{$IFDEF BITS_GETTER}

function NilBackend.GetBits: PColor32Array;
begin
  Result := nil;
end;

{$ENDIF}

procedure NilBackend.InitializeSurface(NewWidth, NewHeight: Integer;
  ClearBuffer: Boolean);
begin
end;

procedure NilBackend.FinalizeSurface;
begin
end;

constructor NilBackend.Create;
begin
end;

constructor NilBackend.Create(Owner: TCustomBitmap32);
begin
end;

destructor NilBackend.Destroy;
begin
end;

procedure NilBackend.Assign(Source: TPersistent);
begin
end;

procedure NilBackend.Clear;
begin
end;

function NilBackend.Empty: Boolean;
begin
  Result := false;
end;

procedure NilBackend.ChangeSize(out Width, Height: Integer; NewWidth, NewHeight: Integer;
  ClearBuffer: Boolean = True);
begin
end;

procedure TTestEllipse.FillEllipse_HandlesBackendBitsBeingNil;
begin
  // The FillRect function handles the case where the Bits property is nil. We want to
  // handle this for FillEllipse as well so we provoke an error here if it is not handled
  // properly.
  Have.SetSize(20, 20);
  Have.Backend := NilBackend.Create;
  Have.FillEllipse(1, 1, 19, 19, clRed32);
  // If FillEllipse tries to access the Bits an AccessViolation should occur.
end;

procedure TTestEllipse.FillEllipse_InMeasuringModeDrawsNothing;
begin
  Want.SetSize(20, 20);

  Have.SetSize(20, 20);
  Have.BeginMeasuring(nil);
  Have.FillEllipse(1, 1, 19, 19, clRed32);
  Have.EndMeasuring;

  CheckBitmapsEqual(Want, Have);
end;

procedure TTestEllipse.RememberLastChangeEvent(Sender: TObject; const Area: TRect;
  const Info: Cardinal);
begin
  Inc(ChangeCount);
  ChangeArea := Area;
  ChangeInfo := Info;
end;

procedure TTestEllipse.FillEllipse_InMeasuringModeChangesBoundingRectangle;
begin
  Have.SetSize(20, 20);
  Have.BeginMeasuring(RememberLastChangeEvent);
  Have.FillEllipse(1, 2, 15, 10, clRed32);
  Have.EndMeasuring;
  CheckEquals(1, ChangeCount);
  CheckEquals(AREAINFO_RECT, ChangeInfo);
  CheckEquals(1, ChangeArea.Left);
  CheckEquals(2, ChangeArea.Top);
  // Note that these are not the Right and Bottom values of the ellipse but 1 larger. This
  // is done in accordance with the implementation of FillRect which also marks an area 1
  // larger than the rectangle being filled. This might well be a bug as discussed in this
  // Github issue:
  // https://github.com/graphics32/graphics32/issues/157#issuecomment-817732999
  // but until we are sure it is a bug and not on purpose we do the same here to be safe.
  CheckEquals(16, ChangeArea.Right);
  CheckEquals(11, ChangeArea.Bottom);
end;

procedure TTestEllipse.FillEllipseT_WorksForArbitrarySizes;
const
  MaxSize = 15;
var
  X, Y, W, H: Integer;
begin
  Want.LoadFromFile('gold_ellipses_in_all_sizes.bmp');
  Have.SetSize(Want.Width, Want.Height);
  // The cmMerge mode makes the half-transparent red on black background appear full red,
  // thus we can use the same gold bitmap for comparison.
  Have.CombineMode := cmMerge;

  Y := 1;
  for H := 1 to MaxSize do
  begin
    X := 1;
    for W := 1 to MaxSize do
    begin
      Have.FillEllipseT(X, Y, X + W, Y + H, $80FF0000);
      Inc(X, W + 1);
    end;
    Inc(Y, H + 1);
  end;

  CheckBitmapsEqual(Want, Have);
end;

procedure TTestEllipse.FillEllipseT_CanBlendEllipses;
begin
  Want.LoadFromFile('gold_blend_ellipses.bmp');

  Have.SetSize(26, 17);
  Have.CombineMode := cmBlend;
  Have.FillEllipseT(1, 1, 16, 16, $80FF0000);
  Have.FillEllipseT(10, 1, 25, 16, $800000FF);

  CheckBitmapsEqual(Want, Have);
end;

procedure TTestEllipse.FillEllipseT_CanMergeEllipses;
begin
  Want.LoadFromFile('gold_merge_ellipses.bmp');

  Have.SetSize(26, 17);
  Have.CombineMode := cmMerge;
  Have.FillEllipseT(1, 1, 16, 16, $80FF0000);
  Have.FillEllipseT(10, 1, 25, 16, $800000FF);

  CheckBitmapsEqual(Want, Have);
end;

procedure TTestEllipse.FillEllipseT_WithFullOpacityBehavesLikeFillEllipse;
begin
  Want.SetSize(20, 20);
  Have.SetSize(20, 20);
  Have.FillEllipseT(1, 1, 19, 19, $FFFF0000);
  Want.FillEllipse(1, 1, 19, 19, $FFFF0000);
  CheckBitmapsEqual(Want, Have);
end;

procedure TTestEllipse.FillEllipseT_WithFullTransparencyDrawsNothing;
begin
  Want.SetSize(20, 20);
  Have.SetSize(20, 20);
  Have.FillEllipseT(1, 1, 19, 19, $00FF0000);
  CheckBitmapsEqual(Want, Have);
end;

procedure TTestEllipse.FillEllipseT_HandlesBackendBitsBeingNil;
begin
  Have.SetSize(20, 20);
  Have.Backend := NilBackend.Create;
  Have.FillEllipseT(1, 1, 19, 19, $88FF0000);
end;

procedure TTestEllipse.FillEllipseT_InMeasuringModeDrawsNothing;
begin
  Want.SetSize(20, 20);

  Have.SetSize(20, 20);
  Have.BeginMeasuring(nil);
  // Use different alpha values.
  Have.FillEllipseT(1, 1, 10, 10, $FFFF0000);
  Have.FillEllipseT(10, 10, 19, 19, $80FF0000);
  Have.FillEllipseT(1, 1, 19, 19, $00FF0000);
  Have.EndMeasuring;

  CheckBitmapsEqual(Want, Have);
end;

procedure TTestEllipse.FillEllipseT_InMeasuringModeChangesBoundingRectangle;
begin
  Have.SetSize(20, 20);

  // Fully opaque.
  Have.BeginMeasuring(RememberLastChangeEvent);
  Have.FillEllipseT(1, 2, 15, 10, $FFFF0000);
  Have.EndMeasuring;
  CheckEquals(1, ChangeCount);
  CheckEquals(AREAINFO_RECT, ChangeInfo);
  CheckEquals(1, ChangeArea.Left);
  CheckEquals(2, ChangeArea.Top);
  CheckEquals(16, ChangeArea.Right);
  CheckEquals(11, ChangeArea.Bottom);

  // Semi transparent.
  Have.BeginMeasuring(RememberLastChangeEvent);
  Have.FillEllipseT(2, 3, 14, 9, $88FF0000);
  Have.EndMeasuring;
  CheckEquals(2, ChangeCount);
  CheckEquals(AREAINFO_RECT, ChangeInfo);
  CheckEquals(2, ChangeArea.Left);
  CheckEquals(3, ChangeArea.Top);
  CheckEquals(15, ChangeArea.Right);
  CheckEquals(10, ChangeArea.Bottom);

  // Fully transparent ellipses do not change anything.
  Have.BeginMeasuring(RememberLastChangeEvent);
  Have.FillEllipseT(3, 4, 13, 8, $00FF0000);
  Have.EndMeasuring;
  CheckEquals(2, ChangeCount); // Same as before.
end;

procedure TTestEllipse.FillEllipseS_WorksForArbitrarySizes;
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
      Have.FillEllipseS(X, Y, X + W, Y + H, clRed32);
      Inc(X, W + 1);
    end;
    Inc(Y, H + 1);
  end;

  CheckBitmapsEqual(Want, Have);
end;

procedure TTestEllipse.FillEllipseS_ClipsEllipses;
begin
  Want.LoadFromFile('gold_clip_rect_ellipses.bmp');

  Have.SetSize(22, 20);
  Have.ClipRect := MakeRect(1, 2, 20, 16);
  Have.FillEllipseS(-6, -6, 9, 9, clRed32);
  Have.FillEllipseS(14, -6, 29, 9, clRed32);
  Have.FillEllipseS(-6, 12, 9, 27, clRed32);
  Have.FillEllipseS(14, 12, 29, 27, clRed32);

  CheckBitmapsEqual(Want, Have);
end;

procedure TTestEllipse.FillEllipseS_OnZeroSizedBitmapDoesNothing;
begin
  Want.SetSize(0, 0);
  Have.SetSize(0, 0);
  Have.FillEllipseS(-10, -10, 10, 10, clRed32);
  CheckBitmapsEqual(Want, Have);
end;

procedure TTestEllipse.FillEllipseS_HandlesBackendBitsBeingNil;
begin
  Have.SetSize(20, 20);
  Have.Backend := NilBackend.Create;
  Have.FillEllipseS(1, 1, 19, 19, clRed32);
end;

procedure TTestEllipse.FillEllipseS_DoesNotDrawInvalidEllipses;
begin
  Have.SetSize(20, 20);
  Want.SetSize(20, 20);

  // Regression: this ellipse provokes a bug from the past.
  Have.FillEllipseS(-90, -90, 10, 10, clFuchsia32);

  Have.FillEllipseS(1, 1, 1, 5, clRed32); // 0 wide.
  Have.FillEllipseS(1, 1, 5, 1, clRed32); // 0 high.
  Have.FillEllipseS(1, 1, 0, 5, clRed32); // Negative width.
  Have.FillEllipseS(1, 1, 5, 0, clRed32); // Negative height.

  // Ellipses outside the clipping rectangle.
  Have.ClipRect := MakeRect(5, 6, 10, 11);
  Have.FillEllipseS(0, 0, 5, 20, clRed32);
  Have.FillEllipseS(0, 0, 20, 6, clRed32);
  Have.FillEllipseS(10, 0, 20, 20, clRed32);
  Have.FillEllipseS(0, 11, 20, 20, clRed32);
end;

procedure TTestEllipse.FillEllipseS_InMeasuringModeDrawsNothing;
begin
  Want.SetSize(20, 20);

  Have.SetSize(20, 20);
  Have.BeginMeasuring(nil);
  Have.FillEllipseS(1, 1, 10, 10, clRed32);
  Have.EndMeasuring;

  CheckBitmapsEqual(Want, Have);
end;

procedure TTestEllipse.FillEllipseS_InMeasuringModeChangesBoundingRectangle;
begin
  Have.SetSize(20, 20);
  Have.BeginMeasuring(RememberLastChangeEvent);
  Have.FillEllipseS(1, 2, 15, 10, clRed32);
  Have.EndMeasuring;
  CheckEquals(1, ChangeCount);
  CheckEquals(AREAINFO_RECT, ChangeInfo);
  CheckEquals(1, ChangeArea.Left);
  CheckEquals(2, ChangeArea.Top);
  CheckEquals(16, ChangeArea.Right);
  CheckEquals(11, ChangeArea.Bottom);
end;

procedure TTestEllipse.FillEllipseS_MeasuresOnlyClippedRectangle;
begin
  Have.SetSize(20, 20);
  Have.BeginMeasuring(RememberLastChangeEvent);
  Have.FillEllipseS(-10, -10, 30, 30, clRed32);
  Have.EndMeasuring;
  CheckEquals(1, ChangeCount);
  CheckEquals(AREAINFO_RECT, ChangeInfo);
  CheckEquals(0, ChangeArea.Left);
  CheckEquals(0, ChangeArea.Top);
  CheckEquals(21, ChangeArea.Right);
  CheckEquals(21, ChangeArea.Bottom);
end;

procedure TTestEllipse.FillEllipseS_HasOverloadTakingRectangle;
begin
  Want.SetSize(20, 20);
  Want.FillEllipseS(5, 5, 30, 30, clRed32);

  Have.SetSize(20, 20);
  Have.FillEllipseS(MakeRect(5, 5, 30, 30), clRed32);

  CheckBitmapsEqual(Want, Have);
end;

procedure TTestEllipse.FillEllipseTS_WorksForArbitrarySizes;
const
  MaxSize = 15;
var
  X, Y, W, H: Integer;
begin
  Want.LoadFromFile('gold_ellipses_in_all_sizes.bmp');
  Have.SetSize(Want.Width, Want.Height);
  // The cmMerge mode makes the half-transparent red on black background appear full red,
  // thus we can use the same gold bitmap for comparison.
  Have.CombineMode := cmMerge;

  Y := 1;
  for H := 1 to MaxSize do
  begin
    X := 1;
    for W := 1 to MaxSize do
    begin
      Have.FillEllipseTS(X, Y, X + W, Y + H, $80FF0000);
      Inc(X, W + 1);
    end;
    Inc(Y, H + 1);
  end;

  CheckBitmapsEqual(Want, Have);
end;

procedure TTestEllipse.FillEllipseTS_ClipsBlendedEllipses;
begin
  Want.LoadFromFile('gold_blend_clipped_ellipses.bmp');

  Have.SetSize(26, 17);
  Have.ClipRect := MakeRect(2, 3, 2 + 17, 3 + 12);
  Have.CombineMode := cmBlend;
  Have.FillEllipseTS(1, 1, 16, 16, $80FF0000);
  Have.FillEllipseTS(10, 1, 26, 16, $800000FF);

  CheckBitmapsEqual(Want, Have);
end;

procedure TTestEllipse.FillEllipseTS_ClipsMrgedEllipses;
begin
  Want.LoadFromFile('gold_merge_clipped_ellipses.bmp');

  Have.SetSize(26, 17);
  Have.ClipRect := MakeRect(2, 3, 2 + 17, 3 + 12);
  Have.CombineMode := cmMerge;
  Have.FillEllipseTS(1, 1, 16, 16, $80FF0000);
  Have.FillEllipseTS(10, 1, 26, 16, $800000FF);

  CheckBitmapsEqual(Want, Have);
end;

procedure TTestEllipse.FillEllipseTS_OnZeroSizedBitmapDoesNothing;
begin
  Want.SetSize(0, 0);
  Have.SetSize(0, 0);
  Have.FillEllipseTS(-10, -10, 10, 10, $88FF0000);
  CheckBitmapsEqual(Want, Have);
end;

procedure TTestEllipse.FillEllipseTS_WithFullOpacityBehavesLike_FillEllipseS;
begin
  Want.SetSize(20, 20);
  Have.SetSize(20, 20);
  Have.FillEllipseTS(5, 5, 30, 30, $FFFF0000);
  Want.FillEllipseS(5, 5, 30, 30, $FFFF0000);
  CheckBitmapsEqual(Want, Have);
end;

procedure TTestEllipse.FillEllipseTS_WithFullTransparencyDrawsNothing;
begin
  Want.SetSize(20, 20);
  Have.SetSize(20, 20);
  Have.FillEllipseTS(1, 1, 19, 19, $00FF0000);
  CheckBitmapsEqual(Want, Have);
end;

procedure TTestEllipse.FillEllipseTS_HandlesBackendBitsBeingNil;
begin
  Have.SetSize(20, 20);
  Have.Backend := NilBackend.Create;
  Have.FillEllipseTS(1, 1, 19, 19, $88FF0000);
end;

procedure TTestEllipse.FillEllipseTS_DoesNotDrawInvalidEllipses;
begin
  Have.SetSize(20, 20);
  Want.SetSize(20, 20);

  Have.FillEllipseTS(1, 1, 1, 5, $88FF0000); // 0 wide.
  Have.FillEllipseTS(1, 1, 5, 1, $88FF0000); // 0 high.
  Have.FillEllipseTS(1, 1, 0, 5, $88FF0000); // Negative width.
  Have.FillEllipseTS(1, 1, 5, 0, $88FF0000); // Negative height.

  // Ellipses outside the clipping rectangle.
  Have.ClipRect := MakeRect(5, 6, 10, 11);
  Have.FillEllipseTS(0, 0, 5, 20, $88FF0000);
  Have.FillEllipseTS(0, 0, 20, 6, $88FF0000);
  Have.FillEllipseTS(10, 0, 20, 20, $88FF0000);
  Have.FillEllipseTS(0, 11, 20, 20, $88FF0000);
end;

procedure TTestEllipse.FillEllipseTS_InMeasuringModeDrawsNothing;
begin
  Want.SetSize(20, 20);

  Have.SetSize(20, 20);
  Have.BeginMeasuring(nil);
  // Use different alpha values.
  Have.FillEllipseTS(5, 5, 30, 30, $FFFF0000);
  Have.FillEllipseTS(10, 10, 40, 40, $80FF0000);
  Have.FillEllipseTS(15, 15, 50, 50, $00FF0000);
  Have.EndMeasuring;

  CheckBitmapsEqual(Want, Have);
end;

procedure TTestEllipse.FillEllipseTS_InMeasuringModeChangesBoundingRectangle;
begin
  Have.SetSize(20, 20);

  // Fully opaque.
  Have.BeginMeasuring(RememberLastChangeEvent);
  Have.FillEllipseTS(1, 2, 15, 10, $FFFF0000);
  Have.EndMeasuring;
  CheckEquals(1, ChangeCount);
  CheckEquals(AREAINFO_RECT, ChangeInfo);
  CheckEquals(1, ChangeArea.Left);
  CheckEquals(2, ChangeArea.Top);
  CheckEquals(16, ChangeArea.Right);
  CheckEquals(11, ChangeArea.Bottom);

  // Semi transparent.
  Have.BeginMeasuring(RememberLastChangeEvent);
  Have.FillEllipseTS(2, 3, 14, 9, $88FF0000);
  Have.EndMeasuring;
  CheckEquals(2, ChangeCount);
  CheckEquals(AREAINFO_RECT, ChangeInfo);
  CheckEquals(2, ChangeArea.Left);
  CheckEquals(3, ChangeArea.Top);
  CheckEquals(15, ChangeArea.Right);
  CheckEquals(10, ChangeArea.Bottom);

  // Fully transparent ellipses do not change anything.
  Have.BeginMeasuring(RememberLastChangeEvent);
  Have.FillEllipseTS(3, 4, 13, 8, $00FF0000);
  Have.EndMeasuring;
  CheckEquals(2, ChangeCount); // Same as before.
end;

procedure TTestEllipse.FillEllipseTS_MeasuresOnlyClippedRectangle;
begin
  Have.SetSize(20, 20);

  // Fully opaque.
  Have.BeginMeasuring(RememberLastChangeEvent);
  Have.FillEllipseTS(-10, -10, 30, 30, $FFFF0000);
  Have.EndMeasuring;
  CheckEquals(1, ChangeCount);
  CheckEquals(AREAINFO_RECT, ChangeInfo);
  CheckEquals(0, ChangeArea.Left);
  CheckEquals(0, ChangeArea.Top);
  CheckEquals(21, ChangeArea.Right);
  CheckEquals(21, ChangeArea.Bottom);

  // Semi transparent.
  Have.BeginMeasuring(RememberLastChangeEvent);
  Have.FillEllipseTS(-10, -10, 30, 30, $88FF0000);
  Have.EndMeasuring;
  CheckEquals(2, ChangeCount);
  CheckEquals(AREAINFO_RECT, ChangeInfo);
  CheckEquals(0, ChangeArea.Left);
  CheckEquals(0, ChangeArea.Top);
  CheckEquals(21, ChangeArea.Right);
  CheckEquals(21, ChangeArea.Bottom);

  // Fully transparent ellipses do not change anything.
  Have.BeginMeasuring(RememberLastChangeEvent);
  Have.FillEllipseTS(-10, -10, 30, 30, $00FF0000);
  Have.EndMeasuring;
  CheckEquals(2, ChangeCount); // Same as before.
end;

procedure TTestEllipse.FillEllipseTS_HasOverloadTakingRectangle;
begin
  Want.SetSize(20, 20);
  Want.FillEllipseTS(5, 5, 30, 30, $88FF0000);

  Have.SetSize(20, 20);
  Have.FillEllipseTS(MakeRect(5, 5, 30, 30), $88FF0000);

  CheckBitmapsEqual(Want, Have);
end;

procedure TTestEllipse.FillRect_Benchmark;
var
  Watch: TStopwatch;
  X, Y: Integer;
begin
  Have.SetSize(1000, 1000);
  Watch := TStopwatch.StartNew;

  for Y := 0 to 100 - 10 do
    for X := 0 to 100 - 10 do
      Have.FillRect(X * 10, Y * 10, X * 10 + 100, Y * 10 + 100, clFuchsia32);

  for Y := 0 to 100 - 5 do
    for X := 0 to 100 - 5 do
      Have.FillRect(X * 10, Y * 10, X * 10 + 50, Y * 10 + 50, clRed32);

  for Y := 0 to 100 - 1 do
    for X := 0 to 100 - 1 do
      Have.FillRect(X * 10, Y * 10, X * 10 + 10, Y * 10 + 10, clBlue32);

  Watch.Stop;
  Have.SaveToFile('FillRect_Benchmark.bmp');
  Fail(Format('FillRect took %d ms', [Watch.ElapsedMilliseconds]));
end;

procedure TTestEllipse.FillEllipse_Benchmark;
var
  Watch: TStopwatch;
  X, Y: Integer;
begin
  Have.SetSize(1000, 1000);
  Watch := TStopwatch.StartNew;

  for Y := 0 to 100 - 10 do
    for X := 0 to 100 - 10 do
      Have.FillEllipse(X * 10, Y * 10, X * 10 + 100, Y * 10 + 100, clFuchsia32);

  for Y := 0 to 100 - 5 do
    for X := 0 to 100 - 5 do
      Have.FillEllipse(X * 10, Y * 10, X * 10 + 50, Y * 10 + 50, clRed32);

  for Y := 0 to 100 - 1 do
    for X := 0 to 100 - 1 do
      Have.FillEllipse(X * 10, Y * 10, X * 10 + 10, Y * 10 + 10, clBlue32);

  Watch.Stop;
  Have.SaveToFile('FillEllipse_Benchmark.bmp');
  Fail(Format('FillEllipse took %d ms', [Watch.ElapsedMilliseconds]));
end;

procedure TTestEllipse.FillEllipseT_Benchmark;
var
  Watch: TStopwatch;
  X, Y: Integer;
begin
  Have.SetSize(1000, 1000);
  Watch := TStopwatch.StartNew;

  for Y := 0 to 100 - 10 do
    for X := 0 to 100 - 10 do
      Have.FillEllipseT(X * 10, Y * 10, X * 10 + 100, Y * 10 + 100, $66FF00FF);

  for Y := 0 to 100 - 5 do
    for X := 0 to 100 - 5 do
      Have.FillEllipseT(X * 10, Y * 10, X * 10 + 50, Y * 10 + 50, $55FF0000);

  for Y := 0 to 100 - 1 do
    for X := 0 to 100 - 1 do
      Have.FillEllipseT(X * 10, Y * 10, X * 10 + 10, Y * 10 + 10, $440000FF);

  Watch.Stop;
  Have.SaveToFile('FillEllipseT_Benchmark.bmp');
  Fail(Format('FillEllipseT took %d ms', [Watch.ElapsedMilliseconds]));
end;

procedure TTestEllipse.TCavas32_Ellipse_Benchmark;
var
  Watch: TStopwatch;
  C: TCanvas32;
  Brush: TSolidBrush;
  X, Y: Integer;
begin
  Have.SetSize(1000, 1000);
  C := TCanvas32.Create(Have);
  Brush := C.Brushes.Add(TSolidBrush) as TSolidBrush;

  Watch := TStopwatch.StartNew;

  Brush.FillColor := clFuchsia32;
  for Y := 0 to 100 - 10 do
    for X := 0 to 100 - 10 do
      C.Ellipse(X * 10 + 50, Y * 10 + 50, 50, 50, 50);

  Brush.FillColor := clRed32;
  for Y := 0 to 100 - 5 do
    for X := 0 to 100 - 5 do
      C.Ellipse(X * 10 + 25, Y * 10 + 25, 25, 25, 25);

  Brush.FillColor := clBlue32;
  for Y := 0 to 100 - 1 do
    for X := 0 to 100 - 1 do
      C.Ellipse(X * 10 + 5, Y * 10 + 5, 5, 5, 5);

  Watch.Stop;
  Have.SaveToFile('TCavas32_Ellipse_Benchmark.bmp');
  C.Free;
  Fail(Format('TCavas32.Ellipse took %d ms', [Watch.ElapsedMilliseconds]));
end;

procedure TTestEllipse.FillEllipseS_Benchmark;
var
  Watch: TStopwatch;
  X, Y: Integer;
begin
  Have.SetSize(1000, 1000);
  Watch := TStopwatch.StartNew;

  for Y := -10 to 100 do
    for X := -10 to 100 do
      Have.FillEllipseS(X * 10, Y * 10, X * 10 + 100, Y * 10 + 100, clFuchsia32);

  for Y := -10 to 100 do
    for X := -10 to 100 do
      Have.FillEllipseS(X * 10, Y * 10, X * 10 + 50, Y * 10 + 50, clRed32);

  for Y := -10 to 100 do
    for X := -10 to 100 do
      Have.FillEllipseS(X * 10, Y * 10, X * 10 + 10, Y * 10 + 10, clBlue32);

  Watch.Stop;
  Have.SaveToFile('FillEllipseS_Benchmark.bmp');
  Fail(Format('FillEllipseS took %d ms', [Watch.ElapsedMilliseconds]));
end;

procedure TTestEllipse.FillEllipseTS_Benchmark;
var
  Watch: TStopwatch;
  X, Y: Integer;
begin
  Have.SetSize(1000, 1000);
  Watch := TStopwatch.StartNew;

  for Y := -10 to 100 do
    for X := -10 to 100 do
      Have.FillEllipseTS(X * 10, Y * 10, X * 10 + 100, Y * 10 + 100, $66FF00FF);

  for Y := -10 to 100 do
    for X := -10 to 100 do
      Have.FillEllipseTS(X * 10, Y * 10, X * 10 + 50, Y * 10 + 50, $55FF0000);

  for Y := -10 to 100 do
    for X := -10 to 100 do
      Have.FillEllipseTS(X * 10, Y * 10, X * 10 + 10, Y * 10 + 10, $440000FF);

  Watch.Stop;
  Have.SaveToFile('FillEllipseTS_Benchmark.bmp');
  Fail(Format('FillEllipseTS took %d ms', [Watch.ElapsedMilliseconds]));
end;

procedure TTestEllipse.Compare_FillEllipse_And_TCanvas32_Ellipse;
var
  C: TCanvas32;
  Brush: TSolidBrush;
begin
  Have.SetSize(20, 20);
  Have.FillEllipse(1, 1, 19, 19, clRed32);

  Want.SetSize(20, 20);
  C := TCanvas32.Create(Want);
  Brush := C.Brushes.Add(TSolidBrush) as TSolidBrush;
  Brush.FillColor := clRed32;
  C.Ellipse(10, 10, 9, 9);
  C.Free;

  CheckBitmapsEqual(Want, Have);
end;

initialization

TestFrameWork.RegisterTest(TTestEllipse.Suite);

end.