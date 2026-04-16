unit TestGR32Premultiply;

interface

{$I ..\..\Source\GR32.inc}

uses
{$IFDEF FPC}
  fpcunit, testregistry,
{$ELSE}
  TestFramework,
{$ENDIF}
  SysUtils, Math,
  GR32,
  GR32_Blend,
  GR32_LowLevel;

type
  TTestPremultiply = class(TTestCase)
  private
    procedure ReferencePremultiply(Color: PColor32Entry; Count: Integer);
    procedure ReferenceUnpremultiply(Color: PColor32Entry; Count: Integer);
    procedure CheckImplementation(const Name: string; PremultProc, UnpremultProc: TPremultiplyMem);
  published
    procedure TestBinding;
    procedure TestPascal;
{$if not defined(PUREPASCAL)}
    procedure TestSSE2;
    procedure TestSSE41;
{$ifend}
  end;

implementation

uses
  GR32_Bindings,
  GR32_System,
  GR32.Blend.Pascal,
  GR32.Blend.SSE2;

{ TTestPremultiply }

procedure TTestPremultiply.ReferencePremultiply(Color: PColor32Entry; Count: Integer);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if Color.A = 0 then
      Color.ARGB := 0
    else
    if Color.A < 255 then
    begin
      Color.R := Round(Color.R * Color.A / 255.0);
      Color.G := Round(Color.G * Color.A / 255.0);
      Color.B := Round(Color.B * Color.A / 255.0);
    end;
    Inc(Color);
  end;
end;

procedure TTestPremultiply.ReferenceUnpremultiply(Color: PColor32Entry; Count: Integer);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if Color.A = 0 then
      Color.ARGB := 0
    else
    if Color.A < 255 then
    begin
      Color.R := Clamp(Round(Color.R * 255.0 / Color.A));
      Color.G := Clamp(Round(Color.G * 255.0 / Color.A));
      Color.B := Clamp(Round(Color.B * 255.0 / Color.A));
    end;
    Inc(Color);
  end;
end;

procedure TTestPremultiply.CheckImplementation(const Name: string; PremultProc, UnpremultProc: TPremultiplyMem);
var
  Reference, Expected, Actual, Source: TBitmap32;
  x, y: Integer;
  C1, C2, C3: TColor32Entry;
  r, g, b, a: Byte;
  fracX, fracY: Double;
  Pixel_Ref, Pixel_Exp, Pixel_Act: TColor32Entry;
begin
  Reference := TBitmap32.Create(256, 256);
  Expected := TBitmap32.Create(256, 256);
  Actual := TBitmap32.Create(256, 256);
  Source := TBitmap32.Create(256, 256);
  try
    for y := 0 to 255 do
    begin
      fracY := y / 255.0;
      for x := 0 to 255 do
      begin
        fracX := x / 255.0;

        // Gradient X: $FFFF00FF (Magenta) to $0000FF00 (Green)
        a := Round(255 * (1.0 - fracX));
        r := Round(255 * (1.0 - fracX));
        g := Round(255 * fracX);
        b := Round(255 * (1.0 - fracX));
        C1.A := a; C1.R := r; C1.G := g; C1.B := b;

        // Gradient Y: $FF00FF00 (Green) to $00FF00FF (Magenta)
        a := Round(255 * (1.0 - fracY));
        r := Round(255 * fracY);
        g := Round(255 * (1.0 - fracY));
        b := Round(255 * fracY);
        C2.A := a; C2.R := r; C2.G := g; C2.B := b;

        // Combine for variety
        C3.A := (C1.A + C2.A) div 2;
        C3.R := (C1.R + C2.R) div 2;
        C3.G := (C1.G + C2.G) div 2;
        C3.B := (C1.B + C2.B) div 2;

        Reference.Pixel[x, y] := C3.ARGB;
        Expected.Pixel[x, y] := C3.ARGB;
        Actual.Pixel[x, y] := C3.ARGB;
      end;
    end;

    Reference.CopyMapTo(Source);

    // 2. Premultiply expected using reference
    ReferencePremultiply(pointer(Expected.Bits), 256 * 256);

    // 3. Premultiply actual using tested
    PremultProc(pointer(Actual.Bits), 256 * 256);

    // 4. Compare Premultiply results
    for y := 0 to 255 do
      for x := 0 to 255 do
      begin
        const MaxPremultiplyLoss = 1;
        Pixel_Exp.ARGB := Expected.Pixel[x, y];
        Pixel_Act.ARGB := Actual.Pixel[x, y];
        if (Abs(Pixel_Exp.A - Pixel_Act.A) > 0) or
           (Abs(Pixel_Exp.R - Pixel_Act.R) > MaxPremultiplyLoss) or
           (Abs(Pixel_Exp.G - Pixel_Act.G) > MaxPremultiplyLoss) or
           (Abs(Pixel_Exp.B - Pixel_Act.B) > MaxPremultiplyLoss) then
          Fail(Format('%s: Premultiply failure [%.8X] at (%d,%d). Expected %.8X, Actual %.8X', [Name, Source.Pixel[x, y], x, y, Pixel_Exp.ARGB, Pixel_Act.ARGB]));
      end;

    // Unpremultiply reference's multiplied to avoid error accumulation
    Expected.CopyMapTo(Source);
    Source.CopyMapTo(Actual);

    // 5. Unpremultiply expected using reference
    ReferenceUnpremultiply(pointer(Expected.Bits), 256 * 256);

    // 6. Unpremultiply actual using tested
    UnpremultProc(pointer(Actual.Bits), 256 * 256);

    // 7. Compare Unpremultiply results
    for y := 0 to 255 do
      for x := 0 to 255 do
      begin
        const MaxUnpremultiplyLoss = 2;
        Pixel_Exp.ARGB := Expected.Pixel[x, y];
        Pixel_Act.ARGB := Actual.Pixel[x, y];
        if (Abs(Pixel_Exp.A - Pixel_Act.A) > 0) or
           (Abs(Pixel_Exp.R - Pixel_Act.R) > MaxUnpremultiplyLoss) or
           (Abs(Pixel_Exp.G - Pixel_Act.G) > MaxUnpremultiplyLoss) or
           (Abs(Pixel_Exp.B - Pixel_Act.B) > MaxUnpremultiplyLoss) then
          Fail(Format('%s: Unpremultiply failure [%.8X] at (%d,%d). Expected %.8X, Actual %.8X', [Name, Source.Pixel[x, y], x, y, Pixel_Exp.ARGB, Pixel_Act.ARGB]));
      end;

    // 8. Compare reference bitmap against actual (Round-trip check)
    for y := 0 to 255 do
      for x := 0 to 255 do
      begin
        Pixel_Ref.ARGB := Reference.Pixel[x, y];
        Pixel_Act.ARGB := Actual.Pixel[x, y];

        // Alpha should always be perfectly preserved
        if Pixel_Ref.A <> Pixel_Act.A then
          Fail(Format('%s: Alpha preservation failure [%.8X] at (%d,%d). RefAlpha %d, ActAlpha %d', [Name, Reference.Pixel[x, y], x, y, Pixel_Ref.A, Pixel_Act.A]));

        if Pixel_Ref.A = 0 then
        begin
          if Pixel_Act.ARGB <> 0 then
            Fail(Format('%s: Zero Alpha failure at %d,%d. Expected 0, Actual %.8X', [Name, Reference.Pixel[x, y], x, y, Pixel_Act.ARGB]));
        end else
        begin
          var MaxLoss: integer;
          // Check lossy round-trip only for high enough alpha
          case Pixel_Ref.A of
            0..5:
              MaxLoss := 255;
            6..7:
              MaxLoss := 20;
            8..15:
              MaxLoss := 15;
            16..19:
              MaxLoss := 8;
            20..22:
              MaxLoss := 6;
            23..29:
              MaxLoss := 5;
            30..199:
              MaxLoss := 4;
            200..254:
              MaxLoss := 1;
          else
            MaxLoss := 0;
          end;

          if (Abs(Pixel_Ref.R - Pixel_Act.R) > MaxLoss) or
             (Abs(Pixel_Ref.G - Pixel_Act.G) > MaxLoss) or
             (Abs(Pixel_Ref.B - Pixel_Act.B) > MaxLoss) then
            Fail(Format('%s: Lossy Premult/Unpremult failure [%.8X] at (%d,%d). Ref %.8X, Actual %.8X', [Name, Reference.Pixel[x, y], x, y, Pixel_Ref.ARGB, Pixel_Act.ARGB]));
        end;
      end;

  finally
    Reference.Free;
    Expected.Free;
    Actual.Free;
    Source.Free;
  end;
end;

procedure TTestPremultiply.TestBinding;
begin
  CheckImplementation('Auto', TPremultiplyMem(@PremultiplyMem), TPremultiplyMem(@UnpremultiplyMem));
end;

procedure TTestPremultiply.TestPascal;
begin
{$if declared(PremultiplyMem_Pas) and declared(UnpremultiplyMem_Pas)}
  if (isPascal in CPU.InstructionSupport) then
    CheckImplementation('Pascal', TPremultiplyMem(@PremultiplyMem_Pas), TPremultiplyMem(@UnpremultiplyMem_Pas))
  else
    Status('Pascal not supported');
{$else}
  Fail('PremultiplyMem_Pas or UnpremultiplyMem_Pas not implemented');
{$ifend}

  CheckImplementation('Pascal', TPremultiplyMem(@PremultiplyMem_Pas), TPremultiplyMem(@UnpremultiplyMem_Pas));
end;

{$if not defined(PUREPASCAL)}
procedure TTestPremultiply.TestSSE2;
begin
{$if declared(PremultiplyMem_SSE2) and declared(UnpremultiplyMem_SSE2)}
  if (isSSE2 in CPU.InstructionSupport) then
    CheckImplementation('SSE2', TPremultiplyMem(@PremultiplyMem_SSE2), TPremultiplyMem(@UnpremultiplyMem_SSE2))
  else
    Status('SSE2 not supported');
{$else}
  Fail('PremultiplyMem_SSE2 or UnpremultiplyMem_SSE2 not implemented');
{$ifend}
end;

procedure TTestPremultiply.TestSSE41;
begin
{$if declared(PremultiplyMem_SSE41) and declared(UnpremultiplyMem_SSE41)}
  if (isSSE41 in CPU.InstructionSupport) then
    CheckImplementation('SSE4.1', TPremultiplyMem(@PremultiplyMem_SSE41), TPremultiplyMem(@UnpremultiplyMem_SSE41))
  else
    Status('SSE4.1 not supported');
{$else}
  Fail('PremultiplyMem_SSE41 or UnpremultiplyMem_SSE41 not implemented');
{$ifend}
end;
{$ifend}

initialization
  RegisterTest(TTestPremultiply.Suite);

end.
