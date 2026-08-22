# Resampling & Transforms

Graphics32 features an extensible sampling and resampling pipeline for scaling, filtering, and transforming images with high visual quality.

---

## Resamplers

Resamplers determine pixel interpolation quality when stretching bitmaps or performing affine transformations.

### Common Resampler Types
- `TNearestResampler`: Fast, zero interpolation (point sampling).
- `TLinearResampler`: Bilinear interpolation for smooth scaling.
- `TDraftResampler`: Fast draft quality.
- `TKernelResampler`: High-quality windowed sinc / Lanczos / Gaussian filtering.

### Example: High-Quality Scaling

```pascal
uses GR32, GR32_Resamplers;

procedure ScaleBitmap(Source, Dest: TBitmap32);
var
  Resampler: TLinearResampler;
begin
  Resampler := TLinearResampler.Create(Source);
  try
    Source.Resampler := Resampler;
    Source.StretchDraw(Dest.BoundsRect, Dest);
  finally
    Resampler.Free;
  end;
end;
```

---

## Affine Transformations (`GR32_Transforms`)

The `TAffineTransformation` class allows rotation, scaling, skewing, and translation.

```pascal
uses GR32, GR32_Transforms;

procedure RotateBitmap(Source, Dest: TBitmap32; AngleDegrees: Single);
var
  Transform: TAffineTransformation;
begin
  Transform := TAffineTransformation.Create;
  try
    Transform.SrcRect := FloatRect(Source.BoundsRect);
    Transform.Rotate(Source.Width / 2, Source.Height / 2, AngleDegrees);

    // Transform source into dest bitmap
    Transform.Transform(Source, Dest);
  finally
    Transform.Free;
  end;
end;
```
