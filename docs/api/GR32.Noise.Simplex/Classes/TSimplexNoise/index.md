---
layout: doc
docType: api
unit: GR32.Noise.Simplex
entity: TSimplexNoise
kind: Class
declaration: |
  TSimplexNoise = class(TObject)
inheritance:
  - TObject
  - TSimplexNoise
summary: "Generates smooth, deterministic 2D, 3D, and 4D Simplex Noise values based on seed-initialized permutation tables."
---

## Description

`TSimplexNoise` provides a fast, seedable implementation of multi-dimensional Simplex Noise in Pascal. It allows applications to sample smooth pseudo-random scalar noise across 2D, 3D, and 4D spaces using the overloaded [[Noise]] method.

When an instance of `TSimplexNoise` is created, its internal 512-byte permutation table is initialized using a 64-bit integer seed value ([[Seed]]) and a linear congruential pseudo-random generator configured by the class property [[SeedMult]]. Instances initialized with the same seed will produce identical noise patterns across all platforms.

## Example

```pascal
var
  Simplex: TSimplexNoise;
  Bitmap: TBitmap32;
  x, y: Integer;
  Val: Double;
  IntensityByte: Byte;
begin
  Bitmap := TBitmap32.Create(256, 256);
  Simplex := TSimplexNoise.Create(12345); // Deterministic seed
  try
    for y := 0 to Bitmap.Height - 1 do
    begin
      for x := 0 to Bitmap.Width - 1 do
      begin
        // Sample 2D noise scaled to spatial frequency
        Val := Simplex.Noise(x * 0.01, y * 0.01); // Val in range [-1.0, 1.0]

        // Map [-1.0, 1.0] to [0, 255] byte intensity
        IntensityByte := Round((Val + 1.0) * 0.5 * 255);
        Bitmap.Pixel[x, y] := Gray32(IntensityByte);
      end;
    end;
  finally
    Simplex.Free;
    Bitmap.Free;
  end;
end;
```

![2D Simplex Noise Bitmap](/images/simplex-noise-2d.png)

[members]
