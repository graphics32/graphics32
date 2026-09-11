---
title: "3. Fast Pixel Access & Direct Manipulation"
---

# Fast Pixel Access & Direct Manipulation

When building real-time image processing, games, procedural textures, or computer vision algorithms, standard drawing functions can be too slow. Graphics32 gives you direct pointer access to the raw 32-bit pixel array, allowing high-speed pixel manipulation that runs at native C-like speed.

In this tutorial, you will learn the three ways to access pixels in `TBitmap32`, ranging from simple coordinate indexing to maximum-performance pointer iteration.

---

## 1. Pixel vs PixelS Access

`TBitmap32` overall provides three 2D properties for reading and writing pixels by $(X, Y)$ coordinate:

- **`Pixel[X, Y]`**: Direct access without bounds checking. Extremely fast, but passing coordinates outside $[0..Width-1, 0..Height-1]$ will cause memory access violations or corrupt memory!
- **`PixelS[X, Y]`**: Safe access with automatic clipping. If $(X, Y)$ falls outside the bitmap bounds, reads return `$00000000` (transparent black) by default[^1] and writes are safely ignored.
- **`PixelW[X, Y]`**: Safe access with automatic wrapping. If $(X, Y)$ falls outside the bitmap bounds, the pixel coordinates wrap according to the value of the `WrapMode` property (clamp, repeat, or mirror/reflect).

[^1]: The color value return by out of bounds pixel reads can be configured with the [[TBitmap32.OuterColor|OuterColor]] property. The default is `clNone32` = `$00000000`.

```pascal
var
  Bitmap: TBitmap32;
  Color: TColor32;
begin
  Bitmap := TBitmap32.Create;
  try
    Bitmap.SetSize(100, 100);

    // 1. Safe access - checks bounds automatically
    // Safe even if coordinates are outside bounds
    Bitmap.PixelS[150, -10] := clRed32; // Ignored safely without crashing

    // 2. Unchecked access - fast, but requires valid coordinates!
    Bitmap.Pixel[50, 50] := clBlue32;
    Color := Bitmap.Pixel[50, 50];

    // 3. Wrapped access - ensures out-of-bounds coordinates become in-bounds
    Bitmap.WrapMode := wmClamp;
    Bitmap.PixelW[150, 150] := clGreen32; // Sets the [99, 99] pixel
    Color := Bitmap.Pixel[-50, 50]; // Reads the [0, 50] pixel

  finally
    Bitmap.Free;
  end;
end;
```

---

## 2. Fast Row Pointer Iteration with ScanLine

While `Pixel[X, Y]` is convenient, accessing 2D coordinates in nested loops incurs coordinate arithmetic overhead. For image processing loops, the standard Graphics32 pattern uses `ScanLine[Y]`.

`ScanLine[Y]` returns a `PColor32Array` pointer directly targeting the first pixel of row $Y$.

```pascal
procedure ConvertToGrayscale(Bitmap: TBitmap32);
var
  X, Y: Integer;
  Row: PColor32Array;
  Color: TColor32;
  R, G, B, A, Gray: Byte;
begin
  for Y := 0 to Bitmap.Height - 1 do
  begin
    // Obtain direct row pointer for row Y
    Row := Bitmap.ScanLine[Y];

    for X := 0 to Bitmap.Width - 1 do
    begin
      Color := Row[X];

      // Extract ARGB channels
      R := RedComponent(Color);
      G := GreenComponent(Color);
      B := BlueComponent(Color);
      A := AlphaComponent(Color);

      // Standard perceived luminance weighting: 0.299*R + 0.587*G + 0.114*B
      // Same as: Intensity(Color)
      Gray := Round(0.299 * R + 0.587 * G + 0.114 * B);

      // Write gray pixel back preserving original alpha channel
      // Same as: Gray32(Gray, A)
      Row[X] := Color32(Gray, Gray, Gray, A);
    end;
  end;

  Bitmap.Changed; // Notify control/listeners that pixels changed
end;
```

---

## 3. Flat Memory Access with Bits Array

In `TBitmap32`, pixels are stored sequentially in a contiguous memory block row by row (top-to-bottom, left-to-right). You can access this entire memory buffer as a flat 1D array via the `Bits` pointer property (`PColor32Array`).

This approach eliminates the row loop entirely, enabling ultra-fast whole-image operations!

```pascal
procedure InvertColors(Bitmap: TBitmap32);
var
  P: PColor32Array;
  I, TotalPixels: Integer;
  Color: TColor32;
  R, G, B, A: Byte;
begin
  TotalPixels := Bitmap.Width * Bitmap.Height;
  if TotalPixels = 0 then
    Exit;

  // Get pointer to the start of the entire pixel buffer
  P := Bitmap.Bits;

  for I := 0 to TotalPixels - 1 do
  begin
    Color := P[I];

    // Extract channels
    R := RedComponent(Color);
    G := GreenComponent(Color);
    B := BlueComponent(Color);
    A := AlphaComponent(Color);

    // Invert RGB channels while preserving original Alpha
    P[I] := Color32(255 - R, 255 - G, 255 - B, A);

    // We actually have a function that does exactly this,
    // called InvertColor, but you get the picture - so to say.
  end;

  Bitmap.Changed;
end;
```

::: info
Unlike `TBitmap`, which store pixel data in either top-down or bottom-up order (and bottom-up by default), `TBitmap32` **always** store pixel data in top-down order.
:::

---

## 4. Adjusting Brightness & Contrast Example

Here is a complete practical snippet demonstrating brightness adjustment using flat `Bits` pointer iteration:

```pascal
uses
  GR32_LowLevel; // required for the Clamp() function

procedure AdjustBrightness(Bitmap: TBitmap32; Amount: Integer);
var
  P: PColor32Array;
  I, TotalPixels: Integer;
  Color: TColor32;
  R, G, B, A: Byte;
begin
  TotalPixels := Bitmap.Width * Bitmap.Height;
  if TotalPixels = 0 then
    Exit;

  P := Bitmap.Bits;
  for I := 0 to TotalPixels - 1 do
  begin
    Color := P[I];
    Color32Components(Color, R, G, B, A);

    // Add brightness offset and clamp to valid 8-bit range
    R := Clamp(R + Amount);
    G := Clamp(G + Amount);
    B := Clamp(B + Amount);

    P[I] := Color32(R, G, B, A);
  end;

  Bitmap.Changed;
end;
```

And here is another snippet demonstrating contrast adjustment. We use a slightly different technique here for better performance: A color lookup table:

```pascal
uses
  GR32_LowLevel;

procedure AdjustContrast(Bitmap: TBitmap32; AContrast: Integer);
var
  Count: Integer;
  LUT: array[Byte] of Byte;
  Factor: Single;
  P: PColor32Entry;
  I, TotalPixels: Integer;
begin
  Count := Bitmap.Width * Bitmap.Height;
  if Count = 0 then
    Exit;

  // 1. Calculate the contrast factor
  //    AContrast values range from -100 to 100 (0 means no change)
  Factor := (259 * (AContrast + 255)) / (255 * (259 - AContrast));

  // 2. Build a Lookup Table (LUT) for performance
  for I := 0 to 255 do
    LUT[I] := Clamp(Round(Factor * (I - 128) + 128));

  // 3. Apply the LUT directly to the pixel buffer
  P := PColor32Entry(ABitmap.Bits);

  while (Count > 0) do
  begin
    // Modify R, G, and B components while preserving Alpha
    P.R := LUT[P.R];
    P.G := LUT[P.G];
    P.B := LUT[P.B];

    // Move on to next pixel
    Inc(P);
    Dec(Count);
  end;

  Bitmap.Changed;
end;
```

---

## Performance Comparison Summary

| Access Method | Safety | Relative Speed | Recommended Use Case |
| :--- | :--- | :--- | :--- |
| `PixelS[X, Y]` | Bounds checked | Moderate | Single pixel tweaks, user clicks, UI bounds safety |
| `PixelW[X, Y]` | Bounds checked | Moderate | Same |
| `Pixel[X, Y]` | Unchecked | Fast | Simple 2D algorithms with verified bounds |
| `ScanLine[Y]` | Row pointer | Very Fast | 2D filters, convolution matrices, line-by-line processing |
| `Bits` | Flat buffer | Maximum | 1D filters, whole-image operations, color transforms, blits |

---

## Summary

::: box-green

In this tutorial, you learned:
1. The difference between bounds-checked `PixelS` and `PixelW`, and unchecked `Pixel`.
2. How to process images line-by-line using `ScanLine[Y]` pointers.
3. How to process the whole bitmap in a single loop using `Bits`.
4. How to perform custom image processing algorithms (grayscale, inversion, brightness and contrast adjustment).

:::

Next, check out [Image Resampling & High-Quality Scaling](./resampling-and-scaling) to see how Graphics32 resamples and resizes bitmaps!
