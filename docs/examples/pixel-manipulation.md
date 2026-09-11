---
title: "Fast Pixel Access & Direct Manipulation"
---

# Fast Pixel Access & Direct Manipulation

When building real-time image processing, games, procedural textures, or computer vision algorithms, standard drawing functions can be too slow. Graphics32 gives you direct pointer access to the raw 32-bit pixel array, allowing high-speed pixel manipulation that runs at native C-like speed.

In this tutorial, you will learn the three ways to access pixels in `TBitmap32`, ranging from simple coordinate indexing to maximum-performance pointer iteration.

---

## 1. Pixel vs PixelS Access

`TBitmap32` provides two 2D default properties for reading and writing pixels by $(X, Y)$ coordinate:

- **`Pixel[X, Y]`**: Direct access without bounds checking. Extremely fast, but passing coordinates outside $[0..Width-1, 0..Height-1]$ will cause memory access violations or corrupt memory!
- **`PixelS[X, Y]`**: Safe access with automatic clipping. If $(X, Y)$ falls outside the bitmap bounds, reads return `$00000000` (transparent black) by default[^1] and writes are safely ignored.

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
  // Lock or begin update batching
  Bitmap.BeginUpdate;
  try
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
  finally
    Bitmap.EndUpdate;
  end;
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
  if TotalPixels = 0 then Exit;

  Bitmap.BeginUpdate;
  try
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
  finally
    Bitmap.EndUpdate;
  end;
end;
```

---

## 4. Adjusting Brightness & Contrast Example

Here is a complete practical snippet demonstrating brightness adjustment using flat `Bits` pointer iteration:

```pascal
procedure AdjustBrightness(Bitmap: TBitmap32; Amount: Integer);
var
  P: PColor32Array;
  I, TotalPixels: Integer;
  Color: TColor32;
  R, G, B, A: Byte;

  // Helper inline function to clamp integer values to [0..255]
  // ...Or simply use the Clamp function in GR32_LowLevel.
  function ClampByte(Val: Integer): Byte;
  begin
    if Val < 0 then
      Result := 0
    else
    if Val > 255 then
      Result := 255
    else
      Result := Byte(Val);
  end;

begin
  TotalPixels := Bitmap.Width * Bitmap.Height;
  if TotalPixels = 0 then Exit;

  Bitmap.BeginUpdate;
  try
    P := Bitmap.Bits;
    for I := 0 to TotalPixels - 1 do
    begin
      Color := P[I];
      Color32Components(Color, R, G, B, A);

      // Add brightness offset and clamp to valid 8-bit range
      R := ClampByte(R + Amount);
      G := ClampByte(G + Amount);
      B := ClampByte(B + Amount);

      P[I] := Color32(R, G, B, A);
    end;

    Bitmap.Changed;
  finally
    Bitmap.EndUpdate;
  end;
end;
```

---

## Performance Comparison Summary

| Access Method | Safety | Relative Speed | Recommended Use Case |
| :--- | :--- | :--- | :--- |
| `PixelS[X, Y]` | Bounds checked | Moderate | Single pixel tweaks, user clicks, UI bounds safety |
| `Pixel[X, Y]` | Unchecked | Fast | Simple 2D algorithms with verified bounds |
| `ScanLine[Y]` | Row pointer | Very Fast | 2D filters, convolution matrices, line-by-line processing |
| `Bits` | Flat buffer | Maximum | Whole-image operations, color transforms, blits |

---

## Summary

In this tutorial, you learned:
1. The difference between bounds-checked `PixelS` and unchecked `Pixel`.
2. How to process images line-by-line using `ScanLine[Y]` pointers.
3. How to process the whole bitmap in a single loop using `Bits`.
4. How to perform custom image processing algorithms (grayscale, inversion, brightness adjustment).

Next, check out [Image Resampling & High-Quality Scaling](./resampling-and-scaling) to see how Graphics32 resamples and resizes bitmaps!
