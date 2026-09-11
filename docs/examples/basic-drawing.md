---
title: "Basic Drawing & Shapes"
---

# Basic Drawing & Shapes

Welcome to Graphics32! If you are used to working with standard Delphi or Lazarus `TBitmap` or GDI canvases, drawing in Graphics32 (`TBitmap32`) will feel familiar yet significantly more flexible and powerful.

`TBitmap32` provides direct 32-bit pixel storage where each pixel contains Red, Green, Blue, and Alpha channels. In this tutorial, you will learn how to create a bitmap, clear its surface, draw fundamental geometric shapes, and control pen colors.

---

## 1. Creating and Setting Up TBitmap32

Before drawing anything, you need an instance of `TBitmap32`. Unlike standard GDI bitmaps, `TBitmap32` lives entirely in 32-bit memory, making pixel operations fast and consistent across platforms.

```pascal
uses
  Classes, SysUtils, GR32;

var
  Bitmap: TBitmap32;
begin
  // Create an instance of TBitmap32
  Bitmap := TBitmap32.Create;
  try
    // Set width and height (Width = 600, Height = 400)
    // The third parameter, Clear = False, instructs SetSize not to
    // clear the bitmap.
    Bitmap.SetSize(600, 400, False);

    // Clear the background to solid white ($FFFFFFFF)
    Bitmap.Clear(clWhite32);

    // Your drawing operations go here...

    // Save the result to disk
    Bitmap.SaveToFile('drawing_output.png');
  finally
    Bitmap.Free;
  end;
end;
```

### Key Takeaways
- **`SetSize(Width, Height)`**: Allocates memory for specified width and height.
- **`Clear(Color)`**: Fills the entire surface with a single `TColor32` value in one fast memory operation.
- **Memory Management**: Always wrap your bitmap allocation inside a `try...finally` block to prevent memory leaks.

---

## 2. Drawing Lines and Framed Shapes

`TBitmap32` maintains an internal pen state for simple 2D line drawing (`PenColor`, `PenPos`). You can position the virtual pen using `MoveTo` and draw connected line segments using `LineTo` or `LineToAS` (anti-aliased).

```pascal
// Configure pen appearance
Bitmap.PenColor := clBlack32; // Solid black ($FF000000)

// Draw an anti-aliased triangle
Bitmap.MoveTo(100, 300);
Bitmap.LineToAS(300, 100);
Bitmap.LineToAS(500, 300);
Bitmap.LineToAS(100, 300);

// Draw an unfilled (framed) rectangle
// FrameRectS takes integer coordinates (Left, Top, Right, Bottom)
Bitmap.FrameRectS(50, 50, 550, 350, clGray32);
```

### Understanding Anti-Aliasing (`LineToAS`)
Graphics32 offers both standard integer-binned line routines and anti-aliased line routines ending with `AS` (e.g. `LineToAS`). Anti-aliasing blends edge pixels with the background color to produce smooth, non-jagged lines.

---

## 3. Filled Rectangles

Filling areas with color is a common task in graphics applications. `TBitmap32` includes optimized routines for solid rectangle fills.

```pascal
// 1. Solid filled rectangle
// FillRectS(Left, Top, Right, Bottom, Color)
Bitmap.FillRectS(60, 60, 200, 180, clRed32);

// 2. Translucent filled rectangle
// FillRectTS blends the fill color with underlying pixels
Bitmap.FillRectTS(120, 100, 300, 240, clTrBlue32);
```

### Method Naming Convention Quick-Reference
- **`*S` suffix** (e.g., `FillRectS`, `FrameRectS`): Performs automatic boundary checking (clipping) against bitmap boundaries, preventing out-of-bounds crashes.
- **`*TS` suffix** (e.g., `FillRectTS`): Performs boundary checking **and** alpha blending with existing pixels.

---

## Summary

In this example, you learned how to:
1. Instaniate a `TBitmap32` and set its canvas dimensions.
2. Clear the surface to a solid background color.
3. Use `MoveTo` and `LineToAS` to draw smooth anti-aliased lines.
4. Fill rectangular regions with solid or semi-transparent colors using `FillRectS` and `FillRectTS`.

Next, proceed to the [Alpha Blending & Transparency](./alpha-blending) tutorial to learn how 32-bit color channels and alpha composition modes work in Graphics32!
