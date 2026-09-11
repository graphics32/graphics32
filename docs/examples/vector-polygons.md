---
title: "Vector Polygons & Path Drawing"
---

# Vector Polygons & Path Drawing

While pixel drawing operates on discrete grid points, vector graphics use floating-point coordinates (`TFloatPoint` or `TFixedPoint`) for resolution-independent shape rendering. Graphics32 includes a high-performance Vectorial Polygon Rasterizer (VPR) that renders anti-aliased polygons and strokes with sub-pixel precision.

In this tutorial, you will learn how to build polygons, fill shapes with anti-aliasing, stroke paths with custom line thicknesses, and configure line joins and caps.

---

## 1. Floating-Point Coordinates and Points

In vector units, points are defined using single-precision floating-point coordinates (`TFloatPoint` or `TPoint2F`):

```pascal
uses
  GR32, GR32_VectorUtils;

var
  Pt1, Pt2: TFloatPoint;
begin
  // Create float point using FloatPoint(X, Y) helper
  Pt1 := FloatPoint(100.5, 200.25);
  Pt2 := FloatPoint(350.0, 420.75);
end;
```

---

## 2. Drawing Anti-Aliased Polygons with VPR

To render smooth, anti-aliased filled polygons, use `TPolygon32` or the high-level VPR polygon routines in `GR32_Polygons.pas`.

```pascal
uses
  GR32, GR32_Polygons, GR32_VectorUtils;

var
  Bitmap: TBitmap32;
  Points: TArrayOfFloatPoint;
begin
  Bitmap := TBitmap32.Create;
  try
    Bitmap.SetSize(500, 500);
    Bitmap.Clear(clWhite32);

    // Build a star polygon using an array of float points
    SetLength(Points, 5);
    Points[0] := FloatPoint(250.0,  50.0);
    Points[1] := FloatPoint(310.0, 400.0);
    Points[2] := FloatPoint(100.0, 150.0);
    Points[3] := FloatPoint(400.0, 150.0);
    Points[4] := FloatPoint(190.0, 400.0);

    // Fill anti-aliased polygon using VPR (Vectorial Polygon Rasterizer)
    // PolygonFS performs high-quality sub-pixel anti-aliasing
    PolygonFS(Bitmap, Points, clNavy32);

    Bitmap.SaveToFile('vpr_star.png');
  finally
    Bitmap.Free;
  end;
end;
```

---

## 3. Stroking Paths with Thick Lines, Joins & Caps

To draw outlines or stroked paths with custom line width, use `BuildPolyPolyLine` in `GR32_VectorUtils.pas` to convert line segments into closed thick outline polygons, then rasterize them with `PolyPolygonFS`.

```pascal
uses
  GR32, GR32_Polygons, GR32_VectorUtils;

var
  Bitmap: TBitmap32;
  Path: TArrayOfFloatPoint;
  StrokePolygon: TArrayOfArrayOfFloatPoint;
begin
  Bitmap := TBitmap32.Create;
  try
    Bitmap.SetSize(600, 400);
    Bitmap.Clear(clWhite32);

    // Define zig-zag path
    SetLength(Path, 4);
    Path[0] := FloatPoint(50.0,  300.0);
    Path[1] := FloatPoint(200.0, 100.0);
    Path[2] := FloatPoint(350.0, 300.0);
    Path[3] := FloatPoint(500.0, 100.0);

    // Convert path to thick stroke polygon (Line Width = 12.0 pixels)
    // Parameters: Path, Closed, StrokeWidth, JoinStyle, CapStyle
    StrokePolygon := BuildPolyPolyLine(
      PolyPoints([Path]),  // Input contours
      False,               // Closed path = False
      12.0,                // Line thickness
      jsRound,             // Line Join: jsMiter, jsRound, jsBevel
      dsRound              // End Cap: dsEndSquare, dsRound, dsButt
    );

    // Fill thick stroke polygon onto bitmap
    PolyPolygonFS(Bitmap, StrokePolygon, clDarkRed32);

    Bitmap.SaveToFile('thick_stroke.png');
  finally
    Bitmap.Free;
  end;
end;
```

---

## 4. Complex Multi-Contour Polygons and Holes

You can define complex shapes with cutouts (holes) by combining multiple closed contours into a `TArrayOfArrayOfFloatPoint` (poly-polygon) and selecting fill rules (`pfEvenOdd` vs `pfNonZero`).

```pascal
var
  OuterRing, InnerHole: TArrayOfFloatPoint;
  DonutShape: TArrayOfArrayOfFloatPoint;
begin
  // Build outer circle points
  OuterRing := Circle(250.0, 250.0, 150.0, 64);

  // Build inner hole points (clockwise or counter-clockwise)
  InnerHole := Circle(250.0, 250.0, 75.0, 64);

  // Combine outer shape and inner cutout
  SetLength(DonutShape, 2);
  DonutShape[0] := OuterRing;
  DonutShape[1] := InnerHole;

  // Render donut with hole cut out automatically using Even-Odd fill rule
  PolyPolygonFS(Bitmap, DonutShape, clTeal32, pfEvenOdd);
end;
```

---

## Summary

In this tutorial, you learned:
1. How to work with sub-pixel float coordinates (`TFloatPoint`).
2. How to fill anti-aliased polygons using `PolygonFS` and `PolyPolygonFS`.
3. How to stroke paths with line joins (`jsRound`, `jsMiter`, `jsBevel`) and end caps (`dsRound`, `dsEndSquare`).
4. How to draw donut shapes and complex multi-contour polygons with holes using `pfEvenOdd`.

Next, head to [Color Gradients & Fill Samplers](./color-gradients) to discover how to fill polygons with smooth linear and radial color gradients!
