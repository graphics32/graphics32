---
layout: doc
docType: api
unit: GR32_Polygons
entity: Polygon
kind: Function
aliases: [PolygonFS, PolygonFS_LCD, PolygonFS_LCD2, PolygonXS, PolygonXS_LCD, PolygonXS_LCD2]
declaration: |
  procedure PolygonFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
  procedure PolygonFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint; Filler: TCustomPolygonFiller; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
  procedure PolygonFS_LCD(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
  procedure PolygonFS_LCD2(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
  procedure PolygonFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint; ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
  procedure PolygonFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint; ClipRect: TRect; Filler: TCustomPolygonFiller; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
  procedure PolygonFS_LCD(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint; ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
  procedure PolygonFS_LCD2(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint; ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;

  procedure PolygonXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
  procedure PolygonXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint; Filler: TCustomPolygonFiller; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
  procedure PolygonXS_LCD(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil);
  procedure PolygonXS_LCD2(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil);
parameters:
  - name: Bitmap
    type: TCustomBitmap32
    description: "Destination bitmap."
  - name: Points
    type: "-"
    description: "Polygon vertices."
  - name: Color
    type: TColor32
    description: "Solid fill color."
  - name: Filler
    type: TCustomPolygonFiller
    description: "Custom span filler."
  - name: ClipRect
    type: TRect
    description: "Optional explicit clipping rectangle."
  - name: FillMode
    type: TPolyFillMode
    description: "Polygon fill rule (`pfAlternate` or `pfWinding`)."
  - name: Transformation
    type: TTransformation
    description: "Optional coordinate transformation."
summary: "Rasterizes a single filled polygon shape onto a destination bitmap using floating-point or fixed-point coordinates with optional LCD sub-pixel antialiasing and custom fillers."
seealso:
  - "[Naming Conventions](/guide/naming-conventions)"
  - "[[PolyPolygon]]"
  - "[[Polyline]]"
  - "[[TPolyFillMode]]"
---

## Description

The `Polygon` routines rasterize a single closed polygon contour defined by `Points` onto a destination `Bitmap`.

Function variants support floating-point (`FS`) and fixed-point (`XS`) coordinate inputs, solid color or custom span filler rendering, optional explicit bounding box clipping (`ClipRect`), and sub-pixel LCD antialiasing (`LCD` and `LCD2`).

## Variants

| Variant | Coordinate Type | Anti-Aliasing | Description |
| --- | --- | --- | --- |
| `PolygonFS` | Floating-point | Standard coverage | Rasterizes a floating-point polygon contour using standard antialiased coverage blending. |
| `PolygonFS_LCD` | Floating-point | 3x LCD sub-pixel | Rasterizes a floating-point polygon contour using horizontal sub-pixel LCD rendering for sharp text and crisp vector outlines on LCD monitors. |
| `PolygonFS_LCD2` | Floating-point | Soft LCD sub-pixel | Rasterizes a floating-point polygon contour using a soft sub-pixel LCD filtering profile. |
| `PolygonXS` | Fixed-point | Standard coverage | Rasterizes a fixed-point polygon contour using standard antialiased coverage blending. |
| `PolygonXS_LCD` | Fixed-point | 3x LCD sub-pixel | Rasterizes a fixed-point polygon contour using horizontal sub-pixel LCD rendering. |
| `PolygonXS_LCD2` | Fixed-point | Soft LCD sub-pixel | Rasterizes a fixed-point polygon contour using a soft sub-pixel LCD filtering profile. |

## Example

```pascal
var
  Pts: TArrayOfFloatPoint;
begin
  SetLength(Pts, 3);
  Pts[0] := FloatPoint(50.0, 10.0);
  Pts[1] := FloatPoint(90.0, 90.0);
  Pts[2] := FloatPoint(10.0, 90.0);

  // Render solid antialiased polygon
  PolygonFS(Bitmap, Pts, clRed32, pfAlternate);
end;
```
