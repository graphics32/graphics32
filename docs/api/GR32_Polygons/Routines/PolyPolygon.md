---
layout: doc
docType: api
unit: GR32_Polygons
entity: PolyPolygon
kind: Function
aliases: [PolyPolygonFS, PolyPolygonFS_LCD, PolyPolygonFS_LCD2, PolyPolygonXS, PolyPolygonXS_LCD, PolyPolygonXS_LCD2]
declaration: |
  procedure PolyPolygonFS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFloatPoint; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
  procedure PolyPolygonFS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFloatPoint; Filler: TCustomPolygonFiller; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
  procedure PolyPolygonFS_LCD(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFloatPoint; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
  procedure PolyPolygonFS_LCD2(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFloatPoint; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
  procedure PolyPolygonFS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFloatPoint; ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
  procedure PolyPolygonFS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFloatPoint; ClipRect: TRect; Filler: TCustomPolygonFiller; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
  procedure PolyPolygonFS_LCD(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFloatPoint; ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
  procedure PolyPolygonFS_LCD2(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFloatPoint; ClipRect: TRect; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;

  procedure PolyPolygonXS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
  procedure PolyPolygonXS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint; Filler: TCustomPolygonFiller; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
  procedure PolyPolygonXS_LCD(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
  procedure PolyPolygonXS_LCD2(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint; Color: TColor32; FillMode: TPolyFillMode = pfAlternate; Transformation: TTransformation = nil); overload;
parameters:
  - name: Bitmap
    type: TCustomBitmap32
    description: "Destination bitmap."
  - name: Points
    type: "-"
    description: "Array of polygon contours."
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
summary: "Rasterizes filled multi-contour polygon shapes (including complex paths with holes) onto a destination bitmap using floating-point or fixed-point coordinates."
seealso:
  - "[Naming Conventions](/guide/naming-conventions)"
  - "[[Polygon]]"
  - "[[PolyPolyline]]"
  - "[[TPolyFillMode]]"
---

## Description

The `PolyPolygon` routines rasterize multi-contour filled polygon shapes (such as compound shapes with holes or multiple disconnected sub-paths) defined by `Points` onto a destination `Bitmap`.

Function variants support floating-point (`FS`) and fixed-point (`XS`) coordinate inputs, solid color or custom span filler rendering, optional explicit bounding box clipping (`ClipRect`), and sub-pixel LCD antialiasing (`LCD` and `LCD2`).

## Variants

| Variant | Coordinate Type | Anti-Aliasing | Description |
| --- | --- | --- | --- |
| `PolyPolygonFS` | Floating-point | Standard coverage | Rasterizes multi-contour floating-point polygons using standard antialiased coverage blending. |
| `PolyPolygonFS_LCD` | Floating-point | 3x LCD sub-pixel | Rasterizes multi-contour floating-point polygons using horizontal sub-pixel LCD rendering. |
| `PolyPolygonFS_LCD2` | Floating-point | Soft LCD sub-pixel | Rasterizes multi-contour floating-point polygons using a soft sub-pixel LCD filtering profile. |
| `PolyPolygonXS` | Fixed-point | Standard coverage | Rasterizes multi-contour fixed-point polygons using standard antialiased coverage blending. |
| `PolyPolygonXS_LCD` | Fixed-point | 3x LCD sub-pixel | Rasterizes multi-contour fixed-point polygons using horizontal sub-pixel LCD rendering. |
| `PolyPolygonXS_LCD2` | Fixed-point | Soft LCD sub-pixel | Rasterizes multi-contour fixed-point polygons using a soft sub-pixel LCD filtering profile. |

## Example

```pascal
var
  PolyPts: TArrayOfArrayOfFloatPoint;
begin
  SetLength(PolyPts, 2);
  // Outer rectangle
  PolyPts[0] := BuildRectangle(FloatRect(10, 10, 100, 100));
  // Inner hole
  PolyPts[1] := BuildRectangle(FloatRect(30, 30, 70, 70));

  // Render multi-contour polygon with hole using alternate fill mode
  PolyPolygonFS(Bitmap, PolyPts, clBlue32, pfAlternate);
end;
```
