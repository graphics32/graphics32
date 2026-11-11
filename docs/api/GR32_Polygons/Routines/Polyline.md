---
layout: doc
docType: api
unit: GR32_Polygons
entity: Polyline
kind: Function
aliases: [PolylineFS, PolylineXS]
declaration: |
  procedure PolylineFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint; Color: TColor32; Closed: Boolean = False; StrokeWidth: TFloat = 1.0; JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt; MiterLimit: TFloat = 4.0; Transformation: TTransformation = nil); overload;
  procedure PolylineFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint; Filler: TCustomPolygonFiller; Closed: Boolean = False; StrokeWidth: TFloat = 1.0; JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt; MiterLimit: TFloat = 4.0; Transformation: TTransformation = nil); overload;

  procedure PolylineXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint; Color: TColor32; Closed: Boolean = False; StrokeWidth: TFixed = $10000; JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt; MiterLimit: TFixed = $40000; Transformation: TTransformation = nil); overload;
  procedure PolylineXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint; Filler: TCustomPolygonFiller; Closed: Boolean = False; StrokeWidth: TFixed = $10000; JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt; MiterLimit: TFixed = $40000; Transformation: TTransformation = nil); overload;
parameters:
  - name: Bitmap
    type: TCustomBitmap32
    description: "Destination bitmap."
  - name: Points
    type: "-"
    description: "Polyline vertices."
  - name: Color
    type: TColor32
    description: "Solid stroke color."
  - name: Filler
    type: TCustomPolygonFiller
    description: "Custom span filler."
  - name: Closed
    type: Boolean
    description: "Specifies whether the polyline forms a closed loop."
  - name: StrokeWidth
    type: "-"
    description: "Stroke line width."
  - name: JoinStyle
    type: TJoinStyle
    description: "Corner join style (`jsMiter`, `jsBevel`, `jsRound`)."
  - name: EndStyle
    type: TEndStyle
    description: "Line end cap style (`esButt`, `esSquare`, `esRound`)."
  - name: MiterLimit
    type: "-"
    description: "Miter limit ratio before beveling sharp miter joins."
  - name: Transformation
    type: TTransformation
    description: "Optional coordinate transformation."
summary: "Renders a single stroked polyline contour with configurable join styles, cap styles, stroke widths, and span fillers onto a destination bitmap."
seealso:
  - "[Naming Conventions](/guide/naming-conventions)"
  - "[[PolyPolyline]]"
  - "[[Polygon]]"
  - "[[DashLine]]"
---

## Description

The `Polyline` routines render a single stroked polyline contour defined by `Points` onto a destination `Bitmap`.

Function variants support floating-point (`FS`) and fixed-point (`XS`) coordinate inputs as well as solid color or custom span filler rendering. Stroke outlines are generated automatically according to `StrokeWidth`, `JoinStyle`, `EndStyle`, and `MiterLimit`.

## Variants

| Variant | Coordinate Type | Description |
| --- | --- | --- |
| `PolylineFS` | Floating-point | Renders a single floating-point polyline contour with antialiased stroke outlines. |
| `PolylineXS` | Fixed-point | Renders a single fixed-point polyline contour with antialiased stroke outlines. |

## Example

```pascal
var
  Pts: TArrayOfFloatPoint;
begin
  SetLength(Pts, 4);
  Pts[0] := FloatPoint(10.0, 10.0);
  Pts[1] := FloatPoint(50.0, 80.0);
  Pts[2] := FloatPoint(90.0, 20.0);
  Pts[3] := FloatPoint(130.0, 90.0);

  // Render open polyline with 3.0px width and round caps/joins
  PolylineFS(Bitmap, Pts, clBlack32, False, 3.0, jsRound, esRound);
end;
```
