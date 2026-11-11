---
layout: doc
docType: api
unit: GR32_Polygons
entity: PolyPolyline
kind: Function
aliases: [PolyPolylineFS, PolyPolylineXS]
declaration: |
  procedure PolyPolylineFS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFloatPoint; Color: TColor32; Closed: Boolean = False; StrokeWidth: TFloat = 1.0; JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt; MiterLimit: TFloat = 4.0; Transformation: TTransformation = nil); overload;
  procedure PolyPolylineFS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFloatPoint; Filler: TCustomPolygonFiller; Closed: Boolean = False; StrokeWidth: TFloat = 1.0; JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt; MiterLimit: TFloat = 4.0; Transformation: TTransformation = nil); overload;

  procedure PolyPolylineXS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint; Color: TColor32; Closed: Boolean = False; StrokeWidth: TFixed = $10000; JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt; MiterLimit: TFixed = $40000; Transformation: TTransformation = nil); overload;
  procedure PolyPolylineXS(Bitmap: TCustomBitmap32; const Points: TArrayOfArrayOfFixedPoint; Filler: TCustomPolygonFiller; Closed: Boolean = False; StrokeWidth: TFixed = $10000; JoinStyle: TJoinStyle = jsMiter; EndStyle: TEndStyle = esButt; MiterLimit: TFixed = $40000; Transformation: TTransformation = nil); overload;
parameters:
  - name: Bitmap
    type: TCustomBitmap32
    description: "Destination bitmap."
  - name: Points
    type: "-"
    description: "Array of polyline contours."
  - name: Color
    type: TColor32
    description: "Solid stroke color."
  - name: Filler
    type: TCustomPolygonFiller
    description: "Custom span filler."
  - name: Closed
    type: Boolean
    description: "Specifies whether each polyline contour forms a closed loop."
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
summary: "Renders multiple stroked polyline contours with configurable join styles, cap styles, stroke widths, and span fillers onto a destination bitmap."
seealso:
  - "[Naming Conventions](/guide/naming-conventions)"
  - "[[Polyline]]"
  - "[[PolyPolygon]]"
  - "[[DashLine]]"
---

## Description

The `PolyPolyline` routines render multiple stroked polyline contours defined by `Points` onto a destination `Bitmap`.

Function variants support floating-point (`FS`) and fixed-point (`XS`) coordinate inputs as well as solid color or custom span filler rendering. Stroke outlines are generated automatically for all contours according to `StrokeWidth`, `JoinStyle`, `EndStyle`, and `MiterLimit`.

## Variants

| Variant | Coordinate Type | Description |
| --- | --- | --- |
| `PolyPolylineFS` | Floating-point | Renders multiple floating-point polyline contours with antialiased stroke outlines. |
| `PolyPolylineXS` | Fixed-point | Renders multiple fixed-point polyline contours with antialiased stroke outlines. |

## Example

```pascal
var
  Lines: TArrayOfArrayOfFloatPoint;
begin
  SetLength(Lines, 2);
  Lines[0] := BuildLine(FloatPoint(10, 10), FloatPoint(100, 10));
  Lines[1] := BuildLine(FloatPoint(10, 50), FloatPoint(100, 50));

  // Render multiple stroke lines
  PolyPolylineFS(Bitmap, Lines, clGreen32, False, 2.5);
end;
```
