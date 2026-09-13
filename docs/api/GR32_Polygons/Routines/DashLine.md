---
layout: doc
docType: api
unit: GR32_Polygons
entity: DashLine
kind: Function
aliases: [DashLineFS, DashLineXS]
declaration: |
  procedure DashLineFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint; const Dashes: TArrayOfFloat; Color: TColor32; Closed: Boolean = False; Width: TFloat = 1.0); overload;
  procedure DashLineFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint; const Dashes: TArrayOfFloat; FillColor, StrokeColor: TColor32; Closed: Boolean; Width: TFloat; StrokeWidth: TFloat = 2.0); overload;
  procedure DashLineFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint; const Dashes: TArrayOfFloat; Filler: TCustomPolygonFiller; Closed: Boolean = False; Width: TFloat = 1.0); overload;
  procedure DashLineFS(Bitmap: TCustomBitmap32; const Points: TArrayOfFloatPoint; const Dashes: TArrayOfFloat; Filler: TCustomPolygonFiller; StrokeColor: TColor32; Closed: Boolean; Width: TFloat; StrokeWidth: TFloat = 2.0); overload;

  procedure DashLineXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint; const Dashes: TArrayOfFixed; Color: TColor32; Closed: Boolean = False; Width: TFixed = $10000); overload;
  procedure DashLineXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint; const Dashes: TArrayOfFixed; FillColor, StrokeColor: TColor32; Closed: Boolean; Width: TFixed; StrokeWidth: TFixed = $20000); overload;
  procedure DashLineXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint; const Dashes: TArrayOfFixed; Filler: TCustomPolygonFiller; Closed: Boolean = False; Width: TFixed = $10000); overload;
  procedure DashLineXS(Bitmap: TCustomBitmap32; const Points: TArrayOfFixedPoint; const Dashes: TArrayOfFixed; Filler: TCustomPolygonFiller; StrokeColor: TColor32; Closed: Boolean; Width: TFixed; StrokeWidth: TFixed = $20000); overload;
parameters:
  - name: Bitmap
    type: TCustomBitmap32
    description: "Destination bitmap."
  - name: Points
    type: "-"
    description: "Polyline vertices."
  - name: Dashes
    type: "-"
    description: "Array of dash and gap lengths."
  - name: Color
    type: TColor32
    description: "Solid dash fill color."
  - name: FillColor
    type: TColor32
    description: "Inner dash fill color."
  - name: StrokeColor
    type: TColor32
    description: "Outer stroke outline color."
  - name: Filler
    type: TCustomPolygonFiller
    description: "Custom span filler."
  - name: Closed
    type: Boolean
    description: "Specifies whether the polyline forms a closed loop."
  - name: Width
    type: "-"
    description: "Dash segment width."
  - name: StrokeWidth
    type: "-"
    description: "Outer stroke outline width."
summary: "Renders dashed polylines using pattern lengths, with optional outer stroke outlines and custom span fillers."
seealso:
  - "[Naming Conventions](/guide/naming-conventions)"
  - "[[Polyline]]"
  - "[[PolyPolyline]]"
  - "[[Polygon]]"
---

## Description

The `DashLine` routines break input polyline paths into dashed segment patterns specified by `Dashes` and render them onto `Bitmap`.

Function variants support floating-point (`FS`) and fixed-point (`XS`) coordinate inputs, single solid color fills, dual fill and stroke outline colors, or custom span fillers with outline stroke options.

## Variants

| Variant | Coordinate Type | Description |
| --- | --- | --- |
| `DashLineFS` | Floating-point | Renders dashed polylines using floating-point coordinates and dash lengths. |
| `DashLineXS` | Fixed-point | Renders dashed polylines using fixed-point coordinates and dash lengths. |

## Example

```pascal
var
  Pts: TArrayOfFloatPoint;
  Pattern: TArrayOfFloat;
begin
  SetLength(Pts, 2);
  Pts[0] := FloatPoint(10.0, 50.0);
  Pts[1] := FloatPoint(200.0, 50.0);

  // Dash pattern: 10px dash, 5px space
  SetLength(Pattern, 2);
  Pattern[0] := 10.0;
  Pattern[1] := 5.0;

  // Render dashed line
  DashLineFS(Bitmap, Pts, Pattern, clBlack32, False, 2.0);
end;
```
