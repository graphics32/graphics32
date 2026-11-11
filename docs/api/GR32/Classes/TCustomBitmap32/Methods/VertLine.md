---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.VertLine
kind: Method
scope: Public
aliases: [VertLineS, VertLineT, VertLineTS, VertLineTSP, VertLineX, VertLineXS]
declaration: |
  procedure VertLine(X, Y1, Y2: Integer; Value: TColor32);
  procedure VertLineS(X, Y1, Y2: Integer; Value: TColor32);
  procedure VertLineT(X, Y1, Y2: Integer; Value: TColor32);
  procedure VertLineTS(X, Y1, Y2: Integer; Value: TColor32);
  procedure VertLineTSP(X, Y1, Y2: Integer);
  procedure VertLineX(X, Y1, Y2: TFixed; Value: TColor32);
  procedure VertLineXS(X, Y1, Y2: TFixed; Value: TColor32);
parameters:
  - name: X, Y1, Y2
    type: Integer
    description: "X column, start Y, and end Y coordinates."
  - name: Value
    type: TColor32
    description: "32-bit ARGB color."
summary: "Draws fast vertical 1-pixel wide lines across integer or fixed-point coordinates with optional boundary clipping, alpha blending, and stippling."
seealso:
  - "[Naming Conventions](/guide/naming-conventions)"
  - "[[HorzLine]]"
  - "[[Line]]"
---

## Description

The `VertLine` methods draw fast 1-pixel wide vertical lines between vertical coordinates `Y1` and `Y2` at column `X`.

Method variants support integer (`Integer`) or 16.16 fixed-point (`TFixed`) coordinates, boundary clipping (`S`), alpha blending (`T`), and pattern stippling (`P`).

## Variants

| Variant | Coordinate Type | Modifiers | Description |
| --- | --- | --- | --- |
| `VertLine` | `Integer` | Direct | Fast direct vertical line fill without boundary clipping or alpha blending. |
| `VertLineS` | `Integer` | Safe | Vertical line fill clipped against [[ClipRect]]. |
| `VertLineT` | `Integer` | Transparent | Unclipped vertical line fill with alpha blending using [[DrawMode]] / [[CombineMode]]. |
| `VertLineTS` | `Integer` | Transparent + Safe | Clipped vertical line fill with alpha blending using [[DrawMode]] / [[CombineMode]]. |
| `VertLineTSP` | `Integer` | Transparent + Safe + Stipple | Clipped alpha-blended vertical line using active [[SetStipple\|stipple pattern]]. |
| `VertLineX` | `TFixed` | Fixed Sub-pixel | Unclipped 16.16 fixed-point vertical line with anti-aliasing. |
| `VertLineXS` | `TFixed` | Fixed Sub-pixel + Safe | Clipped 16.16 fixed-point vertical line with anti-aliasing. |

## Example

```pascal
// Fast direct vertical line
Bitmap.VertLine(50, 10, 200, clRed32);

// Clipped alpha-blended vertical line
Bitmap.VertLineTS(75, 0, 300, clTrBlue32);
```
