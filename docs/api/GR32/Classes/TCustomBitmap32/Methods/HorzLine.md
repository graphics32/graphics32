---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.HorzLine
kind: Method
scope: Public
aliases: [HorzLineS, HorzLineT, HorzLineTS, HorzLineTSP, HorzLineX, HorzLineXS]
declaration: |
  procedure HorzLine(X1, Y, X2: Integer; Value: TColor32);
  procedure HorzLineS(X1, Y, X2: Integer; Value: TColor32);
  procedure HorzLineT(X1, Y, X2: Integer; Value: TColor32);
  procedure HorzLineTS(X1, Y, X2: Integer; Value: TColor32);
  procedure HorzLineTSP(X1, Y, X2: Integer);
  procedure HorzLineX(X1, Y, X2: TFixed; Value: TColor32);
  procedure HorzLineXS(X1, Y, X2: TFixed; Value: TColor32);
parameters:
  - name: X1, Y, X2
    type: Integer
    description: "Start X, Y row, and end X coordinates."
  - name: Value
    type: TColor32
    description: "32-bit ARGB color."
summary: "Draws fast horizontal 1-pixel wide lines across integer or fixed-point coordinates with optional boundary clipping, alpha blending, and stippling."
seealso:
  - "[Naming Conventions](/guide/naming-conventions)"
  - "[[VertLine]]"
  - "[[Line]]"
---

## Description

The `HorzLine` methods draw fast 1-pixel wide horizontal lines between horizontal coordinates `X1` and `X2` at row `Y`. Because horizontal lines align directly with bitmap row scanlines in memory, these methods offer superior performance compared to general line drawing routines.

Method variants support integer (`Integer`) or 16.16 fixed-point (`TFixed`) coordinates, boundary clipping (`S`), alpha blending (`T`), and pattern stippling (`P`).

## Variants

| Variant | Coordinate Type | Modifiers | Description |
| --- | --- | --- | --- |
| `HorzLine` | `Integer` | Direct | Fast direct horizontal line fill without boundary clipping or alpha blending. |
| `HorzLineS` | `Integer` | Safe | Horizontal line fill clipped against [[ClipRect]]. |
| `HorzLineT` | `Integer` | Transparent | Unclipped horizontal line fill with alpha blending using [[DrawMode]] / [[CombineMode]]. |
| `HorzLineTS` | `Integer` | Transparent + Safe | Clipped horizontal line fill with alpha blending using [[DrawMode]] / [[CombineMode]]. |
| `HorzLineTSP` | `Integer` | Transparent + Safe + Stipple | Clipped alpha-blended horizontal line using active [[SetStipple\|stipple pattern]]. |
| `HorzLineX` | `TFixed` | Fixed Sub-pixel | Unclipped 16.16 fixed-point horizontal line with anti-aliasing. |
| `HorzLineXS` | `TFixed` | Fixed Sub-pixel + Safe | Clipped 16.16 fixed-point horizontal line with anti-aliasing. |

## Example

```pascal
// Fast direct horizontal line
Bitmap.HorzLine(10, 50, 200, clRed32);

// Clipped alpha-blended horizontal line
Bitmap.HorzLineTS(0, 75, 300, clTrGreen32);
```
