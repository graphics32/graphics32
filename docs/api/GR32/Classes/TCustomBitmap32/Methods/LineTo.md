---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.LineTo
kind: Method
scope: Public
aliases: [LineToS, LineToTS, LineToAS, LineToXS, LineToFS, LineToXSP, LineToFSP]
declaration: |
  procedure LineToS(X, Y: Integer);
  procedure LineToTS(X, Y: Integer);
  procedure LineToAS(X, Y: Integer);
  procedure LineToXS(X, Y: TFixed);
  procedure LineToFS(X, Y: Single);
  procedure LineToXSP(X, Y: TFixed);
  procedure LineToFSP(X, Y: Single);
parameters:
  - name: X, Y
    type: "-"
    description: "Target end pixel coordinates."
summary: "Draws 1-pixel wide line segments from the current pen position to target coordinates using integer, fixed-point, or floating-point positioning, updating pen position afterwards."
seealso:
  - "[Naming Conventions](/guide/naming-conventions)"
  - "[[MoveTo]]"
  - "[[PenColor]]"
  - "[[PenPos]]"
---

## Description

The `LineTo` methods draw a 1-pixel wide straight line segment from the current pen position ([[PenPos]] / [[PenPosF]]) to specified target coordinates `(X, Y)`. Upon completing the line segment, the current pen position is automatically updated to `(X, Y)`.

::: tip Note
By design, `LineTo` does **not** draw the last pixel of the line segment. This is done so it can be used to draw contiguous polyline segments without double-rendering vertices.

Internally `LineTo` is implemented as:

```pascal
Line(PenPos.X, PenPos.Y, X, Y, PenColor, False);
PenPos.X := X;
PenPos.Y := Y;
```
:::

Method variants provide specific combinations of coordinate types (`Integer`, `TFixed`, `Single`), boundary clipping (`S`), alpha transparency (`T`), anti-aliasing (`A`, `X`, `F`), and pattern stippling (`P`).

## Variants

| Variant | Coordinate Type | Modifiers | Description |
| --- | --- | --- | --- |
| `LineToS` | `Integer` | Safe | Clipped integer line drawing from current pen position using [[PenColor]]. |
| `LineToTS` | `Integer` | Transparent + Safe | Clipped integer line drawing with alpha blending using [[PenColor]]. |
| `LineToAS` | `Integer` | Anti-aliased + Safe | Clipped anti-aliased integer line drawing using Wu's algorithm. |
| `LineToXS` | `TFixed` | Fixed Sub-pixel + Safe | Clipped 16.16 fixed-point anti-aliased line drawing. |
| `LineToFS` | `Single` | Float Sub-pixel + Safe | Clipped floating-point anti-aliased line drawing. |
| `LineToXSP` | `TFixed` | Fixed + Stipple + Safe | Clipped 16.16 fixed-point stippled line drawing using active [[SetStipple\|stipple pattern]]. |
| `LineToFSP` | `Single` | Float + Stipple + Safe | Clipped floating-point stippled line drawing using active [[SetStipple\|stipple pattern]]. |

## Example

```pascal
Bitmap.PenColor := clRed32;
Bitmap.MoveTo(10, 10);
Bitmap.LineToS(100, 10);
Bitmap.LineToS(100, 100);
Bitmap.LineToS(10, 10);
```
