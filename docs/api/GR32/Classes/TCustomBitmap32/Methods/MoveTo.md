---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.MoveTo
kind: Method
scope: Public
aliases: [MoveToX, MoveToF]
declaration: |
  procedure MoveTo(X, Y: Integer);
  procedure MoveToX(X, Y: TFixed);
  procedure MoveToF(X, Y: Single);
parameters:
  - name: X, Y
    type: "-"
    description: "Target pixel coordinates."
summary: "Sets the current pen position for subsequent LineTo drawing operations using integer, fixed-point, or floating-point coordinates."
seealso:
  - "[Naming Conventions](/guide/naming-conventions)"
  - "[[LineTo]]"
  - "[[PenPos]]"
---

## Description

The `MoveTo` methods set the current drawing pen position ([[PenPos]] / [[PenPosF]]) to coordinates `(X, Y)` without drawing a line segment. Subsequent calls to [[LineTo]] methods will draw line segments originating from this position.

Method variants accept integer (`Integer`), 16.16 fixed-point (`TFixed`), or single-precision floating-point (`Single`) coordinates.

## Variants

| Variant | Coordinate Type | Description |
| --- | --- | --- |
| `MoveTo` | `Integer` | Sets current pen position using integer coordinates. |
| `MoveToX` | `TFixed` | Sets current pen position using 16.16 fixed-point coordinates. |
| `MoveToF` | `Single` | Sets current pen position using single-precision floating-point coordinates. |

## Example

```pascal
Bitmap.MoveTo(50, 50);
Bitmap.LineToS(150, 50);
```
