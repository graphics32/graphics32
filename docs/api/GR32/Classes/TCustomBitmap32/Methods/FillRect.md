---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.FillRect
kind: Method
scope: Public
aliases: [FillRectS, FillRectT, FillRectTS]
declaration: |
  procedure FillRect(X1, Y1, X2, Y2: Integer; Value: TColor32);
  procedure FillRectS(X1, Y1, X2, Y2: Integer; Value: TColor32); overload;
  procedure FillRectS(const ARect: TRect; Value: TColor32); overload;
  procedure FillRectT(X1, Y1, X2, Y2: Integer; Value: TColor32);
  procedure FillRectTS(X1, Y1, X2, Y2: Integer; Value: TColor32); overload;
  procedure FillRectTS(const ARect: TRect; Value: TColor32); overload;
parameters:
  - name: X1, Y1, X2, Y2
    type: Integer
    description: "Rectangle corner coordinates."
  - name: ARect
    type: TRect
    description: "Target TRect."
  - name: Value
    type: TColor32
    description: "32-bit ARGB color."
summary: "Fills a rectangular area with a specified 32-bit ARGB color using integer coordinates or TRect bounds, supporting optional boundary clipping and alpha blending."
seealso:
  - "[Naming Conventions](/guide/naming-conventions)"
  - "[[FrameRect]]"
  - "[[Clear]]"
---

## Description

The `FillRect` methods fill a rectangular region bounded by coordinates `(X1, Y1, X2, Y2)` or a `TRect` record `ARect` with a 32-bit ARGB color `Value`.

Method variants provide combinations of direct unclipped memory fill (`FillRect`), boundary clipping against `ClipRect` (`S`), and alpha blending (`T`).

::: tip Note
By convention, `FillRect` includes the left and top borders, but **excludes** the right and bottom borders of the rectangle.

This means that if `X1 >= X2` or `Y1 >= Y2`, then nothing is drawn.
:::

## Variants

| Variant | Coordinate / Bounds | Modifiers | Description |
| --- | --- | --- | --- |
| `FillRect` | `(X1, Y1, X2, Y2)` | Direct | Fast direct fill without boundary clipping or alpha blending. Coordinates must be within bitmap bounds. |
| `FillRectS` | `(X1, Y1, X2, Y2)` or `TRect` | Safe | Fills rectangle clipped against [[ClipRect]]. |
| `FillRectT` | `(X1, Y1, X2, Y2)` | Transparent | Fills rectangle with alpha blending using [[DrawMode]] / [[CombineMode]]. Unclipped. |
| `FillRectTS` | `(X1, Y1, X2, Y2)` or `TRect` | Transparent + Safe | Fills rectangle clipped against [[ClipRect]] with alpha blending using [[DrawMode]] / [[CombineMode]]. |

## Example

```pascal
// Direct unclipped solid fill
Bitmap.FillRect(0, 0, 100, 100, clRed32);

// Clipped alpha-blended fill
var r := Rect(20, 20, 150, 150);
Bitmap.FillRectTS(r, clTrBlue32);
```
