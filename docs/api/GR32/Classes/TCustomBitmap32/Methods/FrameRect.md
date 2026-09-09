---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.FrameRect
kind: Method
scope: Public
aliases: [FrameRectS, FrameRectTS, FrameRectTSP]
declaration: |
  procedure FrameRectS(X1, Y1, X2, Y2: Integer; Value: TColor32); overload;
  procedure FrameRectS(const ARect: TRect; Value: TColor32); overload;
  procedure FrameRectTS(X1, Y1, X2, Y2: Integer; Value: TColor32); overload;
  procedure FrameRectTS(const ARect: TRect; Value: TColor32); overload;
  procedure FrameRectTSP(X1, Y1, X2, Y2: Integer);
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
summary: "Draws 1-pixel wide rectangular outline frames around specified coordinates or TRect bounds with optional clipping, alpha blending, and stippling."
seealso:
  - "[Naming Conventions](/guide/naming-conventions)"
  - "[[FillRect]]"
---

## Description

The `FrameRect` methods draw a 1-pixel wide rectangular outline frame around a bounding box specified by coordinates `(X1, Y1, X2, Y2)` or a `TRect` record `ARect`.

Method variants provide boundary clipping against [[ClipRect]] (`S`), alpha blending (`T`), and pattern stippling (`P`).

::: tip Note
By convention, `FrameRect` includes the left and top borders, but **excludes** the right and bottom borders of the rectangle.

This means that if `X1 >= X2` or `Y1 >= Y2`, then nothing is drawn.
:::

## Variants

| Variant | Coordinate / Bounds | Modifiers | Description |
| --- | --- | --- | --- |
| `FrameRectS` | `(X1, Y1, X2, Y2)` or `TRect` | Safe | Draws a 1-pixel clipped rectangular frame using color `Value`. |
| `FrameRectTS` | `(X1, Y1, X2, Y2)` or `TRect` | Transparent + Safe | Draws a 1-pixel clipped rectangular frame with alpha blending using [[DrawMode]] / [[CombineMode]]. |
| `FrameRectTSP` | `(X1, Y1, X2, Y2)` | Transparent + Safe + Stipple | Draws a 1-pixel clipped alpha-blended rectangular frame using the active [[SetStipple\|stipple pattern]]. |


## Example

```pascal
// Clipped 1-pixel outline frame
Bitmap.FrameRectS(10, 10, 100, 100, clBlack32);

// Alpha-blended frame using TRect
var r := Rect(20, 20, 150, 150);
Bitmap.FrameRectTS(r, clTrRed32);
```
