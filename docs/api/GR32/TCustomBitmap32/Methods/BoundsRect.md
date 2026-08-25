---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.BoundsRect
kind: Method
scope: Public
declaration: "function BoundsRect: TRect;"
summary: "Returns a TRect structure representing the full pixel bounds of the bitmap (0, 0, Width, Height)."
---

## Description

`BoundsRect` returns a rectangle structure (`TRect`) spanning from `(0, 0)` to `(Width, Height)`.

## Example

```pascal
Bitmap.FillRect(Bitmap.BoundsRect, clWhite32);
```
