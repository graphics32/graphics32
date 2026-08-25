---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.SetPixelTS
kind: Method
scope: Public
declaration: "procedure SetPixelTS(X, Y: Integer; Value: TColor32);"
summary: "Sets a pixel color with boundary clipping using current DrawMode and CombineMode blending rules."
parameters:
  - name: X, Y
    type: Integer
    description: "Pixel coordinates."
  - name: Value
    type: TColor32
    description: "32-bit ARGB color."
---

## Description

`SetPixelTS` checks coordinate boundaries against `ClipRect` before writing with alpha blending.

## Example

```pascal
Bitmap.SetPixelTS(-5, 10, clRed32); // Safe against out-of-bounds coordinates
```
