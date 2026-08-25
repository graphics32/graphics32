---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.PixelW
kind: Property
scope: Public
declaration: "property PixelW[X, Y: Integer]: TColor32 read GetPixelW write SetPixelW;"
summary: "Wrapped indexed property for reading and writing pixels at integer coordinates with boundary tiling."
---

## Description

`PixelW` wraps coordinates modulo `Width` and `Height`.

## Example

```pascal
Color := Bitmap.PixelW[850, 650]; // Tiled access
```
