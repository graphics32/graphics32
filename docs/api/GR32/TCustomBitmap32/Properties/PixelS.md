---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.PixelS
kind: Property
scope: Public
declaration: "property PixelS[X, Y: Integer]: TColor32 read GetPixelS write SetPixelS;"
summary: "Boundary-clipped indexed property for safe pixel reading and writing at integer coordinates."
---

## Description

`PixelS` verifies coordinates against `ClipRect` before reading or writing. Out-of-bounds reads return `OuterColor`.

## Example

```pascal
Color := Bitmap.PixelS[-1, 5]; // Safe read
```
