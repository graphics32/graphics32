---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.CopyMapTo
kind: Method
scope: Public
declaration: "procedure CopyMapTo(Dst: TCustomBitmap32); virtual;"
summary: "Copies raw map buffer dimensions and pixel memory to a destination bitmap without firing change notifications."
parameters:
  - name: Dst
    type: TCustomBitmap32
    description: "Destination bitmap receiving the raw pixel buffer copy."
---

## Description

`CopyMapTo` resizes `Dst` to match this bitmap's dimensions and performs a direct block memory copy of the 32-bit pixel array.

## Example

```pascal
Bitmap.CopyMapTo(TargetBitmap);
```
