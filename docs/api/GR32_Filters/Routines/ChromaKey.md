---
layout: doc
docType: api
unit: GR32_Filters
entity: ChromaKey
kind: Function
declaration: "procedure ChromaKey(ABitmap: TCustomBitmap32; KeyColor: TColor32);"
summary: "Makes pixels matching a specific RGB key color fully transparent."
parameters:
  - name: ABitmap
    type: TCustomBitmap32
    description: "Bitmap to process."
  - name: KeyColor
    type: TColor32
    description: "Target key color to make transparent (alpha component ignored)."
---

## Description

`ChromaKey` compares the RGB channels of every pixel in `ABitmap` against `KeyColor and $00FFFFFF`. Matching pixels have their alpha channel set to `0` (fully transparent).

## Example

```pascal
// Make pure green pixels (clGreen32) transparent
ChromaKey(Bitmap, clGreen32);
```
