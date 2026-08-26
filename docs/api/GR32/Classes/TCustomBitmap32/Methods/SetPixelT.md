---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.SetPixelT
kind: Method
scope: Public
summary: "Sets a pixel color using current DrawMode and CombineMode blending rules."
overloads:
  - signature: "procedure SetPixelT(X, Y: Integer; Value: TColor32); overload;"
    summary: "Sets pixel at coordinates (X, Y) using blending rules."
    parameters:
      - name: X, Y
        type: Integer
        description: "Pixel coordinates."
      - name: Value
        type: TColor32
        description: "32-bit ARGB color."
  - signature: "procedure SetPixelT(var Ptr: PColor32; Value: TColor32); overload;"
    summary: "Sets pixel at pointer reference Ptr using blending rules."
    parameters:
      - name: Ptr
        type: PColor32
        description: "Pointer to target pixel."
      - name: Value
        type: TColor32
        description: "32-bit ARGB color."
---

## Description

`SetPixelT` applies alpha blending (`DrawMode` / `CombineMode`) when writing pixel values.

## Example

```pascal
Bitmap.SetPixelT(10, 20, clTrRed32);
```
