---
layout: doc
docType: api
unit: GR32
parent: TBitmap32
entity: TBitmap32.Clear
kind: Method
summary: "Fills all pixels in the bitmap buffer with a specified 32-bit ARGB color."
overloads:
  - signature: "procedure Clear; overload;"
    summary: "Fills the pixel buffer (or clipped rectangle, if clipping is enabled) with transparent black ($00000000)."
  - signature: "procedure Clear(FillColor: TColor32); overload;"
    summary: "Fills the pixel buffer (or clipped rectangle, if clipping is enabled) with the specified 32-bit ARGB color."
    parameters:
      - name: FillColor
        type: TColor32
        description: "The 32-bit ARGB color to fill the bitmap with."
---

## Example

```pascal
// Clear bitmap to opaque white
Bmp.Clear(clWhite32);

// Clear bitmap to fully transparent
Bmp.Clear(clTrColor32);
```
