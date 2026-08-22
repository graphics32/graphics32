---
layout: doc
docType: api
unit: GR32
entity: TBitmap32.Clear
kind: Method
declaration: |
  procedure Clear; overload;
  procedure Clear(FillColor: TColor32); overload;
summary: "Fills all pixels in the bitmap buffer with a specified 32-bit ARGB color."
parameters:
  - name: FillColor
    type: TColor32
    description: "The 32-bit ARGB color to fill the bitmap with. If omitted, defaults to OuterColor or transparent ($00000000)."
---

## Example

```pascal
// Clear bitmap to opaque white
Bmp.Clear(clWhite32);

// Clear bitmap to fully transparent
Bmp.Clear(clTrColor32);
```
