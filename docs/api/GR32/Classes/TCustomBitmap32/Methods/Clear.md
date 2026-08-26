---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.Clear
kind: Method
scope: Public
summary: "Clears the entire pixel buffer to transparent black (clNone32) or a specified TColor32 value."
overloads:
  - signature: "procedure Clear; overload; override;"
    summary: "Clears all pixels in the bitmap to clNone32 ($00000000)."
  - signature: "procedure Clear(FillColor: TColor32); reintroduce; overload;"
    summary: "Clears all pixels in the bitmap to the specified TColor32 fill color."
    parameters:
      - name: FillColor
        type: TColor32
        description: "32-bit ARGB fill color."
---

## Description

`Clear` fills the entire pixel buffer with a uniform color value.

## Example

```pascal
Bitmap.Clear(clRed32);
```
