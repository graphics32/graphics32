---
layout: doc
docType: api
unit: GR32_Filters
entity: InvertRGB
kind: Function
summary: "Inverts red, green, and blue color channels while leaving the alpha channel untouched."
overloads:
  - signature: "procedure InvertRGB(ABitmap: TCustomBitmap32); overload;"
    summary: "Inverts RGB channels of ABitmap in-place."
    parameters:
      - name: ABitmap
        type: TCustomBitmap32
        description: "Bitmap to invert."

  - signature: "procedure InvertRGB(Dst, Src: TCustomBitmap32); overload;"
    summary: "Inverts RGB channels of Src and stores result in Dst."
    parameters:
      - name: Dst
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: Src
        type: TCustomBitmap32
        description: "Source bitmap."
---

## Description

`InvertRGB` is a convenience routine that calls `Invert` passing `[ccRed, ccGreen, ccBlue]`.

## Example

```pascal
InvertRGB(Bitmap);
```
