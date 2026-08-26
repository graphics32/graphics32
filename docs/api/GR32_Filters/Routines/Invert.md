---
layout: doc
docType: api
unit: GR32_Filters
entity: Invert
kind: Function
summary: "Inverts (negates) specified color channels of a bitmap."
overloads:
  - signature: "procedure Invert(ABitmap: TCustomBitmap32; Components: TColor32Components = [ccAlpha, ccRed, ccGreen, ccBlue]); overload;"
    summary: "Inverts specified color channels of ABitmap in-place."
    parameters:
      - name: ABitmap
        type: TCustomBitmap32
        description: "Bitmap to invert in-place."
      - name: Components
        type: TColor32Components
        description: "Set of channels to invert (default all ARGB channels)."

  - signature: "procedure Invert(Dst, Src: TCustomBitmap32; Components: TColor32Components = [ccAlpha, ccRed, ccGreen, ccBlue]); overload;"
    summary: "Inverts specified color channels of Src and stores result in Dst."
    parameters:
      - name: Dst
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: Src
        type: TCustomBitmap32
        description: "Source bitmap."
      - name: Components
        type: TColor32Components
        description: "Set of channels to invert."
---

## Description

`Invert` performs a bitwise logical `xor` operation using a bitmask generated from `Components`, negating channel byte values ($255 - \text{Value}$).

## Example

```pascal
Invert(Bitmap, [ccRed, ccGreen, ccBlue]);
```
