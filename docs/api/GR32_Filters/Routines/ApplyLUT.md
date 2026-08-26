---
layout: doc
docType: api
unit: GR32_Filters
entity: ApplyLUT
kind: Function
summary: "Transforms bitmap color channels using an 8-bit Look-Up Table (TLUT8)."
overloads:
  - signature: "procedure ApplyLUT(ABitmap: TCustomBitmap32; const LUT: TLUT8; PreserveAlpha: Boolean = False); overload;"
    summary: "Transforms color channels of ABitmap in-place using LUT."
    parameters:
      - name: ABitmap
        type: TCustomBitmap32
        description: "Bitmap to transform."
      - name: LUT
        type: TLUT8
        description: "256-byte lookup table array."
      - name: PreserveAlpha
        type: Boolean
        description: "If True, preserves alpha channel values. If False (default), sets alpha to $FF."

  - signature: "procedure ApplyLUT(Dst, Src: TCustomBitmap32; const LUT: TLUT8; PreserveAlpha: Boolean = False); overload;"
    summary: "Transforms color channels of Src using LUT and stores result in Dst."
    parameters:
      - name: Dst
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: Src
        type: TCustomBitmap32
        description: "Source bitmap."
      - name: LUT
        type: TLUT8
        description: "256-byte lookup table array."
      - name: PreserveAlpha
        type: Boolean
        description: "If True, preserves original alpha values."
---

## Description

`ApplyLUT` maps each pixel's RGB channel byte value $C$ to `LUT[C]`.

## Example

```pascal
ApplyLUT(Bitmap, MyLUT, True);
```
