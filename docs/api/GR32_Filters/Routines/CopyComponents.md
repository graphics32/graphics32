---
layout: doc
docType: api
unit: GR32_Filters
entity: CopyComponents
kind: Function
summary: "Copies specified ARGB color channels from a source bitmap to a destination bitmap."
overloads:
  - signature: "procedure CopyComponents(Dst, Src: TCustomBitmap32; Components: TColor32Components); overload;"
    summary: "Copies specified color components from Src to Dst, resizing Dst if dimensions differ."
    parameters:
      - name: Dst
        type: TCustomBitmap32
        description: "Destination bitmap receiving specified channels."
      - name: Src
        type: TCustomBitmap32
        description: "Source bitmap."
      - name: Components
        type: TColor32Components
        description: "Set of ARGB channels to copy (e.g. [ccRed, ccGreen])."

  - signature: "procedure CopyComponents(Dst: TCustomBitmap32; DstX, DstY: Integer; Src: TCustomBitmap32; SrcRect: TRect; Components: TColor32Components); overload;"
    summary: "Copies specified color components from a sub-rectangle of Src to Dst at coordinates (DstX, DstY)."
    parameters:
      - name: Dst
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: DstX, DstY
        type: Integer
        description: "Top-left destination pixel coordinate."
      - name: Src
        type: TCustomBitmap32
        description: "Source bitmap."
      - name: SrcRect
        type: TRect
        description: "Source sub-rectangle region."
      - name: Components
        type: TColor32Components
        description: "Set of ARGB channels to copy."
---

## Description

`CopyComponents` transfers selected ARGB channels (such as alpha channel only, or RGB channels only) from `Src` to `Dst` while leaving unselected channels in `Dst` unchanged.

If `Src` and `Dst` are the same bitmap instance, or if `Components = []`, the procedure exits without making changes.

## Example

```pascal
// Copy only the alpha channel from AlphaMap to Bitmap
CopyComponents(Bitmap, AlphaMap, [ccAlpha]);
```
