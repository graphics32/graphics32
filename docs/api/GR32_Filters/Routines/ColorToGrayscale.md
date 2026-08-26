---
layout: doc
docType: api
unit: GR32_Filters
entity: ColorToGrayscale
kind: Function
summary: "Converts a color bitmap to grayscale based on weighted pixel luminance (intensity)."
overloads:
  - signature: "procedure ColorToGrayscale(ABitmap: TCustomBitmap32; PreserveAlpha: Boolean = False); overload;"
    summary: "Converts ABitmap to grayscale in-place based on luminance."
    parameters:
      - name: ABitmap
        type: TCustomBitmap32
        description: "Bitmap to convert in-place."
      - name: PreserveAlpha
        type: Boolean
        description: "If True, preserves original alpha channel. If False (default), sets alpha to opaque ($FF)."

  - signature: "procedure ColorToGrayscale(Dst, Src: TCustomBitmap32; PreserveAlpha: Boolean = False); overload;"
    summary: "Converts Src to grayscale and stores the result in Dst, resizing Dst if necessary."
    parameters:
      - name: Dst
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: Src
        type: TCustomBitmap32
        description: "Source bitmap."
      - name: PreserveAlpha
        type: Boolean
        description: "If True, preserves original alpha values."
---

## Description

`ColorToGrayscale` calculates pixel luminance using weighted intensity coefficients ($0.238 \cdot R + 0.680 \cdot G + 0.082 \cdot B$) via `Intensity(Color)` and assigns the resulting value to RGB channels.

## Example

```pascal
ColorToGrayscale(Bitmap, True); // Convert to grayscale while preserving alpha mask
```
