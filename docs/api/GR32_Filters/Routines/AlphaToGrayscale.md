---
layout: doc
docType: api
unit: GR32_Filters
entity: AlphaToGrayscale
kind: Function
summary: "Converts a bitmap to grayscale by copying alpha channel values to red, green, and blue channels."
overloads:
  - signature: "procedure AlphaToGrayscale(ABitmap: TCustomBitmap32); overload;"
    summary: "Converts ABitmap to grayscale in-place by copying its alpha values to RGB."
    parameters:
      - name: ABitmap
        type: TCustomBitmap32
        description: "Bitmap to convert in-place."

  - signature: "procedure AlphaToGrayscale(Dst, Src: TCustomBitmap32); overload;"
    summary: "Copies the alpha channel of Src to RGB channels of Dst, resizing Dst if necessary."
    parameters:
      - name: Dst
        type: TCustomBitmap32
        description: "Destination bitmap receiving grayscale output."
      - name: Src
        type: TCustomBitmap32
        description: "Source bitmap."
---

## Description

`AlphaToGrayscale` copies the alpha component (`A`) of each pixel to its red (`R`), green (`G`), and blue (`B`) channels, visualizing the alpha mask as a grayscale image.

## Example

```pascal
AlphaToGrayscale(Bitmap);
```
