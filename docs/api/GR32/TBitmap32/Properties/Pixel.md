---
layout: doc
docType: api
unit: GR32
entity: TBitmap32.Pixel
kind: Property
declaration: "property Pixel[X, Y: Integer]: TColor32 read GetPixel write SetPixel; default;"
summary: "Provides 2D array property access to individual 32-bit ARGB pixel values by pixel coordinate (X, Y)."
---

## Remarks

- `Pixel` is the default indexed property of `TBitmap32`, allowing direct array access syntax like `Bmp[X, Y]`.
- Reading or writing `Pixel` automatically performs bounds checking based on `Clipping`.
- For ultra-fast pixel loops without bounds checks, use `PixelPtr[X, Y]` or direct scanline access via `ScanLine[Y]`.
