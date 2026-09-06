---
layout: doc
docType: api
unit: GR32.ImageFormats.TBitmap
entity: GR32.ImageFormats.TBitmap
kind: Unit
summary: "Provides image format adapter support for VCL/LCL TBitmap objects."
seealso:
  - "[[GR32.ImageFormats.BMP]]"
---

## Description

The `GR32.ImageFormats.TBitmap` unit implements image format adapters for converting between [[TCustomBitmap32]] and standard VCL/LCL `TBitmap` objects.

::: info
In addition to handling `TBitmap32` <--> `TBitmap` conversion, `GR32.ImageFormats.TBitmap` is also used as a fallback for reading BMP sub-formats that the native Graphics32 BMP implementation doesn't support. Notably the legacy indexed (palette) and RLE-compressed BMP formats.
:::

---

[members]
