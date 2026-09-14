---
layout: doc
docType: api
unit: GR32
entity: TDrawMode
kind: Type
declaration: "type TDrawMode = (dmOpaque, dmBlend, dmCustom, dmTransparent);"
summary: "Specifies the pixel drawing and compositing mode when rendering or copying bitmaps."
seealso:
  - "[[TCustomBitmap32.DrawMode|DrawMode]]"
  - "[[TCombineMode]]"
---

## Description

`TDrawMode` defines how source pixels are composited onto destination pixels during bitmap drawing, block transfers, and polygon rendering operations.

| Value | Description |
| --- | --- |
| `dmOpaque` | Source pixels overwrite destination pixels completely, ignoring alpha channel transparency. |
| `dmBlend` | Source pixels are blended onto destination pixels using foreground alpha transparency according to the active [[TCombineMode]] (`cmBlend` or `cmMerge`). |
| `dmCustom` | Source pixels are combined with destination pixels using a custom user-defined callback event handler ([[TCustomBitmap32.OnPixelCombine\|OnPixelCombine]]). |
| `dmTransparent` | Source pixels matching the bitmap's [[OuterColor]] are omitted, leaving destination pixels unchanged. Also known as *Color Key* transparency. |
