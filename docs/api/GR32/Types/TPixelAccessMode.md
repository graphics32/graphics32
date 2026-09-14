---
layout: doc
docType: api
unit: GR32
entity: TPixelAccessMode
kind: Type
declaration: "type TPixelAccessMode = (pamUnsafe, pamSafe, pamWrap, pamTransparentEdge);"
summary: "Specifies pixel boundary checking and safety behavior for bitmap resamplers."
seealso:
  - "[[TCustomResampler.PixelAccessMode|PixelAccessMode]]"
  - "[[TCustomResampler]]"
---

## Description

`TPixelAccessMode` defines how [[GR32_Resamplers|resamplers]] handle boundary clipping and pixel access safety during image resampling calculations.

| Value | Description |
| --- | --- |
| `pamUnsafe` | Direct pixel array access without bounds checking for maximum speed (requires caller to guarantee coordinates lie within valid clip rects). |
| `pamSafe` | Clamped bounds checking ensuring coordinates outside bitmap bounds return safe edge or outer colors. |
| `pamWrap` | Uses the bitmap's [[TWrapMode]] to wrap out-of-bounds coordinates. |
| `pamTransparentEdge` | Out-of-bounds pixel samples return fully transparent pixels (`$00000000`). |
