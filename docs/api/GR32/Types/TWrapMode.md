---
layout: doc
docType: api
unit: GR32
entity: TWrapMode
kind: Type
declaration: "type TWrapMode = (wmClamp, wmRepeat, wmMirror, wmReflect);"
summary: "Specifies coordinate wrapping behavior when sampling pixel coordinates outside bitmap boundaries."
seealso:
  - "[[TCustomBitmap32.WrapMode|WrapMode]]"
  - "[[GR32_LowLevel.Clamp|Clamp]]"
  - "[[GR32_LowLevel.Wrap|Wrap]] (Repeat)"
  - "[[GR32_LowLevel.Mirror|Mirror]]"
  - "[[GR32_LowLevel.Reflect|Reflect]]"
---

## Description

`TWrapMode` controls how out-of-bounds coordinates are mapped back into valid bitmap ranges ($0 \dots \text{Size} - 1$) during resampling and texture sampling operations.

| Value | Description | Example |
| --- | --- | --- |
| `wmClamp` | Clamps coordinates to the nearest edge pixel ($0$ or $\text{Size} - 1$). | ![](/images/plot-clamp.svg) |
| `wmRepeat` | Tile coordinates by wrapping modulo the bitmap dimension. | ![](/images/plot-wrap.svg) |
| `wmMirror` | Tile coordinates with alternating mirrored flipping along boundary edges. | ![](/images/plot-mirror.svg) |
| `wmReflect` | Symmetric reflection across boundary edges. | ![](/images/plot-reflect.svg) |
