---
layout: doc
docType: api
unit: GR32
entity: TCombineMode
kind: Type
declaration: "type TCombineMode = (cmBlend, cmMerge);"
summary: "Specifies the alpha compositing mode used during alpha blending operations."
seealso:
  - "[[TCustomBitmap32.CombineMode|CombineMode]]"
  - "[[TDrawMode]]"
  - "[[GR32_Blend]]"
---

## Description

`TCombineMode` determines which mathematical alpha composition formula is executed when blending semi-transparent pixels (`dmBlend`).

| Value | Description |
| --- | --- |
| `cmBlend` | Standard alpha blending. Mixes foreground color $F$ onto background color $B$ assuming $B$ is fully opaque ($B_a = 255$). Uses [[Blend]]. |
| `cmMerge` | Associative alpha merging. Combines foreground $F$ and background $B$ taking both $F_a$ and $B_a$ alpha values into account according to Bruce Wallace's formula. Uses [[Merge]]. |
