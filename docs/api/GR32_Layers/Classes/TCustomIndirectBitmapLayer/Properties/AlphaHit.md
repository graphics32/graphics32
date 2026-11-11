---
layout: doc
docType: api
unit: GR32_Layers
parent: TCustomIndirectBitmapLayer
entity: TCustomIndirectBitmapLayer.AlphaHit
kind: Property
scope: Public
declaration: "property AlphaHit: Boolean read FAlphaHit write FAlphaHit;"
summary: "Controls whether hit testing considers pixel alpha transparency."
---

## Description

When `AlphaHit` is `True`, hit testing (`HitTest`) evaluates the alpha channel of the bitmap pixel under the cursor, ignoring clicks on fully transparent pixels (alpha = 0).
