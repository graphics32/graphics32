---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.AdvanceStippleCounter
kind: Method
scope: Public
declaration: "procedure AdvanceStippleCounter(LengthPixels: Single);"
summary: "Advances current stipple counter phase position by specified length in pixels."
parameters:
  - name: LengthPixels
    type: Single
    description: "Pixel distance to advance."
---

## Description

`AdvanceStippleCounter` shifts `StippleCounter` by `LengthPixels * StippleStep`.
