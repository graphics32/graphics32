---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.MoveTo
kind: Method
scope: Public
declaration: "procedure MoveTo(X, Y: Integer);"
summary: "Sets the current pen position to integer coordinates (X, Y)."
parameters:
  - name: X, Y
    type: Integer
    description: "Target pen coordinates."
---

## Description

`MoveTo` updates `PenPos` to `(X, Y)` for subsequent `LineTo` drawing operations.
