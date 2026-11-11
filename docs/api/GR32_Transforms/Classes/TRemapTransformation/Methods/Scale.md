---
layout: doc
docType: api
unit: GR32_Transforms
parent: TRemapTransformation
entity: TRemapTransformation.Scale
kind: Method
declaration: "procedure Scale(Sx, Sy: TFloat);"
summary: "Applies horizontal and vertical scaling factors to vector map displacements."
parameters:
  - name: Sx, Sy
    type: TFloat
    description: "Displacement vector scaling factors."
---

## Description

`Scale` multiplies displacement vectors stored in `VectorMap` by `Sx` and `Sy` when the transformation is applied. The `VectorMap` itself is not modified.
