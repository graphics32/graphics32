---
layout: doc
docType: api
unit: GR32_Transforms
entity: VectorTransform
kind: Function
declaration: "function VectorTransform(const M: TFloatMatrix; const V: TVector3f): TVector3f;"
summary: "Transforms a 3-element vector by a 3x3 matrix."
parameters:
  - name: M
    type: TFloatMatrix
    description: "Transformation matrix."
  - name: V
    type: TVector3f
    description: "Input vector."
---

## Description

`VectorTransform` multiplies $3 \times 3$ matrix `M` by column vector `V` and returns the resulting 3-element vector.
