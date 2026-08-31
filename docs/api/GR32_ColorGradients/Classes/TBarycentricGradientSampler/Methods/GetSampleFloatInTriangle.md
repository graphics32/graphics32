---
layout: doc
docType: api
unit: GR32_ColorGradients
parent: TBarycentricGradientSampler
entity: TBarycentricGradientSampler.GetSampleFloatInTriangle
kind: Method
declaration: "function GetSampleFloatInTriangle(X, Y: TFloat): TColor32;"
summary: "Samples color at (X, Y) assuming point is guaranteed to lie inside triangle."
parameters:
  - name: X, Y
    type: TFloat
    description: "Sampling coordinates."
---

## Description

Optimized sampling routine that computes barycentric color weights directly without executing boundary clipping checks.
