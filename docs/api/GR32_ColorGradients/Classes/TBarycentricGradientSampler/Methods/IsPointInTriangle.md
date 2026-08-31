---
layout: doc
docType: api
unit: GR32_ColorGradients
parent: TBarycentricGradientSampler
entity: TBarycentricGradientSampler.IsPointInTriangle
kind: Method
summary: "Tests whether a 2D coordinate point lies inside the triangle defined by the sampler vertices."
overloads:
  - signature: "function IsPointInTriangle(X, Y: TFloat): Boolean; overload;"
    summary: "Tests scalar coordinates (X, Y)."
    parameters:
      - name: X, Y
        type: TFloat
        description: "Point coordinates."
  - signature: "function IsPointInTriangle(const Point: TFloatPoint): Boolean; overload;"
    summary: "Tests TFloatPoint record."
    parameters:
      - name: Point
        type: TFloatPoint
        description: "Point record."
---

## Description

Returns `True` if barycentric coordinates $(\lambda_1, \lambda_2, \lambda_3)$ for $(X, Y)$ satisfy $\lambda_1 \ge 0, \lambda_2 \ge 0, \lambda_3 \ge 0$.
