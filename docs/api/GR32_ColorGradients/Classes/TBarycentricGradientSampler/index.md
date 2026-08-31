---
layout: doc
docType: api
unit: GR32_ColorGradients
entity: TBarycentricGradientSampler
kind: Class
summary: "Samples linear color gradients inside or across 3 triangular vertices using barycentric coordinates."
declaration: "TBarycentricGradientSampler = class(TCustomSparsePointGradientSampler)"
inheritance:
  - TObject
  - TPersistent
  - TNotifiablePersistent
  - TCustomSampler
  - TCustomSparsePointGradientSampler
  - TBarycentricGradientSampler
---

## Description

A sparse point gradient interpolator using a barycentric coordinate system for interpolation.

Based on three (and only three) vertices, with each vertex specified with a certain color, a linear triangle of color is calculated. Outside this triangle the colours extends as before.

![](/images/gradient-sampler-barycentric.png)

## Mathematics & Algorithm

Given a triangle with vertices $P_1, P_2, P_3$ and associated colors $C_1, C_2, C_3$, any sampling point $P = (X, Y)$ is expressed in barycentric coordinates $(\lambda_1, \lambda_2, \lambda_3)$:

$$P = \lambda_1 P_1 + \lambda_2 P_2 + \lambda_3 P_3, \quad \lambda_1 + \lambda_2 + \lambda_3 = 1$$

The weights $\lambda_i$ are computed via determinants:

$$\lambda_1 = \frac{(Y_2 - Y_3)(X - X_3) + (X_3 - X_2)(Y - Y_3)}{(Y_2 - Y_3)(X_1 - X_3) + (X_3 - X_2)(Y_1 - Y_3)}$$

$$\lambda_2 = \frac{(Y_3 - Y_1)(X - X_3) + (X_1 - X_3)(Y - Y_3)}{(Y_2 - Y_3)(X_1 - X_3) + (X_3 - X_2)(Y_1 - Y_3)}$$

$$\lambda_3 = 1 - \lambda_1 - \lambda_2$$

The interpolated color $C(P)$ is:

$$C(P) = \lambda_1 C_1 + \lambda_2 C_2 + \lambda_3 C_3$$

## References
- [Barycentric Coordinate System - Wikipedia](https://en.wikipedia.org/wiki/Barycentric_coordinate_system)

## See also
- [[TBarycentricGradientPolygonFiller]]

[members]
