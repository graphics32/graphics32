---
layout: doc
docType: api
unit: GR32_ColorGradients
entity: TBilinearGradientSampler
kind: Class
summary: "Samples color gradients across 4 points using bilinear interpolation."
declaration: "TBilinearGradientSampler = class(TCustomSparsePointGradientSampler)"
inheritance:
  - TObject
  - TPersistent
  - TNotifiablePersistent
  - TCustomSampler
  - TCustomSparsePointGradientSampler
  - TBilinearGradientSampler
---

## Description

A sparse point gradient interpolator using a inverse bilinear interpolation.

Based on four (and only four) vertices, with each vertex specified with a certain color, an inverse bilinear interpolation of color is calculated. Outside this the colours extends as before, but discontinuities may occur.

![](/images/gradient-sampler-bilinear.png)

## Mathematics & Algorithm

Given 4 vertex coordinates $P_{00}, P_{10}, P_{01}, P_{11}$ and colors $C_{00}, C_{10}, C_{01}, C_{11}$, sampling coordinates $(u, v) \in [0, 1] \times [0, 1]$ parameterize the unit quad:

$$P(u, v) = (1-u)(1-v) P_{00} + u(1-v) P_{10} + (1-u)v P_{01} + uv P_{11}$$

$$C(u, v) = (1-u)(1-v) C_{00} + u(1-v) C_{10} + (1-u)v C_{01} + uv C_{11}$$

## References
- [Bilinear Interpolation - Wikipedia](https://en.wikipedia.org/wiki/Bilinear_interpolation)

[members]
