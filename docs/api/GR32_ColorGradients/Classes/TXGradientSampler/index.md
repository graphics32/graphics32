---
layout: doc
docType: api
unit: GR32_ColorGradients
entity: TXGradientSampler
kind: Class
summary: "Linear 2D vector gradient sampler."
declaration: "TXGradientSampler = class(TCustomCenterRadiusAngleLutGradientSampler)"
inheritance:
  - TObject
  - TPersistent
  - TNotifiablePersistent
  - TCustomSampler
  - TCustomGradientSampler
  - TCustomGradientLookUpTableSampler
  - TCustomCenterLutGradientSampler
  - TCustomCenterRadiusLutGradientSampler
  - TCustomCenterRadiusAngleLutGradientSampler
  - TXGradientSampler
---

## Description

`TXGradientSampler` samples a 1D linear gradient projected along a vector defined by [[StartPoint]] and [[EndPoint]].

<!-- TODO: more description -->

| Clamp | Mirror | Repeat |
| --- | --- | --- |
| ![](/images/gradient-sampler-x-clamp.png) | ![](/images/gradient-sampler-x-mirror.png) | ![](/images/gradient-sampler-x-repeat.png) |

## Mathematics & Algorithm

Given start point $P_1 = (X_1, Y_1)$ and end point $P_2 = (X_2, Y_2)$, direction vector $V = P_2 - P_1$.

For any sampling point $P = (X, Y)$, linear parameter $u$ is the scalar projection onto $V$:

$$u = \frac{(P - P_1) \cdot V}{\|V\|^2} = \frac{(X - X_1)(X_2 - X_1) + (Y - Y_1)(Y_2 - Y_1)}{(X_2 - X_1)^2 + (Y_2 - Y_1)^2}$$

[members]
