---
layout: doc
docType: api
unit: GR32_ColorGradients
entity: TRadialExGradientSampler
kind: Class
summary: "HTML5 radial gradient."
declaration: "TRadialExGradientSampler = class(TCustomGradientLookUpTableSampler)"
inheritance:
  - TObject
  - TPersistent
  - TNotifiablePersistent
  - TCustomSampler
  - TCustomGradientSampler
  - TCustomGradientLookUpTableSampler
  - TRadialExGradientSampler
---

## Description

`TRadialExGradientSampler` provides gradation of colors along a cylinder defined by two circles $(C_1, R_1)$ and $(C_2, R_2)$.
 Also knows as "two point conical gradient".

<!-- TODO: more description -->

| Clamp | Mirror | Repeat |
| --- | --- | --- |
| ![](/images/gradient-sampler-radialex-clamp.png) | ![](/images/gradient-sampler-radialex-mirror.png) | ![](/images/gradient-sampler-radialex-repeat.png) |

Colors are mapped according to [[WrapMode]].

## Mathematics & Algorithm

Given focal center $C_1 = (X_1, Y_1)$ and outer center $C_2 = (X_2, Y_2)$, sampling parameter $t$ solves the quadratic intersection equation between the line segment from $C_1$ to $(X, Y)$ and the moving interpolated circle:

$$C(t) = C_1 + t(C_2 - C_1), \quad R(t) = R_1 + t(R_2 - R_1)$$

Solving $\|(X, Y) - C(t)\|^2 = R(t)^2$ yields normalized gradient offset $u = t$.

## References
- [HTML Canvas Radial Gradients](https://www.w3schools.com/graphics/canvas_radial_gradients.asp)
- [Microsoft Typography, COLR](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#radial-gradients)

[members]
