---
layout: doc
docType: api
unit: GR32_ColorGradients
entity: TConicGradientSampler
kind: Class
summary: "Samples an angular conic (sweep) color gradient around a center point."
declaration: "TConicGradientSampler = class(TCustomCenterLutGradientSampler)"
inheritance:
  - TObject
  - TPersistent
  - TNotifiablePersistent
  - TCustomSampler
  - TCustomGradientSampler
  - TCustomGradientLookUpTableSampler
  - TCustomCenterLutGradientSampler
  - TConicGradientSampler
---

## Description

Conical color gradient.

A gradient which goes along the circular arc around a `Center`. The gradient can be rotated using the `Angle` property 

<!-- TODO: more description -->

![](/images/gradient-sampler-conic.png)

## Mathematics & Algorithm

`TConicGradientSampler` computes color stops based on polar angle $\theta$ around center $(X_c, Y_c)$:

$$\theta = \text{atan2}(Y - Y_c, X - X_c) - \text{Angle}$$

The normalized offset $u \in [0, 1)$ is determined by mapping $\theta \pmod{2\pi}$ onto $[0, 1]$:

$$u = \frac{\theta \pmod{2\pi}}{2\pi}$$

## See Also
- [Conic Gradient - W3C CSS Images](https://www.w3.org/TR/css-images-4/#conic-gradients)
- [[TSweepGradientSampler]]

[members]
