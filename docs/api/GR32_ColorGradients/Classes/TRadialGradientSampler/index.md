---
layout: doc
docType: api
unit: GR32_ColorGradients
entity: TRadialGradientSampler
kind: Class
summary: "Circular or elliptical radial sampler"
declaration: "TRadialGradientSampler = class(TCustomCenterRadiusLutGradientSampler)"
inheritance:
  - TObject
  - TPersistent
  - TNotifiablePersistent
  - TCustomSampler
  - TCustomGradientSampler
  - TCustomGradientLookUpTableSampler
  - TCustomCenterLutGradientSampler
  - TCustomCenterRadiusLutGradientSampler
  - TRadialGradientSampler
---

## Description

`TRadialGradientSampler` samples a symmetric 2D circular radial gradient around a center point.

<!-- TODO: more description -->

| Clamp | Mirror | Repeat |
| --- | --- | --- |
| ![](/images/gradient-sampler-radial-clamp.png) | ![](/images/gradient-sampler-radial-mirror.png) | ![](/images/gradient-sampler-radial-repeat.png) |

Colors are mapped according to [[WrapMode]].

## Mathematics & Algorithm

`TRadialGradientSampler` calculates normalized radial distance $u$ from center $(X_c, Y_c)$ using Euclidean norm divided by radius $R$:

$$u = \frac{\sqrt{(X - X_c)^2 + (Y - Y_c)^2}}{R}$$

[members]
