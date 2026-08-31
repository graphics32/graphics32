---
layout: doc
docType: api
unit: GR32_ColorGradients
entity: TXYGradientSampler
kind: Class
summary: "Bilinear product gradient sampler."
declaration: "TXYGradientSampler = class(TCustomCenterRadiusAngleLutGradientSampler)"
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
  - TXYGradientSampler
---

## Description

`TXYGradientSampler` samples a bilinear product gradient defined by the multiplication of the X and Y coordinate values.

Its size can be controlled using the [[Radius]] property, which is actually some sort of an incircle radius. The center can be set using the [[Center]] property. Finally the gradient can be rotated using the [[Angle]] property.

| Clamp | Mirror | Repeat |
| --- | --- | --- |
| ![](/images/gradient-sampler-xy-clamp.png) | ![](/images/gradient-sampler-xy-mirror.png) | ![](/images/gradient-sampler-xy-repeat.png) |

## Mathematics & Algorithm

Given local rotated offset coordinates $(X', Y')$ relative to center $(X_c, Y_c)$:

$$u = \frac{|X' \cdot Y'|}{R^2}$$

[members]
