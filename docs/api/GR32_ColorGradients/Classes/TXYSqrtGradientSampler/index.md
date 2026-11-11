---
layout: doc
docType: api
unit: GR32_ColorGradients
entity: TXYSqrtGradientSampler
kind: Class
summary: "Square-root product gradient sampler."
declaration: "TXYSqrtGradientSampler = class(TCustomCenterRadiusAngleLutGradientSampler)"
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
  - TXYSqrtGradientSampler
---

## Description

`TXYSqrtGradientSampler` samples a square-root product gradient defined by the square root of the multiplication of the X and Y coordinate values.

It is related to the [[TXYGradientSampler|XY color gradient]], but mapped using the square root, resulting in more evenly spaced colors.

Its size can be controlled using the [[Radius]] property, which is actually some sort of an incircle radius. The center can be set using the [[Center]] property. Finally, the gradient can be rotated using the [[Angle]] property.

| Clamp | Mirror | Repeat |
| --- | --- | --- |
| ![](/images/gradient-sampler-xy-sqrt-clamp.png) | ![](/images/gradient-sampler-xy-sqrt-mirror.png) | ![](/images/gradient-sampler-xy-sqrt-repeat.png) |

## Mathematics & Algorithm

Given local rotated offset coordinates $(X', Y')$ relative to center $(X_c, Y_c)$:

$$u = \frac{\sqrt{|X' \cdot Y'|}}{R}$$

[members]
