---
layout: doc
docType: api
unit: GR32_ColorGradients
entity: TDiamondGradientSampler
kind: Class
summary: "Samples a diamond-shaped (L1 Manhattan norm) color gradient."
declaration: "TDiamondGradientSampler = class(TCustomCenterRadiusAngleLutGradientSampler)"
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
  - TDiamondGradientSampler
---

<!-- TODO ## Description missing -->

| Clamp | Mirror | Repeat |
| --- | --- | --- |
| ![](/images/gradient-sampler-diamond-clamp.png) | ![](/images/gradient-sampler-diamond-mirror.png) | ![](/images/gradient-sampler-diamond-repeat.png) |

Colors are mapped according to [[WrapMode]].

## Mathematics & Algorithm

`TDiamondGradientSampler` calculates normalized distance $u$ using the Manhattan ($L_1$) distance norm from center $(X_c, Y_c)$ scaled by $R$:

$$u = \frac{|X'| + |Y'|}{R}$$

where $(X', Y')$ is $(X - X_c, Y - Y_c)$ rotated by `Angle`.

[members]
