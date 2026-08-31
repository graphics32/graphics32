---
layout: doc
docType: api
unit: GR32_ColorGradients
entity: TSweepGradientSampler
kind: Class
summary: "Angular sweep gradient sampler."
declaration: "TSweepGradientSampler = class(TCustomCenterLutGradientSampler)"
inheritance:
  - TObject
  - TPersistent
  - TNotifiablePersistent
  - TCustomSampler
  - TCustomGradientSampler
  - TCustomGradientLookUpTableSampler
  - TCustomCenterLutGradientSampler
  - TSweepGradientSampler
---

## Description

`TSweepGradientSampler` samples an angular sweep gradient bounded between a specified [[StartAngle]] and [[EndAngle]] angle.

<!-- TODO: more description -->

## Mathematics & Algorithm

`TSweepGradientSampler` maps polar angles $\theta$ bounded between `StartAngle` ($\theta_0$) and `EndAngle` ($\theta_1$):

$$u = \frac{\theta - \theta_0}{\theta_1 - \theta_0}$$

Where $\theta = \text{atan2}(Y - Y_c, X - X_c)$.

[members]
