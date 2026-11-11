---
layout: doc
docType: api
unit: GR32_ColorGradients
entity: TCustomCenterRadiusAngleLutGradientSampler
kind: Class
abstract: true
summary: "Base class for centered gradient samplers supporting both radius scaling and angular rotation."
declaration: "TCustomCenterRadiusAngleLutGradientSampler = class(TCustomCenterRadiusLutGradientSampler)"
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
---

## Description

`TCustomCenterRadiusAngleLutGradientSampler` adds an `Angle` property to rotate the gradient coordinate space in radians.

[members]
