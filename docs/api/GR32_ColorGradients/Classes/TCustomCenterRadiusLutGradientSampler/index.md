---
layout: doc
docType: api
unit: GR32_ColorGradients
entity: TCustomCenterRadiusLutGradientSampler
kind: Class
summary: "Base class for centered gradient samplers bounded by a radial scaling factor."
declaration: "TCustomCenterRadiusLutGradientSampler = class(TCustomCenterLutGradientSampler)"
inheritance:
  - TObject
  - TPersistent
  - TNotifiablePersistent
  - TCustomSampler
  - TCustomGradientSampler
  - TCustomGradientLookUpTableSampler
  - TCustomCenterLutGradientSampler
  - TCustomCenterRadiusLutGradientSampler
---

## Description

`TCustomCenterRadiusLutGradientSampler` introduces a `Radius` scalar property defining the spatial distance over which the gradient spans from 0.0 to 1.0.

[members]
