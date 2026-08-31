---
layout: doc
docType: api
unit: GR32_ColorGradients
entity: TCustomCenterLutGradientSampler
kind: Class
summary: "Base class for lookup table gradient samplers anchored around a 2D center point."
declaration: "TCustomCenterLutGradientSampler = class(TCustomGradientLookUpTableSampler)"
inheritance:
  - TObject
  - TPersistent
  - TNotifiablePersistent
  - TCustomSampler
  - TCustomGradientSampler
  - TCustomGradientLookUpTableSampler
  - TCustomCenterLutGradientSampler
---

## Description

`TCustomCenterLutGradientSampler` provides a `Center` position property $(X, Y)$ serving as the coordinate origin for radial, conic, diamond, and angular gradient transformations.

[members]
