---
layout: doc
docType: api
unit: GR32_ColorGradients
entity: TCustomGradientLookUpTableSampler
kind: Class
summary: "Base class for gradient samplers utilizing a fast pre-calculated color lookup table."
declaration: "TCustomGradientLookUpTableSampler = class(TCustomGradientSampler)"
inheritance:
  - TObject
  - TPersistent
  - TNotifiablePersistent
  - TCustomSampler
  - TCustomGradientSampler
  - TCustomGradientLookUpTableSampler
---

## Description

`TCustomGradientLookUpTableSampler` extends [[TCustomGradientSampler]] by maintaining an internal [[TColor32LookupTable]] buffer to accelerate sampling operations.

[members]
