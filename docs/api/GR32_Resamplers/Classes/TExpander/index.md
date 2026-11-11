---
layout: doc
docType: api
unit: GR32_Resamplers
entity: TExpander
kind: Class
summary: "Neighborhood sampler that performs multiplicative expansion on local pixel neighborhoods."
declaration: "TExpander = class(TKernelSampler)"
inheritance:
  - TObject
  - TPersistent
  - TNotifiablePersistent
  - TCustomSampler
  - TNestedSampler
  - TKernelSampler
  - TExpander
---

## Description

`TExpander` performs a multiplicative neighborhood expansion operation similar to morphological dilation.

[members]
