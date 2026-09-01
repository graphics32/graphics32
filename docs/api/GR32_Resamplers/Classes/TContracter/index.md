---
layout: doc
docType: api
unit: GR32_Resamplers
entity: TContracter
kind: Class
summary: "Neighborhood sampler that performs morphological pixel contraction."
declaration: "TContracter = class(TExpander)"
inheritance:
  - TObject
  - TPersistent
  - TNotifiablePersistent
  - TCustomSampler
  - TNestedSampler
  - TKernelSampler
  - TExpander
  - TContracter
---

## Description

`TContracter` performs a neighborhood contraction operation complementary to [[TExpander]], reducing local pixel values according to the kernel weights.

[members]
