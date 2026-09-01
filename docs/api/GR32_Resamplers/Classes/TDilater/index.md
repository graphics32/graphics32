---
layout: doc
docType: api
unit: GR32_Resamplers
entity: TDilater
kind: Class
summary: "Neighborhood sampler that performs morphological dilation on bitmap regions."
declaration: "TDilater = class(TMorphologicalSampler)"
inheritance:
  - TObject
  - TPersistent
  - TNotifiablePersistent
  - TCustomSampler
  - TNestedSampler
  - TKernelSampler
  - TMorphologicalSampler
  - TDilater
---

## Description

`TDilater` performs morphological dilation, expanding bright regions in the target bitmap based on the kernel structuring element.

[members]
