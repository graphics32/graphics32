---
layout: doc
docType: api
unit: GR32_Resamplers
entity: TEroder
kind: Class
summary: "Neighborhood sampler that performs morphological erosion on bitmap regions."
declaration: "TEroder = class(TMorphologicalSampler)"
inheritance:
  - TObject
  - TPersistent
  - TNotifiablePersistent
  - TCustomSampler
  - TNestedSampler
  - TKernelSampler
  - TMorphologicalSampler
  - TEroder
---

## Description

`TEroder` performs morphological erosion, shrinking bright regions in the target bitmap.

[members]
