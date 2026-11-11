---
layout: doc
docType: api
unit: GR32_Resamplers
entity: TAdaptiveSuperSampler
kind: Class
summary: "Adaptive super-sampler using recursive quadtree subdivision in high-contrast image regions."
declaration: "TAdaptiveSuperSampler = class(TNestedSampler)"
inheritance:
  - TObject
  - TPersistent
  - TNotifiablePersistent
  - TCustomSampler
  - TNestedSampler
  - TAdaptiveSuperSampler
---

## Description

`TAdaptiveSuperSampler` adaptively collects additional sub-pixel samples in high-contrast or high-frequency edge regions using recursive quadtree subdivision.

[members]
