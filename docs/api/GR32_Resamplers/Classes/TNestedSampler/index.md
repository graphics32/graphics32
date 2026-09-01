---
layout: doc
docType: api
unit: GR32_Resamplers
entity: TNestedSampler
kind: Class
summary: "Base class for pipeline samplers that wrap and transform an inner sampler."
declaration: "TNestedSampler = class(TCustomSampler)"
inheritance:
  - TObject
  - TPersistent
  - TNotifiablePersistent
  - TCustomSampler
  - TNestedSampler
---

## Description

`TNestedSampler` provides a foundation for building pipeline samplers that wrap an inner `Sampler` property and modify or transform sampling coordinates or colors.

[members]
