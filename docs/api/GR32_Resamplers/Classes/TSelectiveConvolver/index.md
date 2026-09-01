---
layout: doc
docType: api
unit: GR32_Resamplers
entity: TSelectiveConvolver
kind: Class
summary: "Neighborhood convolver that performs edge-preserving selective bilateral filtering."
declaration: "TSelectiveConvolver = class(TConvolver)"
inheritance:
  - TObject
  - TPersistent
  - TNotifiablePersistent
  - TCustomSampler
  - TNestedSampler
  - TKernelSampler
  - TConvolver
  - TSelectiveConvolver
---

## Description

`TSelectiveConvolver` performs edge-preserving bilateral filtering by ignoring surrounding color samples whose color difference relative to the center sample exceeds `Delta`.

[members]
