---
layout: doc
docType: api
unit: GR32_Resamplers
entity: TConvolver
kind: Class
summary: "Neighborhood sampler that performs discrete 2D spatial matrix convolution."
declaration: "TConvolver = class(TKernelSampler)"
inheritance:
  - TObject
  - TPersistent
  - TNotifiablePersistent
  - TCustomSampler
  - TNestedSampler
  - TKernelSampler
  - TConvolver
---

## Description

`TConvolver` performs discrete spatial 2D matrix convolution over neighborhood pixels for image sharpening, smoothing, and edge detection.

[members]
