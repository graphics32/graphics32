---
layout: doc
docType: api
unit: GR32_Resamplers
entity: TLinearResampler
kind: Class
declaration: "TLinearResampler = class(TCustomResampler)"
inheritance:
  - TObject
  - TPersistent
  - TNotifiablePersistent
  - TCustomSampler
  - TCustomResampler
  - TLinearResampler
summary: "Performance-optimized linear upsampler."
---

## Description

`TLinearResampler` provides fast bilinear interpolation, over a 2x2 pixel grid around fractional sampling coordinates for bitmap *magnification*, falling back to [[TLinearKernel]] for *minification*.

[members]
