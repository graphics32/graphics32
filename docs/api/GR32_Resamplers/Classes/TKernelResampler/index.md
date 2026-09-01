---
layout: doc
docType: api
unit: GR32_Resamplers
entity: TKernelResampler
kind: Class
declaration: "TKernelResampler = class(TCustomResampler)"
inheritance:
  - TObject
  - TPersistent
  - TNotifiablePersistent
  - TCustomSampler
  - TCustomResampler
  - TKernelResampler
summary: "Generic separable 2D kernel window resampler using TCustomKernel instances."
---

## Description

`TKernelResampler` resamples bitmaps using an arbitrary [[TCustomKernel]] descendant (such as [[TLanczosKernel]], [[TCubicKernel]], or [[TMitchellKernel]]).

It supports dynamic evaluation as well as nearest and linear weight lookup tables ([[TKernelMode]]) for high performance.

[members]
