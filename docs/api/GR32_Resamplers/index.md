---
layout: doc
docType: api
unit: GR32_Resamplers
entity: GR32_Resamplers
kind: Unit
summary: "Contains resampling kernels and interpolation filters used for bitmap scaling and transformations."
---

## Description

The `GR32_Resamplers` unit contains resampling kernels and interpolation filters used for bitmap scaling and transformations.

---

## Classes

- `TLinearResampler`: Bilinear interpolation resampler.
- `TNearestResampler`: Nearest neighbor (point) resampler.
- `TKernelResampler`: High-quality windowed sinc / Lanczos / Gaussian resampler.
