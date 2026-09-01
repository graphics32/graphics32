---
layout: doc
docType: api
unit: GR32_Resamplers
parent: TKernelResampler
entity: TKernelResampler.Kernel
kind: Property
scope: Published
declaration: "property Kernel: TCustomKernel read FKernel write SetKernel;"
summary: "The spatial reconstruction kernel instance used for resampling."
---

## Description

`Kernel` specifies the assigned [[TCustomKernel]] instance (e.g. [[TLanczosKernel]], [[TCubicKernel]], [[TMitchellKernel]]) used to compute interpolation weights.
