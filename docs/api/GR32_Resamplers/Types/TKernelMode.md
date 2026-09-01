---
layout: doc
docType: api
unit: GR32_Resamplers
entity: TKernelMode
kind: Type
summary: "Specifies the evaluation mode used by TKernelResampler to compute kernel weights."
declaration: "TKernelMode = (kmDynamic, kmTableNearest, kmTableLinear);"
---

## Description

`TKernelMode` defines how [[TKernelResampler]] computes weight coefficients from the assigned reconstruction kernel during sampling.

| Value | Description |
| --- | --- |
| `kmDynamic` | Evaluates the kernel filter function dynamically on the fly for every sample. Offers maximum precision but is computationally slower for complex mathematical kernels. |
| `kmTableNearest` | Precomputes kernel filter values into a discrete lookup table (`FWeightTable`) during `PrepareSampling` and uses nearest table lookup during resampling. |
| `kmTableLinear` | Precomputes kernel filter values into a lookup table and linearly interpolates between adjacent table entries. Balances high execution speed with sub-table accuracy. |
