---
layout: doc
docType: api
unit: GR32_Resamplers
parent: TKernelResampler
entity: TKernelResampler.KernelMode
kind: Property
scope: Published
declaration: "property KernelMode: TKernelMode read FKernelMode write SetKernelMode;"
summary: "Evaluation mode used to compute kernel weights."
---

## Description

`KernelMode` selects between dynamic evaluation (`kmDynamic`) and precomputed weight table lookup (`kmTableNearest`, `kmTableLinear`).

## See also
- [[TKernelMode]]