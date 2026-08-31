---
layout: doc
docType: api
unit: GR32_ColorGradients
entity: TCustomArbitrarySparsePointGradientSampler
kind: Class
summary: "Base class for sparse point gradient samplers accepting an arbitrary count of scatter vertices."
declaration: "TCustomArbitrarySparsePointGradientSampler = class(TCustomSparsePointGradientSampler)"
inheritance:
  - TObject
  - TPersistent
  - TNotifiablePersistent
  - TCustomSampler
  - TCustomSparsePointGradientSampler
  - TCustomArbitrarySparsePointGradientSampler
---

## Description

`TCustomArbitrarySparsePointGradientSampler` extends [[TCustomSparsePointGradientSampler]] by supporting dynamic addition (`Add`), clearing (`Clear`), and arbitrary arrays of scatter vertices.

[members]
