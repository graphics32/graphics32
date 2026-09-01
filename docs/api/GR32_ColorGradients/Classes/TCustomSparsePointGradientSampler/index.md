---
layout: doc
docType: api
unit: GR32_ColorGradients
entity: TCustomSparsePointGradientSampler
kind: Class
abstract: true
summary: "Abstract base class for sparse point color gradient samplers based on scattered vertex colors."
declaration: "TCustomSparsePointGradientSampler = class(TCustomSampler)"
inheritance:
  - TObject
  - TPersistent
  - TNotifiablePersistent
  - TCustomSampler
  - TCustomSparsePointGradientSampler
---

## Description

`TCustomSparsePointGradientSampler` provides indexed access (`ColorPoint`, `Point`, `Color`) to a set of color vertices ([[TColor32FloatPoint]]).

[members]
