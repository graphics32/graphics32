---
layout: doc
docType: api
unit: GR32_Resamplers
entity: TKernelSampler
kind: Class
abstract: true
summary: "Abstract base class for neighborhood samplers using an integer kernel map."
declaration: "TKernelSampler = class(TNestedSampler)"
inheritance:
  - TObject
  - TPersistent
  - TNotifiablePersistent
  - TCustomSampler
  - TNestedSampler
  - TKernelSampler
---

## Description

`TKernelSampler` is the abstract base class for neighborhood samplers that evaluate pixels over a local 2D kernel area specified by an integer map (`Kernel`).

[members]
