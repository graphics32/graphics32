---
layout: doc
docType: api
unit: GR32_Resamplers
entity: TTransformer
kind: Class
summary: "Nested sampler that applies spatial coordinate transformations to sampling positions."
declaration: "TTransformer = class(TNestedSampler)"
inheritance:
  - TObject
  - TPersistent
  - TNotifiablePersistent
  - TCustomSampler
  - TNestedSampler
  - TTransformer
---

## Description

`TTransformer` wraps a nested sampler and transforms sampling coordinates using a [[TTransformation]] instance.

[members]
