---
layout: doc
docType: api
unit: GR32_Resamplers
entity: TSuperSampler
kind: Class
summary: "Nested sampler performing uniform grid sub-pixel super-sampling anti-aliasing."
declaration: "TSuperSampler = class(TNestedSampler)"
inheritance:
  - TObject
  - TPersistent
  - TNotifiablePersistent
  - TCustomSampler
  - TNestedSampler
  - TSuperSampler
---

## Description

`TSuperSampler` takes a regular grid of sub-pixel samples per pixel ($N \times M$) across the nested sampler to perform uniform anti-aliasing.

[members]
