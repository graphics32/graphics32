---
layout: doc
docType: api
unit: GR32_Resamplers
entity: TMorphologicalSampler
kind: Class
summary: "Base class for morphological operation samplers with internal buffer management."
declaration: "TMorphologicalSampler = class(TKernelSampler)"
inheritance:
  - TObject
  - TPersistent
  - TNotifiablePersistent
  - TCustomSampler
  - TNestedSampler
  - TKernelSampler
  - TMorphologicalSampler
---

## Description

`TMorphologicalSampler` serves as the abstract base class for morphological filter samplers, providing internal line buffer management and temporary storage for multi-pass morphological operations.

[members]
