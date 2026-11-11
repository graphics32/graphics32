---
layout: doc
docType: api
unit: GR32_Resamplers
entity: TPatternSampler
kind: Class
summary: "Nested sampler that evaluates sub-pixel sample points using a fixed 2D offset pattern grid."
declaration: "TPatternSampler = class(TNestedSampler)"
inheritance:
  - TObject
  - TPersistent
  - TNotifiablePersistent
  - TCustomSampler
  - TNestedSampler
  - TPatternSampler
---

## Description

`TPatternSampler` evaluates sub-pixel sample points specified by a 2D offset pattern grid (`Pattern`), accumulating and averaging multiple sub-pixel samples for custom anti-aliasing.

[members]
