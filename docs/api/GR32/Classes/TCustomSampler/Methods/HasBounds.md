---
layout: doc
docType: api
unit: GR32
parent: TCustomSampler
entity: TCustomSampler.HasBounds
kind: Method
scope: Public
declaration: "function HasBounds: Boolean; virtual;"
summary: "Returns True if the sampler has defined spatial boundary limits."
returns:
  - type: Boolean
    description: "Returns `True` if the sampler enforces sampling coordinate boundaries; otherwise `False`."
---

## Description

`HasBounds` indicates whether the sampler operates within a finite bounding region.

In the base `TCustomSampler` class, `HasBounds` returns `False`, indicating an infinite sampling domain (typical for mathematical procedural patterns or infinite gradients). Derived classes such as `TCustomResampler` override `HasBounds` to return `True` when bounded by a source bitmap or defined clipping rectangle (e.g., when `PixelAccessMode` is not set to `pamWrap`).

## Example

```pascal
if Sampler.HasBounds then
  BoundsRect := Sampler.GetSampleBounds
else
  // Sampler covers an infinite spatial domain
```
