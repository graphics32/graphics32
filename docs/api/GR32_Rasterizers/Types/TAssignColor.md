---
layout: doc
docType: api
unit: GR32_Rasterizers
entity: TAssignColor
kind: Type
declaration: "type TAssignColor = procedure(var Dst: TColor32; Src: TColor32) of object;"
summary: "Method pointer type used internally by rasterizers to perform pixel color assignment and blending."
parameters:
  - name: Dst
    type: TColor32
    description: "Reference to the destination pixel color in the bitmap buffer to be updated."
  - name: Src
    type: TColor32
    description: "Source sample color evaluated by the rasterizer's attached TCustomSampler."
---

## Description

`TAssignColor` defines the procedural event signature used by [[TRasterizer]] internal pixel assignment delegates.

Depending on the rasterization `DrawMode` and `CombineMode` specified in [[TCombineInfo]], [[TRasterizer]] assigns an appropriate `TAssignColor` handler (such as opaque replacement, alpha blending, transparent color skipping, or custom combine callbacks) to minimize conditional branching inside high-frequency pixel loops.
