---
layout: doc
docType: api
unit: GR32_Transforms
entity: RasterizeTransformation
kind: Procedure
declaration: |
  procedure RasterizeTransformation(Vectormap: TVectormap;
    Transformation: TTransformation; DstRect: TRect;
    CombineMode: TVectorCombineMode = vcmAdd;
    CombineCallback: TVectorCombineEvent = nil);
summary: "Rasterizes coordinate displacement vectors of a TTransformation into a TVectorMap."
parameters:
  - name: Vectormap
    type: TVectorMap
    description: "Target vector map."
  - name: Transformation
    type: TTransformation
    description: "Transformation object whose inverse displacement vectors are evaluated."
  - name: DstRect
    type: TRect
    description: "Destination rectangle."
  - name: CombineMode
    type: TVectorCombineMode
    description: "Vector combination mode (`vcmAdd`, `vcmReplace`, or `vcmCustom`)."
  - name: CombineCallback
    type: TVectorCombineEvent
    description: "Optional custom combination callback."
---

## Description

`RasterizeTransformation` evaluates `Transformation` across `DstRect` and stores calculated coordinate offset vectors into `Vectormap`.
