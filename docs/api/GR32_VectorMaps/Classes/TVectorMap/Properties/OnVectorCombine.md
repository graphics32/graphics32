---
layout: doc
docType: api
unit: GR32_VectorMaps
parent: TVectorMap
entity: TVectorMap.OnVectorCombine
kind: Event
aliases: [TVectorCombineEvent]
declaration: |
  type TVectorCombineEvent = procedure(F, P: TFixedVector; var B: TFixedVector) of object;
  property OnVectorCombine: TVectorCombineEvent read FOnVectorCombine write FOnVectorCombine;
summary: "Event handler for custom vector combination logic during vector map merge operations."
parameters:
  - name: F
    type: TFixedVector
    description: "Source displacement vector."
  - name: P
    type: TFixedVector
    description: "Normalized progression vector (-1..1) across destination merge rectangle."
  - name: B
    type: TFixedVector
    description: "Destination displacement vector to be updated in place."
---

## Description

`OnVectorCombine` is triggered during [[TVectorMap.Merge]] when [[TVectorMap.VectorCombineMode]] is set to `vcmCustom`.

The event delegate `TVectorCombineEvent` receives the source displacement vector `F`, normalized linear progression coordinates `P` across the merge rectangle $[-1, 1]$, and destination displacement vector `B` to calculate and store the custom combined vector in `B`.
