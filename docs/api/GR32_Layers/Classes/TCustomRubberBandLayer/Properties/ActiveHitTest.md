---
layout: doc
docType: api
unit: GR32_Layers
parent: TCustomRubberBandLayer
entity: TCustomRubberBandLayer.ActiveHitTest
kind: Property
scope: Public
declaration: "property ActiveHitTest: ILayerHitTest read FHitTest;"
summary: "References the active hit test state while a drag operation is in progress."
---

## Description

`ActiveHitTest` returns `nil` when no drag operation is occurring, or an active [[ILayerHitTest]] interface during mouse interaction.
