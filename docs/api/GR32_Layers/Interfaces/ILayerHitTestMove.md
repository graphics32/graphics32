---
layout: doc
docType: api
unit: GR32_Layers
entity: ILayerHitTestMove
kind: Interface
declaration: "ILayerHitTestMove = interface(ILayerHitTest)"
summary: "Interface representing a hit-test context for moving a rubberband layer as a whole."
---

## Description

`ILayerHitTestMove` inherits from `ILayerHitTest` to indicate that a layer body move (drag) operation is active rather than an individual vertex handle edit.
