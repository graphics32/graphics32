---
layout: doc
docType: api
unit: GR32_Layers
parent: TCustomLayer
entity: TCustomLayer.LayerCollection
kind: Property
scope: Public
declaration: "property LayerCollection: TLayerCollection read ... write ...;"
summary: "References the TLayerCollection owning and managing this layer."
---

## Description

`LayerCollection` specifies the [[TLayerCollection]] that owns the layer.

Modifying `LayerCollection` detaches the layer from its current collection and attaches it to the new (if any).
