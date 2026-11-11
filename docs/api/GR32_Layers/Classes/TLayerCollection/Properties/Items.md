---
layout: doc
docType: api
unit: GR32_Layers
parent: TLayerCollection
entity: TLayerCollection.Items
kind: Property
scope: Public
declaration: "property Items[Index: Integer]: TCustomLayer read GetItem write SetItem; default;"
summary: "Provides indexed access to layers in the collection."
parameters:
  - name: Index
    type: Integer
    description: "Zero-based index of the layer."
---

## Description

`Items` is the default array property providing access to layers by index position.
