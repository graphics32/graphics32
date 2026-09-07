---
layout: doc
docType: api
unit: GR32_Layers
parent: TLayerCollection
entity: TLayerCollection.IndexOf
kind: Method
scope: Public
declaration: "function IndexOf(Item: TCustomLayer): Integer;"
summary: "Returns the zero-based index of the specified layer in the collection."
parameters:
  - name: Item
    type: TCustomLayer
    description: "Layer instance to locate."
returns:
  - type: Integer
    description: "Zero-based index of the layer, or -1 if not found."
---

## Description

`IndexOf` searches the collection list for `Item` and returns its index position.
