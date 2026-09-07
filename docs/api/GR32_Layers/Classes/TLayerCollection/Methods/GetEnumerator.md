---
layout: doc
docType: api
unit: GR32_Layers
parent: TLayerCollection
entity: TLayerCollection.GetEnumerator
kind: Method
scope: Public
declaration: "function GetEnumerator: TEnumerator<TCustomLayer>;"
summary: "Returns an enumerator for iterating through layers in the collection using for..in loops."
returns:
  - type: TEnumerator<TCustomLayer>
    description: "Layer enumerator instance."
---

## Description

`GetEnumerator` enables standard Pascal `for..in` iteration over all `TCustomLayer` items in the collection.
