---
layout: doc
docType: api
unit: GR32_Layers
parent: TLayerCollection
entity: TLayerCollection.Delete
kind: Method
scope: Public
declaration: "procedure Delete(Index: Integer);"
summary: "Deletes and frees the layer at the specified index."
parameters:
  - name: Index
    type: Integer
    description: "Zero-based index of the layer to delete."
---

## Description

`Delete` hides the layer at `Index`, removes it from the collection, and disposes of the object.
