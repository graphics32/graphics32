---
layout: doc
docType: api
unit: GR32_Brushes
parent: TBrushCollection
entity: TBrushCollection.Delete
kind: Method
declaration: "procedure Delete(Index: Integer);"
summary: "Frees and removes the brush instance at the specified index."
parameters:
  - name: Index
    type: Integer
    description: "Zero-based index of the brush item to delete."
---

## Description

`Delete` frees the brush instance located at `Index` in the collection and removes its reference from the internal list.
