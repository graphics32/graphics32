---
layout: doc
docType: api
unit: GR32_Brushes
parent: TBrushCollection
entity: TBrushCollection.Items
kind: Property
declaration: "property Items[Index: Integer]: TCustomBrush read GetItem write SetItem; default;"
summary: "Indexed default property accessing TCustomBrush items in the collection."
parameters:
  - name: Index
    type: Integer
    description: "Zero-based index of the brush item."
---

## Description

`Items` provides indexed array access to individual [[TCustomBrush]] items in the collection. `Items` is the default property of `TBrushCollection`.
