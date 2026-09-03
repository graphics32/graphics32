---
layout: doc
docType: api
unit: GR32_Brushes
parent: TBrushCollection
entity: TBrushCollection.Insert
kind: Method
declaration: "function Insert(Index: Integer; ItemClass: TBrushClass): TCustomBrush;"
summary: "Instantiates a new TCustomBrush of class ItemClass and inserts it at the specified position."
parameters:
  - name: Index
    type: Integer
    description: "Zero-based target position index where the new brush item will be inserted."
  - name: ItemClass
    type: TBrushClass
    description: "Class type of the brush item to instantiate."
---

## Description

`Insert` instantiates a new brush object of type `ItemClass` and inserts it at position `Index` in the collection.
