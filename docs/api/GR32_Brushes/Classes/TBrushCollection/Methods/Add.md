---
layout: doc
docType: api
unit: GR32_Brushes
parent: TBrushCollection
entity: TBrushCollection.Add
kind: Method
declaration: "function Add(ItemClass: TBrushClass): TCustomBrush;"
summary: "Creates a new TCustomBrush instance of class ItemClass and adds it to the collection."
parameters:
  - name: ItemClass
    type: TBrushClass
    description: "Specific TCustomBrush class type to instantiate."
returns:
  - type: TCustomBrush
    description: "The newly created [[TCustomBrush]] instance added to the collection."
---

## Description

`Add` instantiates a new brush object of type `ItemClass`, attaches it to this collection, assigns its index to the end of the collection list, and returns the newly created [[TCustomBrush]] instance.
