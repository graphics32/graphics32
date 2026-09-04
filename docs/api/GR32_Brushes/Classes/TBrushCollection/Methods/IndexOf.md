---
layout: doc
docType: api
unit: GR32_Brushes
parent: TBrushCollection
entity: TBrushCollection.IndexOf
kind: Method
declaration: "function IndexOf(Item: TCustomBrush): Integer;"
summary: "Returns the zero-based index of a specified brush item in the collection."
parameters:
  - name: Item
    type: TCustomBrush
    description: "Brush instance to locate."
returns:
  - type: Integer
    description: "The zero-based position index of the specified brush in the collection, or `-1` if not found."
---

## Description

`IndexOf` searches the collection for `Item` and returns its zero-based position in the collection, or `-1` if the item is not present.
