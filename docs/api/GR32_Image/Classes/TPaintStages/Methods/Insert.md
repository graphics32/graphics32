---
layout: doc
docType: api
unit: GR32_Image
parent: TPaintStages
entity: TPaintStages.Insert
kind: Method
scope: Public
declaration: "function Insert(Index: Integer): PPaintStage;"
summary: "Inserts a new paint stage item at the specified zero-based index."
parameters:
  - name: Index
    type: Integer
    description: "Zero-based index position where the new stage item should be inserted."
returns:
  - type: PPaintStage
    description: "Pointer to the newly inserted TPaintStage record."
---

## Description

`Insert` inserts a new `TPaintStage` record at position `Index` in [[TPaintStages]] and returns a pointer to the record.
