---
layout: doc
docType: api
unit: GR32_Image
parent: TPaintStages
entity: TPaintStages.Items
kind: Property
scope: Public
declaration: "property Items[Index: Integer]: PPaintStage read GetItem; default;"
summary: "Provides indexed access to PPaintStage pointers in the collection."
parameters:
  - name: Index
    type: Integer
    description: "Zero-based index of the stage item."
returns:
  - type: PPaintStage
    description: "Pointer to the TPaintStage record at the specified index."
---

## Description

`Items` provides default array indexing access to `PPaintStage` record pointers stored in [[TPaintStages]].
