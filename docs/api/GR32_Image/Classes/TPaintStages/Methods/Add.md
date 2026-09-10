---
layout: doc
docType: api
unit: GR32_Image
parent: TPaintStages
entity: TPaintStages.Add
kind: Method
scope: Public
declaration: "function Add: PPaintStage;"
summary: "Appends a new TPaintStage record to the end of the stage list."
returns:
  - type: PPaintStage
    description: "Pointer to the newly added TPaintStage record."
---

## Description

`Add` allocates and appends a new `TPaintStage` record to the internal array of stages maintained by [[TPaintStages]] and returns a pointer to the record for field initialization.
