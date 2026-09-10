---
layout: doc
docType: api
unit: GR32_Image
parent: TCustomPaintBox32
entity: TCustomPaintBox32.BeginLockUpdate
kind: Method
scope: Public
declaration: "procedure BeginLockUpdate;"
summary: "Locks repainting and locks change notification updates."
---

## Description

`BeginLockUpdate` increments the internal lock update count to **suppress** all intermediate visual repaints during batch update operations.
