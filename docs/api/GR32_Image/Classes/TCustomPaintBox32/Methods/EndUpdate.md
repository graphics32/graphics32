---
layout: doc
docType: api
unit: GR32_Image
parent: TCustomPaintBox32
entity: TCustomPaintBox32.EndUpdate
kind: Method
scope: Public
declaration: "procedure EndUpdate;"
summary: "Decrements update count and dispatches pending change notifications when zero."
---

## Description

`EndUpdate` decrements the update counter. When the counter reaches zero, any deferred change notifications are dispatched via [[Changed]].
