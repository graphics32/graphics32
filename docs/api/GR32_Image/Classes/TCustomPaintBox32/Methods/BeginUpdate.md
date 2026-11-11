---
layout: doc
docType: api
unit: GR32_Image
parent: TCustomPaintBox32
entity: TCustomPaintBox32.BeginUpdate
kind: Method
scope: Public
declaration: "procedure BeginUpdate;"
summary: "Increments update count to defer change notifications."
---

## Description

`BeginUpdate` increments the internal update counter, **deferring** change notifications ([[OnChange]]) until a matching [[EndUpdate]] call is executed.
