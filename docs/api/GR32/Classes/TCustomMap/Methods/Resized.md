---
layout: doc
docType: api
unit: GR32
parent: TCustomMap
entity: TCustomMap.Resized
kind: Method
declaration: "procedure Resized; virtual;"
summary: "Triggers resize processing and fires OnResize."
---

## Description

`Resized` is called internally when `Width` or `Height` changes. It calls `Changed` and invokes the `OnResize` event handler if assigned.
