---
layout: doc
docType: api
unit: GR32
parent: TCustomBackend
entity: TCustomBackend.Clear
kind: Method
scope: Public
declaration: "procedure Clear; virtual;"
summary: "Deallocates the surface buffer and resets dimensions to zero."
---

## Description

`Clear` calls `ChangeSize` with dimensions `(0, 0)`, triggering `FinalizeSurface` to release allocated buffer memory.
