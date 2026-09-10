---
layout: doc
docType: api
unit: GR32
parent: TBitmap32
entity: TBitmap32.UpdateFont
kind: Method
scope: Public
declaration: "procedure UpdateFont;"
summary: "Notifies the backend that font property changes have occurred and updates font settings."
---

## Description

`UpdateFont` synchronizes font modifications with the underlying backend implementation supporting font operations (`IFontSupport`). Call `UpdateFont` when font properties are modified directly or need explicit backend updating.
