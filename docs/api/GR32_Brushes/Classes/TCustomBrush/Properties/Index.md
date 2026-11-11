---
layout: doc
docType: api
unit: GR32_Brushes
parent: TCustomBrush
entity: TCustomBrush.Index
kind: Property
declaration: "property Index: Integer read GetIndex write SetIndex;"
summary: "Zero-based position index of this brush item in its parent brush collection."
---

## Description

`Index` specifies the position of this brush within its parent [[BrushCollection]]. Re-assigning `Index` moves the brush within the collection list, modifying execution order in multi-brush pipelines.
