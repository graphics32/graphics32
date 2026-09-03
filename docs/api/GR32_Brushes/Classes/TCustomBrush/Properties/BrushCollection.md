---
layout: doc
docType: api
unit: GR32_Brushes
parent: TCustomBrush
entity: TCustomBrush.BrushCollection
kind: Property
declaration: "property BrushCollection: TBrushCollection read FBrushCollection write SetBrushCollection;"
summary: "Specifies the parent TBrushCollection managing this brush item."
---

## Description

`BrushCollection` references the [[TBrushCollection]] instance that owns this brush item. Assigning a new collection automatically detaches the brush from its previous collection and inserts it into the new collection.
