---
layout: doc
docType: api
unit: GR32_Brushes
parent: TNestedBrush
entity: TNestedBrush.Brushes
kind: Property
declaration: "property Brushes: TBrushCollection read FBrushes;"
summary: "Read-only property referencing the internal child TBrushCollection."
---

## Description

`Brushes` references the child [[TBrushCollection]] owned by this nested brush. Child brushes added to this collection are rendered in order whenever `TNestedBrush` is invoked.
