---
layout: doc
docType: api
unit: GR32_Brushes
parent: TCustomBrush
entity: TCustomBrush.Create
kind: Constructor
declaration: "constructor Create(ABrushCollection: TBrushCollection); virtual;"
summary: "Creates and initializes a new TCustomBrush instance attached to a brush collection."
parameters:
  - name: ABrushCollection
    type: TBrushCollection
    description: "Brush collection to attach this brush to, or nil."
---

## Description

`Create` initializes a new `TCustomBrush` instance, attaches it to `ABrushCollection`, and sets [[Visible]] to `True`.
