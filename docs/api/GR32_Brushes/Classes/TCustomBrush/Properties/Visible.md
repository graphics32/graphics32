---
layout: doc
docType: api
unit: GR32_Brushes
parent: TCustomBrush
entity: TCustomBrush.Visible
kind: Property
declaration: "property Visible: Boolean read FVisible write SetVisible;"
summary: "Determines whether this brush participates in rendering pipeline operations."
---

## Description

`Visible` controls whether this brush is executed during polygon rendering. When `False`, rendering loops skip this brush. Default value is `True`.
