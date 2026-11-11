---
layout: doc
docType: api
unit: GR32_Brushes
entity: TSolidBrush
kind: Class
declaration: "TSolidBrush = class(TCustomBrush)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TCustomBrush
  - TSolidBrush
summary: "Brush that configures fill color, fill mode, or polygon fillers on the destination polygon renderer."
---

## Description

`TSolidBrush` configures the destination [[TCustomPolygonRenderer]] (typically [[TPolygonRenderer32]]) with a specified 32-bit ARGB fill color ([[FillColor]]), polygon fill rule ([[FillMode]]), or custom gradient/pattern filler ([[Filler]]).

[members]
