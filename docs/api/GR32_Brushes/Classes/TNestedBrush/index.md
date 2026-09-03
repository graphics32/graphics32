---
layout: doc
docType: api
unit: GR32_Brushes
entity: TNestedBrush
kind: Class
declaration: "TNestedBrush = class(TSolidBrush)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TCustomBrush
  - TSolidBrush
  - TNestedBrush
summary: "Composite brush containing an internal brush collection executed sequentially on input geometries."
---

## Description

`TNestedBrush` implements a composite brush pattern. It owns an internal [[TBrushCollection]] ([[Brushes]]) and iterates through all visible child brushes in sequence, rendering each brush on the same input polygon geometry.

This allows combining complex multi-pass brush effects (e.g. stroke + fill + shadow) inside a single nested brush entity.

[members]
