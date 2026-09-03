---
layout: doc
docType: api
unit: GR32_Brushes
entity: TCustomBrush
kind: Class
abstract: true
aliases: [TBrushClass]
declaration: |
  type
    TCustomBrush = class(TNotifiablePersistent)
      ...
    TBrushClass = class of TCustomBrush;
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TCustomBrush
summary: "Base class for vector path and polygon brushes."
---

## Description

`TCustomBrush` defines the abstract pipeline interface for vector brushes in Graphics32.<br>
Brushes receive floating-point vector paths (`TArrayOfFloatPoint` or `TArrayOfArrayOfFloatPoint`), transform or process polygon geometry (such as generating outline strokes or expanding/shrinking boundaries), and dispatch rendering calls to an assigned [[TCustomPolygonRenderer|polygon renderer]].

Derived classes implement specific vector effects:
- [[TSolidBrush]]: Applies fill colors, fill modes, or custom fillers.
- [[TNestedBrush]]: Chains multiple child brushes in sequence.
- [[TStrokeBrush]]: Generates stroked outlines.
- [[TGrowBrush]]: Inflates or deflates shapes.
- [[TDashedBrush]]: Generates dashed stroke patterns.

[members]
