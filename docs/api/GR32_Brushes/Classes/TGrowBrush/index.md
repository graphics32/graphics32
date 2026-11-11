---
layout: doc
docType: api
unit: GR32_Brushes
entity: TGrowBrush
kind: Class
declaration: "TGrowBrush = class(TNestedBrush)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TCustomBrush
  - TSolidBrush
  - TNestedBrush
  - TGrowBrush
summary: "Brush that inflates or deflates polygon boundaries by a floating-point grow amount."
---

## Description

`TGrowBrush` inflates (expands) or deflates (shrinks) input vector polygon boundaries by [[GrowAmount]] using normal offsets (`Grow`).

The expanded/contracted geometry is then passed to internal child brushes in [[Brushes]] for rendering.

[members]
