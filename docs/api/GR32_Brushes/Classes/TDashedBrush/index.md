---
layout: doc
docType: api
unit: GR32_Brushes
entity: TDashedBrush
kind: Class
declaration: "TDashedBrush = class(TStrokeBrush)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TCustomBrush
  - TSolidBrush
  - TStrokeBrush
  - TDashedBrush
summary: "Stroke brush that splits input vector paths into dashed line segments using custom dash arrays and offsets."
---

## Description

`TDashedBrush` extends [[TStrokeBrush]] to render dashed line strokes. It splits input polygon paths into dashed line segments (`BuildDashedLine`) using a floating-point pattern array ([[DashArray]]) and starting offset ([[DashOffset]]) before stroking each segment.

[members]
