---
layout: doc
docType: api
unit: GR32_Brushes
entity: GR32_Brushes
kind: Unit
summary: "Provides vector polygon brush effects including solid filling, stroking, growing, dashing, and nested brush pipelines."
---

## Description

The `GR32_Brushes` unit provides a modular vector brush architecture for rendering vector paths and polygons in Graphics32.

Brushes encapsulate polygon drawing behaviors (such as outline stroking, polygon expansion/shrinking, dashed pattern generation, and solid color/filler assignment) and can be chained together in brush collections or composite nested brushes.

Key classes and types in `GR32_Brushes` include:

- **Brush Collection**: [[TBrushCollection]] manages an ordered list of brush instances assigned to an owner object.
- **Base Class**: [[TCustomBrush]] defines the fundamental interface and pipeline methods for vector path processing and rendering (`PolygonFS`, `PolyPolygonFS`, `PolyPolygonMixedFS`).
- **Solid Brush**: [[TSolidBrush]] applies fill color, fill mode, or custom polygon fillers ([[TCustomPolygonFiller]]) to vector shapes.
- **Nested Brush**: [[TNestedBrush]] contains its own child [[TBrushCollection]], executing multiple brush effects sequentially on the same geometry.
- **Stroke Brush**: [[TStrokeBrush]] converts closed or open vector paths into stroked outlines using specified line widths, join styles, end styles, and miter limits.
- **Grow Brush**: [[TGrowBrush]] inflates or deflates polygon boundaries by a floating-point grow amount.
- **Dashed Brush**: [[TDashedBrush]] splits vector outlines into dashed line segments using custom dash pattern arrays and offsets before stroking.

[members]
