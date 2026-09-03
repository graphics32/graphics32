---
layout: doc
docType: api
unit: GR32_Brushes
entity: TStrokeBrush
kind: Class
declaration: "TStrokeBrush = class(TSolidBrush)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TCustomBrush
  - TSolidBrush
  - TStrokeBrush
summary: "Brush that expands vector paths into stroked outlines using specified line width, join style, end style, and miter limit."
---

## Description

`TStrokeBrush` converts input vector paths into stroked outlines using `BuildPolyPolyLine`.

It supports configurable stroke parameters:
- [[StrokeWidth]]: Floating-point line stroke thickness.
- [[JoinStyle]]: Joint corner style (`jsMiter`, `jsBevel`, `jsRound`).
- [[EndStyle]]: Line cap style (`esButt`, `esSquare`, `esRound`).
- [[MiterLimit]]: Maximum miter extension ratio for sharp corners.

[members]
