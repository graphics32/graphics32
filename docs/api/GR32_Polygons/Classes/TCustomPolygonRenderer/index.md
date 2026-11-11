---
layout: doc
docType: api
unit: GR32_Polygons
entity: TCustomPolygonRenderer
kind: Class
abstract: true
aliases: [TCustomPolygonRendererClass]
declaration: |
  type
    TCustomPolygonRenderer = class abstract(TThreadPersistent)
      ...
    TCustomPolygonRendererClass = class of TCustomPolygonRenderer;
inheritance:
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TThreadPersistent
  - TCustomPolygonRenderer
summary: "Abstract base class for all vector polygon renderers."
---

## Description

`TCustomPolygonRenderer` defines the base interface for vector polygon rasterizers in Graphics32. Descendants implement concrete antialiased rasterization and span filling for floating-point polygon shapes ([[TArrayOfFloatPoint]] and [[TArrayOfArrayOfFloatPoint]]).

---

[members]
