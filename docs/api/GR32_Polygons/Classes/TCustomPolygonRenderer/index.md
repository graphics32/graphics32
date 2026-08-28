---
layout: doc
docType: api
unit: GR32_Polygons
entity: TCustomPolygonRenderer
kind: Class
aliases: [TCustomPolygonRendererClass]
declaration: "TCustomPolygonRenderer = class abstract(TThreadPersistent)"
inheritance:
  - TThreadPersistent
  - TNotifiablePersistent
  - TPlainInterfacedPersistent
  - TPersistent
  - TObject
summary: "Abstract base class for all vector polygon renderers."
---

## Description

`TCustomPolygonRenderer` defines the base interface for vector polygon rasterizers in Graphics32. Descendants implement concrete antialiased rasterization and span filling for floating-point polygon shapes ([[TArrayOfFloatPoint]] and [[TArrayOfArrayOfFloatPoint]]).

---

[members]
