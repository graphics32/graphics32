---
layout: doc
docType: api
unit: GR32_Polygons
entity: TPolygonRenderer32VPR
kind: Class
declaration: "TPolygonRenderer32VPR = class(TPolygonRenderer32)"
inheritance:
  - TPolygonRenderer32
  - TCustomPolygonRenderer
  - TThreadPersistent
  - TNotifiablePersistent
  - TPlainInterfacedPersistent
  - TPersistent
  - TObject
summary: "High-performance vector polygon rasterizer using coverage-based antialiasing (VPR)."
---

## Description

`TPolygonRenderer32VPR` is the standard antialiased polygon renderer in Graphics32. It calculates exact analytical sub-pixel area coverage for vector polygon edges and generates smooth antialiased scanlines.

---

[members]
