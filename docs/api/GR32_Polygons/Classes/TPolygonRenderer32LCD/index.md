---
layout: doc
docType: api
unit: GR32_Polygons
entity: TPolygonRenderer32LCD
kind: Class
declaration: "TPolygonRenderer32LCD = class(TPolygonRenderer32VPR)"
inheritance:
  - TPolygonRenderer32VPR
  - TPolygonRenderer32
  - TCustomPolygonRenderer
  - TThreadPersistent
  - TNotifiablePersistent
  - TPlainInterfacedPersistent
  - TPersistent
  - TObject
summary: "Polygon renderer providing sub-pixel antialiasing tuned for LCD monitor sub-pixel layouts."
---

## Description

`TPolygonRenderer32LCD` provides horizontal sub-pixel antialiasing by scaling horizontal coordinates 3x and mapping coverage values directly across red, green, and blue sub-pixel channels of LCD displays.

---

[members]