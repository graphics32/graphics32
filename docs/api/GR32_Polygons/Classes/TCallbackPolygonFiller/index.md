---
layout: doc
docType: api
unit: GR32_Polygons
entity: TCallbackPolygonFiller
kind: Class
declaration: "TCallbackPolygonFiller = class(TCustomPolygonFiller)"
inheritance:
  - TCustomPolygonFiller
  - TObject
summary: "Polygon filler delegating line rendering to a custom event callback."
---

## Description

`TCallbackPolygonFiller` allows applications to specify a custom method callback (`FillLineEvent`) for painting polygon scanline spans.

---

[members]
