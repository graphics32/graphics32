---
layout: doc
docType: api
unit: GR32_Polygons
entity: UnregisterPolygonRenderer
kind: Function
declaration: "procedure UnregisterPolygonRenderer(PolygonRendererClass: TCustomPolygonRendererClass);"
summary: "Unregisters a custom polygon renderer class globally from renderer lists."
parameters:
  - name: PolygonRendererClass
    type: TCustomPolygonRendererClass
    description: "Class reference of the polygon renderer to unregister."
---

## Description

`UnregisterPolygonRenderer` removes `PolygonRendererClass` from `CustomPolygonRendererList` and `PolygonRendererList`.
