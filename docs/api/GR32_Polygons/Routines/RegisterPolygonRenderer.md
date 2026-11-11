---
layout: doc
docType: api
unit: GR32_Polygons
entity: RegisterPolygonRenderer
kind: Function
declaration: "procedure RegisterPolygonRenderer(PolygonRendererClass: TCustomPolygonRendererClass);"
summary: "Registers a custom polygon renderer class globally in CustomPolygonRendererList and PolygonRendererList."
parameters:
  - name: PolygonRendererClass
    type: TCustomPolygonRendererClass
    description: "Class reference of the polygon renderer to register."
---

## Description

`RegisterPolygonRenderer` adds `PolygonRendererClass` to `CustomPolygonRendererList`, and if it inherits from `TPolygonRenderer32`, to `PolygonRendererList`.
