---
layout: doc
docType: api
unit: GR32_Polygons
entity: DefaultPolygonRendererClass
kind: Variable
declaration: "var DefaultPolygonRendererClass: TPolygonRenderer32Class = TPolygonRenderer32VPR;"
summary: "Default renderer class reference used when rendering polygons without an explicit renderer instance."
---

## Description

`DefaultPolygonRendererClass` specifies the default renderer class instantiated by wrapper routines (such as `PolygonFS` and `PolyPolygonFS`) when no renderer instance is supplied. Defaults to `TPolygonRenderer32VPR`.
