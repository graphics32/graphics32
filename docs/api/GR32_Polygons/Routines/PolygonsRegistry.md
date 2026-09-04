---
layout: doc
docType: api
unit: GR32_Polygons
entity: PolygonsRegistry
kind: Function
declaration: "function PolygonsRegistry: TFunctionRegistry;"
summary: "Returns the global TFunctionRegistry instance managing GR32_Polygons function bindings."
returns:
  - type: TFunctionRegistry
    description: "The global [[TFunctionRegistry]] instance managing function implementations and CPU optimization bindings for polygon rendering routines."
---

## Description

`PolygonsRegistry` returns the `TFunctionRegistry` managing CPU-specific (Pascal, SSE2, SSE4.1) bindings for polygon coverage functions.
