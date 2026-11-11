---
layout: doc
docType: api
unit: GR32_Brushes
parent: TCustomBrush
entity: TCustomBrush.PolyPolygonFS
kind: Method
declaration: "procedure PolyPolygonFS(Renderer: TCustomPolygonRenderer; const Points: TArrayOfArrayOfFloatPoint; const ClipRect: TFloatRect; Transformation: TTransformation; Closed: Boolean); virtual;"
summary: "Processes and renders multiple vector paths sharing the same open or closed state."
parameters:
  - name: Renderer
    type: TCustomPolygonRenderer
    description: "Polygon renderer destination."
  - name: Points
    type: TArrayOfArrayOfFloatPoint
    description: "Array of sub-paths defining the complex polygon geometry."
  - name: ClipRect
    type: TFloatRect
    description: "Clipping rectangle."
  - name: Transformation
    type: TTransformation
    description: "Optional spatial transformation applied to coordinates."
  - name: Closed
    type: Boolean
    description: "Indicates whether all sub-paths are closed or open."
---

## Description

`PolyPolygonFS` processes multiple floating-point sub-paths `Points` through internal path transformation steps (`ProcessPolyPolygon`) and dispatches the resulting geometry to `Renderer`.
