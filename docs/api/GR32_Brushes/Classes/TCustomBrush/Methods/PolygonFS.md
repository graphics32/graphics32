---
layout: doc
docType: api
unit: GR32_Brushes
parent: TCustomBrush
entity: TCustomBrush.PolygonFS
kind: Method
declaration: "procedure PolygonFS(Renderer: TCustomPolygonRenderer; const Points: TArrayOfFloatPoint; const ClipRect: TFloatRect; Transformation: TTransformation; Closed: Boolean); virtual;"
summary: "Renders a single vector polygon path using the specified polygon renderer."
parameters:
  - name: Renderer
    type: TCustomPolygonRenderer
    description: "Polygon renderer destination."
  - name: Points
    type: TArrayOfFloatPoint
    description: "Array of floating-point vertices defining the polygon path."
  - name: ClipRect
    type: TFloatRect
    description: "Clipping rectangle."
  - name: Transformation
    type: TTransformation
    description: "Optional spatial transformation applied to coordinates."
  - name: Closed
    type: Boolean
    description: "Indicates whether the path is closed or open."
---

## Description

`PolygonFS` processes and renders a single vector polygon path defined by `Points`. It wraps `Points` into a single-path array and delegates processing to [[PolyPolygonFS]].
