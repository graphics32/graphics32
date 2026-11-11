---
layout: doc
docType: api
unit: GR32_Brushes
parent: TNestedBrush
entity: TNestedBrush.PolygonFS
kind: Method
declaration: "procedure PolygonFS(Renderer: TCustomPolygonRenderer; const Points: TArrayOfFloatPoint; const ClipRect: TFloatRect; Transformation: TTransformation; Closed: Boolean); override;"
summary: "Renders a single polygon path across all visible child brushes in the collection."
parameters:
  - name: Renderer
    type: TCustomPolygonRenderer
    description: "Polygon renderer destination."
  - name: Points
    type: TArrayOfFloatPoint
    description: "Polygon path coordinates."
  - name: ClipRect
    type: TFloatRect
    description: "Clipping rectangle."
  - name: Transformation
    type: TTransformation
    description: "Optional spatial transformation."
  - name: Closed
    type: Boolean
    description: "Indicates whether the path is closed or open."
---

## Description

`PolygonFS` overrides `TCustomBrush.PolygonFS` to iterate through all visible child brushes in [[Brushes]] and invoke `PolygonFS` on each child brush sequentially.
