---
layout: doc
docType: api
unit: GR32_Brushes
parent: TNestedBrush
entity: TNestedBrush.PolyPolygonFS
kind: Method
declaration: "procedure PolyPolygonFS(Renderer: TCustomPolygonRenderer; const Points: TArrayOfArrayOfFloatPoint; const ClipRect: TFloatRect; Transformation: TTransformation; Closed: Boolean); override;"
summary: "Renders multiple polygon paths across all visible child brushes in the collection."
parameters:
  - name: Renderer
    type: TCustomPolygonRenderer
    description: "Polygon renderer destination."
  - name: Points
    type: TArrayOfArrayOfFloatPoint
    description: "Array of sub-paths."
  - name: ClipRect
    type: TFloatRect
    description: "Clipping rectangle."
  - name: Transformation
    type: TTransformation
    description: "Optional spatial transformation."
  - name: Closed
    type: Boolean
    description: "Indicates whether all paths are closed or open."
---

## Description

`PolyPolygonFS` overrides `TCustomBrush.PolyPolygonFS` to iterate through all visible child brushes in [[Brushes]] and invoke `PolyPolygonFS` on each child brush sequentially.
