---
layout: doc
docType: api
unit: GR32_Brushes
parent: TNestedBrush
entity: TNestedBrush.PolyPolygonMixedFS
kind: Method
declaration: "procedure PolyPolygonMixedFS(Renderer: TCustomPolygonRenderer; const Points: TArrayOfArrayOfFloatPoint; const ClipRect: TFloatRect; Transformation: TTransformation; Closed: TBooleanArray); override;"
summary: "Renders mixed open/closed paths across all visible child brushes in the collection."
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
    type: TBooleanArray
    description: "Array of boolean flags specifying open or closed state per sub-path."
---

## Description

`PolyPolygonMixedFS` overrides `TCustomBrush.PolyPolygonMixedFS` to iterate through all visible child brushes in [[Brushes]] and invoke `PolyPolygonMixedFS` on each child brush sequentially.
