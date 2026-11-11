---
layout: doc
docType: api
unit: GR32_Brushes
parent: TCustomBrush
entity: TCustomBrush.PolyPolygonMixedFS
kind: Method
declaration: "procedure PolyPolygonMixedFS(Renderer: TCustomPolygonRenderer; const Points: TArrayOfArrayOfFloatPoint; const ClipRect: TFloatRect; Transformation: TTransformation; Closed: TBooleanArray); virtual;"
summary: "Processes and renders multiple vector paths with individual open or closed states."
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
    type: TBooleanArray
    description: "Array of boolean flags specifying the open or closed state per sub-path."
---

## Description

`PolyPolygonMixedFS` groups contiguous sub-paths in `Points` that share identical open/closed boolean states in `Closed`, processes each group, and renders the consolidated geometry.
