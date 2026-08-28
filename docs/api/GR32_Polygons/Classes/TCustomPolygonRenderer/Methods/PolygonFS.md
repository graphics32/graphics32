---
layout: doc
docType: api
unit: GR32_Polygons
parent: TCustomPolygonRenderer
entity: TCustomPolygonRenderer.PolygonFS
kind: Method
summary: "Rasterizes a single floating-point polygon shape."
overloads:
  - signature: "procedure PolygonFS(const Points: TArrayOfFloatPoint; const ClipRect: TFloatRect); overload; virtual;"
    summary: "Rasterizes a single floating-point polygon shape clipped by ClipRect."
    parameters:
      - name: Points
        type: TArrayOfFloatPoint
        description: "Array of floating-point polygon vertices."
      - name: ClipRect
        type: TFloatRect
        description: "Bounding clipping rectangle."

  - signature: "procedure PolygonFS(const Points: TArrayOfFloatPoint; const ClipRect: TFloatRect; Transformation: TTransformation); overload; virtual;"
    summary: "Rasterizes a single floating-point polygon shape transformed by a TTransformation matrix."
    parameters:
      - name: Points
        type: TArrayOfFloatPoint
        description: "Array of floating-point polygon vertices."
      - name: ClipRect
        type: TFloatRect
        description: "Bounding clipping rectangle."
      - name: Transformation
        type: TTransformation
        description: "Optional geometric transformation applied to polygon points."
---

## Description

`PolygonFS` converts a single floating-point polygon into a multi-contour polygon array and delegates rasterization to `PolyPolygonFS`.
