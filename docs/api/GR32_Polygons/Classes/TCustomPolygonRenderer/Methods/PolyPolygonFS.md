---
layout: doc
docType: api
unit: GR32_Polygons
parent: TCustomPolygonRenderer
entity: TCustomPolygonRenderer.PolyPolygonFS
kind: Method
summary: "Rasterizes multi-contour floating-point polygon shapes onto the rendering target."
overloads:
  - signature: "procedure PolyPolygonFS(const Points: TArrayOfArrayOfFloatPoint; const ClipRect: TFloatRect); overload; virtual; abstract;"
    summary: "Abstract method to rasterize multi-contour floating-point polygons within a clipping rectangle."
    parameters:
      - name: Points
        type: TArrayOfArrayOfFloatPoint
        description: "Array of polygon contours, where each contour is an array of floating-point vertices."
      - name: ClipRect
        type: TFloatRect
        description: "Bounding clipping rectangle."

  - signature: "procedure PolyPolygonFS(const Points: TArrayOfArrayOfFloatPoint; const ClipRect: TFloatRect; Transformation: TTransformation); overload; virtual;"
    summary: "Rasterizes multi-contour floating-point polygons transformed by a TTransformation matrix."
    parameters:
      - name: Points
        type: TArrayOfArrayOfFloatPoint
        description: "Array of polygon contours to transform and draw."
      - name: ClipRect
        type: TFloatRect
        description: "Bounding clipping rectangle."
      - name: Transformation
        type: TTransformation
        description: "Optional geometric transformation applied to polygon points."
---

## Description

`PolyPolygonFS` rasterizes multi-contour floating-point polygon paths (such as polygons with holes or complex compound paths).
