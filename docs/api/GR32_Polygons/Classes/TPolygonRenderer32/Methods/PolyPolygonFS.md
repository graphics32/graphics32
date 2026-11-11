---
layout: doc
docType: api
unit: GR32_Polygons
parent: TPolygonRenderer32
entity: TPolygonRenderer32.PolyPolygonFS
kind: Method
declaration: "procedure PolyPolygonFS(const Points: TArrayOfArrayOfFloatPoint); overload; virtual;"
summary: "Draws multi-contour floating-point polygons using the destination bitmap's clip rectangle."
parameters:
  - name: Points
    type: TArrayOfArrayOfFloatPoint
    description: "Array of polygon contours."
---

## Description

`PolyPolygonFS` rasterizes multi-contour polygon paths onto `Bitmap` using `Bitmap.ClipRect`.
