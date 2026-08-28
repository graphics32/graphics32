---
layout: doc
docType: api
unit: GR32_Polygons
parent: TPolygonRenderer32VPR
entity: TPolygonRenderer32VPR.PolyPolygonFS
kind: Method
declaration: "procedure PolyPolygonFS(const Points: TArrayOfArrayOfFloatPoint; const ClipRect: TFloatRect); override;"
summary: "Rasterizes multi-contour floating-point polygons within a clipping rectangle using VPR analytical coverage."
parameters:
  - name: Points
    type: TArrayOfArrayOfFloatPoint
    description: "Array of polygon contours."
  - name: ClipRect
    type: TFloatRect
    description: "Clipping rectangle."
---

## Description

`PolyPolygonFS` overrides the base renderer method to perform analytical sub-pixel coverage computation and rasterization on the target bitmap.
