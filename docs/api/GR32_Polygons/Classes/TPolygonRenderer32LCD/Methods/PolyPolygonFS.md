---
layout: doc
docType: api
unit: GR32_Polygons
parent: TPolygonRenderer32LCD
entity: TPolygonRenderer32LCD.PolyPolygonFS
kind: Method
declaration: "procedure PolyPolygonFS(const Points: TArrayOfArrayOfFloatPoint; const ClipRect: TFloatRect); override;"
summary: "Rasterizes multi-contour floating-point polygons using 3x horizontal sub-pixel LCD antialiasing."
parameters:
  - name: Points
    type: TArrayOfArrayOfFloatPoint
    description: "Array of polygon contours."
  - name: ClipRect
    type: TFloatRect
    description: "Clipping rectangle."
---

## Description

`PolyPolygonFS` scales polygon coordinates 3x horizontally and performs sub-pixel LCD filtering.
