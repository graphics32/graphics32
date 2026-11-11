---
layout: doc
docType: api
unit: GR32_Polygons
parent: TPolygonRenderer32
entity: TPolygonRenderer32.PolygonFS
kind: Method
declaration: "procedure PolygonFS(const Points: TArrayOfFloatPoint); overload; virtual;"
summary: "Draws a single floating-point polygon using the destination bitmap's clip rectangle."
parameters:
  - name: Points
    type: TArrayOfFloatPoint
    description: "Array of floating-point polygon vertices."
---

## Description

`PolygonFS` rasterizes a single polygon specified by `Points` onto `Bitmap` using `Bitmap.ClipRect`.
