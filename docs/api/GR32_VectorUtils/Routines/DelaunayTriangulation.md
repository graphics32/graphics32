---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: DelaunayTriangulation
kind: Function
declaration: "function DelaunayTriangulation(Points: TArrayOfFloatPoint): TArrayOfTriangleVertexIndices;"
summary: "Generates a Delaunay triangulation mesh from a set of 2D floating-point points."
parameters:
  - name: Points
    type: TArrayOfFloatPoint
    description: "Array of 2D input points."
---

## Description

`DelaunayTriangulation` computes a Delaunay triangulation mesh for 2D point sets, returning an array of triangle index triplets (`TArrayOfTriangleVertexIndices`).
