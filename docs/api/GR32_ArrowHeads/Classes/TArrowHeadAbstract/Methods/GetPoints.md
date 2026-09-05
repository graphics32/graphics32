---
layout: doc
docType: api
unit: GR32_ArrowHeads
parent: TArrowHeadAbstract
entity: TArrowHeadAbstract.GetPoints
kind: Method
scope: Public
declaration: "function GetPoints(const Line: TArrayOfFloatPoint; AtEnd: Boolean): TArrayOfFloatPoint;"
summary: "Calculates the polygon vertices defining the arrowhead for a given polyline segment."
parameters:
  - name: Line
    type: TArrayOfFloatPoint
    description: "Array of points defining the polyline path. Must contain at least two points."
  - name: AtEnd
    type: Boolean
    description: "True to attach the arrowhead at the end point of the polyline; False to attach at the starting point."
returns:
  - type: TArrayOfFloatPoint
    description: "An array of 2D floating-point coordinates outlining the arrowhead shape."
seealso:
  - Size
---

## Description

`GetPoints` computes the vector direction of the line segment at either the start (`AtEnd = False`) or end (`AtEnd = True`) of `Line`, calculates the tip and base coordinates based on [[Size]], and returns the full polygon contour for the arrowhead by invoking `GetPointsInternal`.

An exception is raised if `Line` contains fewer than two points.
