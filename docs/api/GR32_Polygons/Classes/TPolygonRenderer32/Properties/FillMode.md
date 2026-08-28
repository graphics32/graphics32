---
layout: doc
docType: api
unit: GR32_Polygons
parent: TPolygonRenderer32
entity: TPolygonRenderer32.FillMode
kind: Property
declaration: "property FillMode: TPolyFillMode read FFillMode write SetFillMode;"
summary: "Polygon filling rule (pfAlternate / pfEvenOdd or pfWinding / pfNonZero)."
---

## Description

`FillMode` controls how overlapping polygon contours and self-intersecting boundaries are evaluated during scanline coverage calculation.
