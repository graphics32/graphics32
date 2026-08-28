---
layout: doc
docType: api
unit: GR32_Polygons
entity: TPolyFillMode
kind: Type
declaration: "TPolyFillMode = (pfAlternate, pfWinding, pfEvenOdd = 0, pfNonZero);"
summary: "Enumeration specifying polygon filling rules for intersecting or self-overlapping paths."
---

## Description

`TPolyFillMode` determines how coverage and winding numbers are evaluated to decide whether a point falls inside a complex multi-contour or self-intersecting polygon.

### Enum Values

| Value | Description |
| --- | --- |
| `pfAlternate` | Alternate / Even-Odd fill rule. A point is inside the polygon if a ray drawn from it to infinity crosses an odd number of path edges. Equivalent to `pfEvenOdd`. |
| `pfWinding` | Non-Zero Winding fill rule. Evaluates the direction of contour edges crossing a ray to compute a net winding number; non-zero counts are considered inside. Equivalent to `pfNonZero`. |
| `pfEvenOdd` | Alias for `pfAlternate` (value `0`). |
| `pfNonZero` | Alias for `pfWinding` (value `1`). |
