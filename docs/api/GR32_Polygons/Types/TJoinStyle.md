---
layout: doc
docType: api
unit: GR32_Polygons
entity: TJoinStyle
kind: Type
declaration: "TJoinStyle = (jsMiter, jsBevel, jsRound, jsRoundEx, jsSquare);"
summary: "Enumeration specifying the style used to join adjacent line segments in polyline stroking."
---

## Description

`TJoinStyle` defines how corner vertices between adjacent polyline segments are joined during stroke outline generation in routines such as `BuildPolyPolyLine`, `PolylineFS`, and `PolyPolylineFS`.

### Enum Values

| Value | Description |
| --- | --- |
| `jsMiter` | Outer edges are extended to intersect at a sharp point. If the angle of the corner is very sharp and exceeds `MiterLimit`, the join falls back to a bevel join. |
| `jsBevel` | Corners are cut flat perpendicular to the vertex bisector. |
| `jsRound` | Convex joins are rounded using circular arc approximation. |
| `jsRoundEx` | Both convex and concave joins are rounded. Convex join rounding depth is controlled by `MiterLimit`. |
| `jsSquare` | Corners are extended outwards by half the stroke width with flat square caps. |
