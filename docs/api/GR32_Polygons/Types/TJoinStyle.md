---
layout: doc
docType: api
unit: GR32_Polygons
entity: TJoinStyle
aliases: [TJoinStyles]
kind: Type
declaration: |
  TJoinStyle = (jsMiter, jsBevel, jsRound, jsRoundEx, jsSquare);
  TJoinStyles = set of TJoinStyle;
summary: "Enumeration specifying the style used to join adjacent line segments in polyline stroking."
---

## Description

`TJoinStyle` defines how corner vertices between adjacent polyline segments are joined during stroke outline generation in routines such as [[BuildPolyPolyLine]], [[PolylineFS]], and [[PolyPolylineFS]].

### Enum Values

| Value | Description | Example |
| --- | --- | --- |
| `jsMiter` | Outer edges are extended to intersect at a sharp point. If the angle of the corner is very sharp and exceeds `MiterLimit`, the join falls back to a bevel join. | ![](/images/JoinStyle-EndStyle-jsMiter-esRound.png) |
| `jsBevel` | Corners are cut flat perpendicular to the vertex bisector. | ![](/images/JoinStyle-EndStyle-jsBevel-esRound.png) |
| `jsRound` | Convex joins are rounded using circular arc approximation. | ![](/images/JoinStyle-EndStyle-jsRound-esRound.png) |
| `jsRoundEx` | Both convex and concave joins are rounded. Convex join rounding depth is controlled by `MiterLimit`. | ![](/images/JoinStyle-EndStyle-jsRoundEx-esRound.png) |
| `jsSquare` | Corners are extended outwards by half the stroke width with flat square caps. | ![](/images/JoinStyle-EndStyle-jsSquare-esRound.png) |
