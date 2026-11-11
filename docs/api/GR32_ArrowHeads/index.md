---
layout: doc
docType: api
unit: GR32_ArrowHeads
entity: GR32_ArrowHeads
kind: Unit
summary: "Provides vector arrowhead generation classes for decorating polyline start and end points."
seealso:
  - "**[ArrowHead example](https://github.com/graphics32/graphics32/tree/[branch]/Examples/Drawing/ArrowHead)** - Example application demonstrating construction and rendering of beziers and arrow heads."
  - "**[[GR32_Polygons]]** - Provides polygon rasterization."
  - "**[[GR32_VectorUtils]]** - Provides polygon transformation and construction routines."
  - "**[[GR32_Paths]]** - Provides vector path creation and rendering classes."
---

## Description

The `GR32_ArrowHeads` unit provides a set of classes for generating 2D vector arrowhead shapes along lines and polylines. These shapes can be rendered onto a [[TCustomBitmap32]] surface using polygon filling or stroking functions.

Key components in `GR32_ArrowHeads` include:

| Kind | Example | Class | Description |
| --- | --- | --- | --- |
| **3-Point** | ![](/images/arrowhead-3point.png) | [[TArrowHeadSimple]] | Generates a standard 3-point triangular arrowhead. |
| **4-Point** | ![](/images/arrowhead-4point.png) | [[TArrowHeadFourPt]] | Generates a 4-point dart/barbed arrowhead. |
| **Circle** | ![](/images/arrowhead-circle.png) | [[TArrowHeadCircle]] | Generates a circular endpoint decoration. |
| **Diamond** | ![](/images/arrowhead-diamond.png) | [[TArrowHeadDiamond]] | Generates a 4-point diamond endpoint decoration. |

[members]
