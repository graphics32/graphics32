---
layout: doc
docType: api
unit: GR32_Polygons
entity: GR32_Polygons
kind: Unit
summary: "Contains classes and algorithms for sub-pixel vector polygon rendering, path stroking, filling, and antialiased rasterization."
---

# Unit GR32_Polygons

The `GR32_Polygons` unit contains classes and algorithms for sub-pixel vector polygon rendering, path stroking, filling, and antialiased rasterization.

---

## Classes

| Class | Description |
|---|---|
| `TPolygon32` | Legacy vector polygon class supporting fill and outline rendering. |
| `TPolyPolygon32` | Container for multi-contour polygon shapes (e.g. polygons with holes). |
| `TCustomPolygonFiller` | Abstract base class for custom span fillers (e.g. gradients, texture patterns). |
