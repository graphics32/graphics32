---
layout: doc
docType: api
unit: GR32_Brushes
entity: TBooleanArray
kind: Type
declaration: "type TBooleanArray = array of boolean;"
summary: "Dynamic array type holding boolean flags indicating open or closed state for individual sub-polygons."
---

## Description

`TBooleanArray` is a dynamic array of boolean values used in mixed polygon rendering operations (`PolyPolygonMixedFS`).

Each boolean element in the array specifies whether the corresponding sub-polygon in an array of paths (`TArrayOfArrayOfFloatPoint`) is closed (`True`) or open (`False`).
