---
layout: doc
docType: api
unit: GR32_VectorMaps
parent: TVectorMap
entity: TVectorMap.GetTrimmedBounds
kind: Method
declaration: "function GetTrimmedBounds: TRect;"
summary: "Calculates the minimal bounding rectangle containing all non-zero displacement vectors."
---

## Description

`GetTrimmedBounds` scans the vector map and returns the tightest `TRect` bounding box that encompasses all non-zero displacement vectors. If all displacement vectors are zero or the map is empty, an empty rectangle `(0, 0, 0, 0)` is returned.
