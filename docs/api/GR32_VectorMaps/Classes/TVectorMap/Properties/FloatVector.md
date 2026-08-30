---
layout: doc
docType: api
unit: GR32_VectorMaps
parent: TVectorMap
entity: TVectorMap.FloatVector
kind: Property
aliases: [FloatVectorS, FloatVectorF, FloatVectorFS]
declaration: |
  property FloatVector[X, Y: Integer]: TFloatVector read GetFloatVector write SetFloatVector;
  property FloatVectorS[X, Y: Integer]: TFloatVector read GetFloatVectorS write SetFloatVectorS;
  property FloatVectorF[X, Y: Single]: TFloatVector read GetFloatVectorF write SetFloatVectorF;
  property FloatVectorFS[X, Y: Single]: TFloatVector read GetFloatVectorFS write SetFloatVectorFS;
summary: "Indexed floating-point displacement vector properties with integer, bounds-checked, float, and float bounds-checked variants."
---

## Description

Indexed accessor properties for reading and writing floating-point displacement vectors (`TFloatVector`):

- `FloatVector[X, Y: Integer]`: Integer grid coordinate indexer returning floating-point vectors.
- `FloatVectorS[X, Y: Integer]`: Safe integer grid coordinate indexer with bounds checking (returns `(0, 0)` when out of bounds).
- `FloatVectorF[X, Y: Single]`: Floating-point coordinate indexer using bilinear vector interpolation.
- `FloatVectorFS[X, Y: Single]`: Safe floating-point coordinate indexer using bilinear vector interpolation with bounds checking.
