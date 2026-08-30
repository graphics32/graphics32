---
layout: doc
docType: api
unit: GR32_VectorMaps
parent: TVectorMap
entity: TVectorMap.FixedVector
kind: Property
aliases: [FixedVectorS, FixedVectorX, FixedVectorXS]
declaration: |
  property FixedVector[X, Y: Integer]: TFixedVector read GetFixedVector write SetFixedVector; default;
  property FixedVectorS[X, Y: Integer]: TFixedVector read GetFixedVectorS write SetFixedVectorS;
  property FixedVectorX[X, Y: TFixed]: TFixedVector read GetFixedVectorX write SetFixedVectorX;
  property FixedVectorXS[X, Y: TFixed]: TFixedVector read GetFixedVectorXS write SetFixedVectorXS;
summary: "Indexed 16.16 fixed-point displacement vector properties with integer, bounds-checked, sub-pixel, and sub-pixel bounds-checked variants."
---

## Description

Indexed accessor properties for reading and writing 16.16 fixed-point displacement vectors (`TFixedVector`):

- `FixedVector[X, Y: Integer]`: Default integer coordinate indexer. Fast direct access without bounds checking.
- `FixedVectorS[X, Y: Integer]`: Safe integer coordinate indexer with bounds checking (returns `(0, 0)` when out of bounds).
- `FixedVectorX[X, Y: TFixed]`: Sub-pixel fixed-point coordinate indexer using bilinear vector interpolation.
- `FixedVectorXS[X, Y: TFixed]`: Safe sub-pixel fixed-point coordinate indexer using bilinear vector interpolation with bounds checking.
