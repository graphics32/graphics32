---
layout: doc
docType: api
unit: GR32_Transforms
entity: Quadrilateral Types
kind: Type
aliases: [TQuadrilateral, TFloatQuadrilateral]
summary: "Defines 4-point convex quadrilateral structures for projective warping."
---

## Description

Quadrilateral types define 4-corner polygon regions used by projective transformations (such as [[TProjectiveTransformationEx]]) to map between arbitrary source and destination quadrilaterals.

- `TQuadrilateral`: 4-element array of integer points (`TPoint`).
- `TFloatQuadrilateral`: 4-element array of floating-point points (`TFloatPoint`).

## Types

| Identifier | Type / Declaration | Description |
| --- | --- | --- |
| `TQuadrilateral` | `array [0..3] of TPoint;` | Array of 4 integer coordinates specifying a quadrilateral. |
| `TFloatQuadrilateral` | `array [0..3] of TFloatPoint;` | Array of 4 floating-point coordinates specifying a quadrilateral. |
