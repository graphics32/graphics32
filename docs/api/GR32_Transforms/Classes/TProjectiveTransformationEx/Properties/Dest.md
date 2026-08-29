---
layout: doc
docType: api
unit: GR32_Transforms
parent: TProjectiveTransformationEx
entity: TProjectiveTransformationEx.Dest
kind: Property
aliases: [DestQuad, DestX, DestY, DestX0, DestX1, DestX2, DestX3, DestY0, DestY1, DestY2, DestY3]
declaration: |
  property DestQuad: TFloatQuadrilateral read FDestQuad write SetDestQuad;
  property Dest[Index: Integer]: TFloatPoint read GetDest write SetDest;
  property DestX[Index: Integer]: TFloat read GetDestX write SetDestX;
  property DestY[Index: Integer]: TFloat read GetDestX write SetDestY;
summary: "Destination quadrilateral and indexed vertex coordinate properties."
---

## Description

Properties defining destination quadrilateral coordinates for [[TProjectiveTransformationEx]]:
- `DestQuad`: Complete 4-vertex floating-point destination quadrilateral structure (`TFloatQuadrilateral`).
- `Dest[Index]`: Destination corner point (`TFloatPoint`) for vertex index `0..3`.
- `DestX[Index]`, `DestY[Index]`: Individual X/Y coordinate values (`TFloat`) for vertex index `0..3`.
- `DestX0`..`DestX3`, `DestY0`..`DestY3`: Convenience properties accessing specific corner vertex coordinates.
