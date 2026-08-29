---
layout: doc
docType: api
unit: GR32_Transforms
parent: TProjectiveTransformationEx
entity: TProjectiveTransformationEx.Source
kind: Property
aliases: [SourceQuad, SourceX, SourceY, SourceX0, SourceX1, SourceX2, SourceX3, SourceY0, SourceY1, SourceY2, SourceY3]
declaration: |
  property SourceQuad: TFloatQuadrilateral read FSourceQuad write SetSourceQuad;
  property Source[Index: Integer]: TFloatPoint read GetSource write SetSource;
  property SourceX[Index: Integer]: TFloat read GetSourceX write SetSourceX;
  property SourceY[Index: Integer]: TFloat read GetSourceY write SetSourceY;
summary: "Source quadrilateral and indexed vertex coordinate properties."
---

## Description

Properties defining source quadrilateral coordinates for [[TProjectiveTransformationEx]]:
- `SourceQuad`: Complete 4-vertex floating-point source quadrilateral structure (`TFloatQuadrilateral`).
- `Source[Index]`: Source corner point (`TFloatPoint`) for vertex index `0..3`.
- `SourceX[Index]`, `SourceY[Index]`: Individual X/Y coordinate values (`TFloat`) for vertex index `0..3`.
- `SourceX0`..`SourceX3`, `SourceY0`..`SourceY3`: Convenience properties accessing specific corner vertex coordinates.
