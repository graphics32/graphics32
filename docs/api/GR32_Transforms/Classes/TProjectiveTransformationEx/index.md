---
layout: doc
docType: api
unit: GR32_Transforms
entity: TProjectiveTransformationEx
kind: Class
declaration: "TProjectiveTransformationEx = class(T3x3Transformation)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TTransformation
  - T3x3Transformation
  - TProjectiveTransformationEx
summary: "Performs general projective mapping between an arbitrary source quadrilateral and destination quadrilateral."
---

## Description

`TProjectiveTransformationEx` maps coordinates between two arbitrary convex quadrilaterals (`SourceQuad` and `DestQuad`). It uses rational linear projective mappings to warp non-rectangular regions onto arbitrary target shapes.

[members]
