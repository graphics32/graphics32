---
layout: doc
docType: api
unit: GR32_Transforms
entity: TProjectiveTransformation
kind: Class
declaration: "TProjectiveTransformation = class(T3x3Transformation)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TTransformation
  - T3x3Transformation
  - TProjectiveTransformation
summary: "Maps a rectangular source region onto an arbitrary convex 4-corner destination quadrilateral."
---

## Description

`TProjectiveTransformation` performs projective (perspective) spatial mapping by mapping a source rectangle (`SrcRect`) onto an arbitrary 4-corner convex destination quadrilateral defined by vertex properties (`X0..X3`, `Y0..Y3`).

![Projective Transformation](/images/projective-transformation.png)

::: info Note
The destination quadrilateral must be convex, otherwise the result is undefined.
:::

[members]
