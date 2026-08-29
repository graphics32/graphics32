---
layout: doc
docType: api
unit: GR32_Transforms
entity: TPolarTransformation
kind: Class
declaration: "TPolarTransformation = class(TTransformation)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TTransformation
  - TPolarTransformation
summary: "Converts cartesian coordinates (X, Y) into polar coordinates (Angle, Radius) and vice versa."
---

## Description

`TPolarTransformation` converts cartesian $(X, Y)$ image coordinates into polar $(\theta, R)$ space mapped onto `DstRect`.

[members]
