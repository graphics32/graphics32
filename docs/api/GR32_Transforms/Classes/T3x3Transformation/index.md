---
layout: doc
docType: api
unit: GR32_Transforms
entity: T3x3Transformation
kind: Class
declaration: "T3x3Transformation = class(TTransformation)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TTransformation
  - T3x3Transformation
summary: "Base class for spatial transformations defined by 3x3 homogeneous matrices."
---

## Description

`T3x3Transformation` is the base class for linear transformations that use a $3 \times 3$ matrix (`TFloatMatrix`) to map coordinates.

Internally it maintains both floating-point (`Matrix`, `FInverseMatrix`) and 16.16 fixed-point (`FFixedMatrix`, `FInverseFixedMatrix`) matrix structures for accelerated fixed-point sampling.

[members]
