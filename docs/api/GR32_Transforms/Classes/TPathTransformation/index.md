---
layout: doc
docType: api
unit: GR32_Transforms
entity: TPathTransformation
kind: Class
declaration: "TPathTransformation = class(TTransformation)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TTransformation
  - TPathTransformation
summary: "Warps coordinates along curved top and bottom path curves."
---

## Description

`TPathTransformation` warps image coordinates to fit between two arbitrary floating-point boundary curves (`TopCurve` and `BottomCurve`).

[members]
