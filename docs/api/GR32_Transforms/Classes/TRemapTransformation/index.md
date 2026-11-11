---
layout: doc
docType: api
unit: GR32_Transforms
entity: TRemapTransformation
kind: Class
declaration: "TRemapTransformation = class(TTransformation)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TTransformation
  - TRemapTransformation
summary: "Warps coordinates according to displacement vectors stored in a TVectorMap."
---

## Description

`TRemapTransformation` performs non-linear spatial displacement mapping using coordinate offset vectors stored in a [[TVectorMap]] (`VectorMap`).

[members]
