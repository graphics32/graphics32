---
layout: doc
docType: api
unit: GR32_Transforms
entity: TNestedTransformation
kind: Class
declaration: "TNestedTransformation = class(TTransformation)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TTransformation
  - TNestedTransformation
summary: "Container class for combining multiple sequential transformations into a single transformation chain."
---

## Description

`TNestedTransformation` holds a list of child [[TTransformation]] objects (`Items`) and executes them sequentially in chain order during forward and reverse transformation calls.

[members]
