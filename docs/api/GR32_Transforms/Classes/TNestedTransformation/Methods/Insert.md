---
layout: doc
docType: api
unit: GR32_Transforms
parent: TNestedTransformation
entity: TNestedTransformation.Insert
kind: Method
declaration: "function Insert(Index: Integer; ItemClass: TTransformationClass): TTransformation;"
summary: "Inserts a new transformation class instance into the chain."
parameters:
  - name: Index
    type: Integer
    description: "Position index."
  - name: ItemClass
    type: TTransformationClass
    description: "Transformation class to instantiate."
---

## Description

Instantiates `ItemClass` and appends it to the nested transformation list.
