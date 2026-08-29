---
layout: doc
docType: api
unit: GR32_Transforms
parent: TNestedTransformation
entity: TNestedTransformation.Add
kind: Method
declaration: "function Add(ItemClass: TTransformationClass): TTransformation;"
summary: "Instantiates and appends a new transformation class instance to the chain."
parameters:
  - name: ItemClass
    type: TTransformationClass
    description: "Transformation class to instantiate and append."
---

## Description

Creates an instance of `ItemClass`, adds it to the list of transformations, and returns the newly created transformation object.
