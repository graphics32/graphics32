---
layout: doc
docType: api
unit: GR32_Transforms
parent: TTransformation
entity: TTransformation.Changed
kind: Method
declaration: "procedure Changed; override;"
summary: "Invalidates internal transformation state and notifies observers."
---

## Description

Marks internal transformation state as invalid (`TransformValid := False`) and calls inherited `Changed` to trigger notification events.
