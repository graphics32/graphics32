---
layout: doc
docType: api
unit: GR32_Transforms
entity: TDisturbanceTransformation
kind: Class
declaration: "TDisturbanceTransformation = class(TTransformation)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TTransformation
  - TDisturbanceTransformation
summary: "Applies random noise coordinate displacement (jitter) to images."
---

## Description

`TDisturbanceTransformation` applies random spatial coordinate displacements (jitter) within a range defined by `Disturbance`.

![Disturbance Transformation](/images/disturbance-transformation.png)

[members]
