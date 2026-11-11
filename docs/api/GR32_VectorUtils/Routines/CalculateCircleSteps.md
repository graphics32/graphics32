---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: CalculateCircleSteps
kind: Function
declaration: "function CalculateCircleSteps(Radius: TFloat): Cardinal;"
summary: "Calculates the recommended number of linear steps to approximate a circle of a given radius smoothly."
parameters:
  - name: Radius
    type: TFloat
    description: "Circle radius in pixels."
returns:
  - type: Cardinal
    description: "The calculated number of sampling steps."
---

## Description

`CalculateCircleSteps` computes an optimal number of linear segments required to render a smooth circle of the specified `Radius` without visible polygon faceting.
