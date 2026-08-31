---
layout: doc
docType: api
unit: GR32_ColorGradients
parent: TXGradientSampler
entity: TXGradientSampler.SetPoints
kind: Method
declaration: "procedure SetPoints(const StartPoint, EndPoint: TFloatPoint); virtual;"
summary: "Sets start and end point coordinates defining linear gradient direction vector."
parameters:
  - name: StartPoint
    type: TFloatPoint
    description: "Start position coordinate (offset 0.0)."
  - name: EndPoint
    type: TFloatPoint
    description: "End position coordinate (offset 1.0)."
---

## Description

Configures `StartPoint` $P_1$ and `EndPoint` $P_2$. The linear gradient vector is defined as $V = P_2 - P_1$.
