---
layout: doc
docType: api
unit: GR32_ColorGradients
parent: TCustomLinearGradientPolygonFiller
entity: TCustomLinearGradientPolygonFiller.SetPoints
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

Sets start position $P_1$ and end position $P_2$ for linear polygon filling.
