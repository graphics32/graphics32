---
layout: doc
docType: api
unit: GR32_ColorGradients
parent: TXGradientSampler
entity: TXGradientSampler.SimpleGradient
kind: Method
declaration: "procedure SimpleGradient(const StartPoint: TFloatPoint; StartColor: TColor32; const EndPoint: TFloatPoint; EndColor: TColor32);"
summary: "Configures start/end points and colors in a single call."
parameters:
  - name: StartPoint
    type: TFloatPoint
    description: "Start position."
  - name: StartColor
    type: TColor32
    description: "Start color."
  - name: EndPoint
    type: TFloatPoint
    description: "End position."
  - name: EndColor
    type: TColor32
    description: "End color."
---

## Description

Sets `StartPoint`, `EndPoint`, `StartColor`, and `EndColor` in a single call.
