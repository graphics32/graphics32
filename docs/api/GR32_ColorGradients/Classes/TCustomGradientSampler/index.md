---
layout: doc
docType: api
unit: GR32_ColorGradients
entity: TCustomGradientSampler
kind: Class
abstract: true
summary: "Abstract base class for all continuous 2D color gradient samplers."
declaration: "TCustomGradientSampler = class(TCustomSampler)"
inheritance:
  - TObject
  - TPersistent
  - TNotifiablePersistent
  - TCustomSampler
  - TCustomGradientSampler
---

## Description

`TCustomGradientSampler` derives from [[TCustomSampler]] and acts as the foundation class for gradient samplers in Graphics32. It associates a [[TColor32Gradient]] with wrap mode behaviors (`wmClamp`, `wmRepeat`, `wmMirror`, `wmReflect`).

[members]
