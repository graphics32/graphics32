---
layout: doc
docType: api
unit: GR32_Layers
parent: TCustomRubberBandLayer
entity: TCustomRubberBandLayer.QuantizeShiftToggle
kind: Property
scope: Public
declaration: "property QuantizeShiftToggle: TLayerShiftState read FQuantizeShiftToggle write FQuantizeShiftToggle default [ssAlt];"
summary: "Keyboard modifier shift key combination that toggles grid snapping during drag operations."
seealso:
  - "[[Quantized]] property"
  - "[[Quantize]] method"
---

## Description

`QuantizeShiftToggle` specifies shift key modifiers (default `[ssAlt]`) that invert or toggle grid quantization during dragging. Set to `[]` to disable the toggle.
