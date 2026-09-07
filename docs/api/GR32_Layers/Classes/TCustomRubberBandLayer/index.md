---
layout: doc
docType: api
unit: GR32_Layers
entity: TCustomRubberBandLayer
kind: Class
inheritance:
  - TPersistent
  - TNotifiablePersistent
  - TCustomLayer
  - TPositionedLayer
  - TCustomRubberBandLayer
summary: "Base class for interactive design layers displaying stippled selection frames and draggable handles."
seealso:
  - "[[TRubberbandLayer]]"
---

## Description

`TCustomRubberBandLayer` provides interactive design overlay functionality for transforming or editing underlying target layers (`ChildLayer`). It features:

- **Vertex Handles**: Configurable vertex handles with customizable styling (`HandleStyle`, `HandleSize`, `HandleFill`, `HandleFrame`, `HandleFrameSize`, `HandleHitZone`).
- **Frame Stippling**: Customizable dashed stipple pattern (`FrameStipple`, `FrameStippleStep`, `FrameStippleCounter`, `FrameStippleSegmented`).
- **Interactive Dragging & Quantization**: Quantized positioning (`Quantized`, `QuantizeShiftToggle`, `Quantize`), active hit test tracking (`ActiveHitTest`), and mouse message forwarding (`PassMouseToChild`).
- **Custom Painting Events**: `OnPaintHandle`, `OnUpdateHandle`, `OnHandleClicked`, `OnHandleMove`, `OnHandleMoved`, `OnUserChange`.

[members]
