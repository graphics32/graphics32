---
layout: doc
docType: api
unit: GR32_Layers
entity: TLayerState
kind: Type
aliases: [TLayerStates]
declaration: |
  TLayerState = (lsMouseLeft, lsMouseRight, lsMouseMiddle);
  TLayerStates = set of TLayerState;
summary: "Tracks active mouse buttons currently captured by a layer."
---

## Description

`TLayerState` indicates which mouse buttons are pressed during mouse drag operations. `TLayerStates` is a set of `TLayerState`.

## Values

| Value | Description |
| --- | --- |
| `lsMouseLeft` | Left mouse button is currently pressed over the layer. |
| `lsMouseRight` | Right mouse button is currently pressed over the layer. |
| `lsMouseMiddle` | Middle mouse button is currently pressed over the layer. |
