---
layout: doc
docType: api
unit: GR32_Layers
entity: TRBDragState
kind: Type
declaration: "TRBDragState = (dsNone, dsMove, dsSizeL, dsSizeT, dsSizeR, dsSizeB, dsSizeTL, dsSizeTR, dsSizeBL, dsSizeBR);"
summary: "Identifies active drag operations on rectangular rubberband layers."
---

## Description

`TRBDragState` specifies which handle or body region of a `TRubberbandLayer` is currently being dragged by the user.

## Values

| Value | Description |
| --- | --- |
| `dsNone` | No drag operation active. |
| `dsMove` | Dragging the layer body to translate position. |
| `dsSizeL` | Sizing via left side handle. |
| `dsSizeT` | Sizing via top side handle. |
| `dsSizeR` | Sizing via right side handle. |
| `dsSizeB` | Sizing via bottom side handle. |
| `dsSizeTL` | Sizing via top-left corner handle. |
| `dsSizeTR` | Sizing via top-right corner handle. |
| `dsSizeBL` | Sizing via bottom-left corner handle. |
| `dsSizeBR` | Sizing via bottom-right corner handle. |
