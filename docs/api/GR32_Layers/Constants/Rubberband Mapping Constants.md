---
layout: doc
docType: api
unit: GR32_Layers
entity: Rubberband Mapping Constants
kind: Constant
aliases: [VertexToDragState, DragStateToVertex]
summary: "Lookup arrays mapping rubberband vertex indices to drag states and vice versa."
---

## Description

The rubberband mapping constants provide bi-directional mapping between 8-vertex handle indices (0..7) and `TRBDragState` values.

## Constants Table

| Constant | Type | Description |
| --- | --- | --- |
| `VertexToDragState` | `array[0..7] of TRBDragState` | Maps vertex indices 0..7 to `TRBDragState` (`dsSizeTL`, `dsSizeT`, `dsSizeTR`, `dsSizeR`, `dsSizeBR`, `dsSizeB`, `dsSizeBL`, `dsSizeL`). |
| `DragStateToVertex` | `array[TRBDragState] of Integer` | Maps `TRBDragState` values to corresponding vertex indices (or `-1` for `dsNone` / `dsMove`). |
