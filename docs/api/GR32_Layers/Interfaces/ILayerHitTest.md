---
layout: doc
docType: api
unit: GR32_Layers
entity: ILayerHitTest
kind: Interface
declaration: "ILayerHitTest = interface(IUnknown)"
summary: "Interface representing an active hit-test context during rubberband layer interaction."
---

## Description

`ILayerHitTest` tracks mouse coordinates, initial layer locations, shift states, and active cursors during mouse interaction and dragging on rubberband layers.

## Properties

| Property | Type | Access | Description |
| --- | --- | --- | --- |
| `StartLocation` | `TFloatRect` | read/write | Initial location of the layer when interaction started. |
| `StartPosition` | `TPoint` | read-only | Initial screen/control mouse position when interaction started. |
| `CurrentPosition` | `TPoint` | read/write | Current mouse position during interaction. |
| `Shift` | `TShiftState` | read/write | Keyboard modifier shift state during interaction. |
| `Cursor` | `TCursor` | read/write | Cursor handle associated with the current interaction state. |
