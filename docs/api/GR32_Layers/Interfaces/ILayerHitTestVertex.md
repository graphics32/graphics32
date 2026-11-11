---
layout: doc
docType: api
unit: GR32_Layers
entity: ILayerHitTestVertex
kind: Interface
declaration: "ILayerHitTestVertex = interface(ILayerHitTest)"
summary: "Interface representing a hit-test context specifically for dragging a rubberband control vertex or handle."
---

## Description

`ILayerHitTestVertex` extends `ILayerHitTest` to track vertex handle index and initial vertex coordinates during vertex dragging operations.

## Properties

| Property | Type | Access | Description |
| --- | --- | --- | --- |
| `Vertex` | `Integer` | read/write | Zero-based index of the rubberband vertex being dragged. |
| `StartValue` | `TFloatPoint` | read/write | Initial floating-point coordinates of the vertex before dragging. |
