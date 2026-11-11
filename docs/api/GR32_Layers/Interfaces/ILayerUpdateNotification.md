---
layout: doc
docType: api
unit: GR32_Layers
entity: ILayerUpdateNotification
kind: Interface
declaration: "ILayerUpdateNotification = interface(IUnknown)"
summary: "Interface for listening to individual layer update events."
---

## Description

`ILayerUpdateNotification` provides a simple notification interface for receiving updates when a specific layer changes or requests repainting.

## Methods

### LayerUpdated
```pascal
procedure LayerUpdated(ALayer: TCustomLayer);
```
Fired when `ALayer` updates its state or requests a repaint.
