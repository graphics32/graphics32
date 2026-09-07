---
layout: doc
docType: api
unit: GR32_Layers
entity: ILayerListNotification
kind: Interface
declaration: "ILayerListNotification = interface(IUnknown)"
summary: "Interface for receiving notifications about structural changes to a layer collection."
---

## Description

`ILayerListNotification` notifies subscribers when layers are added, inserted, deleted, or cleared in a `TLayerCollection`.

## Methods

### LayerListNotify
```pascal
procedure LayerListNotify(ALayer: TCustomLayer; AAction: TLayerListNotification; AIndex: Integer);
```
Fired when structural modifications occur in the layer collection.
