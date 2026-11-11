---
layout: doc
docType: api
unit: GR32_Layers
entity: ILayerNotification
kind: Interface
declaration: "ILayerNotification = interface(IUnknown)"
summary: "Unified listener interface for receiving layer updates, area invalidations, and layer list change notifications."
---

## Description

`ILayerNotification` defines callback methods for objects subscribing to a `TLayerCollection` to monitor layer visual changes, region updates, and list operations.

## Methods

### LayerUpdated
```pascal
procedure LayerUpdated(ALayer: TCustomLayer);
```
Fired when a layer requests a visual redrawing or property update.

### LayerAreaUpdated
```pascal
procedure LayerAreaUpdated(ALayer: TCustomLayer; const AArea: TRect; const AInfo: Cardinal);
```
Fired when a specific rectangular sub-region of a layer is invalidated.

### LayerListNotify
```pascal
procedure LayerListNotify(ALayer: TCustomLayer; AAction: TLayerListNotification; AIndex: Integer);
```
Fired when a layer is added, inserted, deleted, or cleared within the layer collection.
