---
layout: doc
docType: api
unit: GR32_Layers
entity: TLayerListNotification
kind: Type
declaration: "TLayerListNotification = (lnLayerAdded, lnLayerInserted, lnLayerDeleted, lnCleared);"
summary: "Indicates the nature of a structural change within a layer collection."
---

## Description

`TLayerListNotification` defines notification codes passed to `ILayerListNotification` listeners and `TLayerListNotifyEvent` handlers when `TLayerCollection` is modified.

## Values

| Value | Description |
| --- | --- |
| `lnLayerAdded` | A layer was appended to the collection. |
| `lnLayerInserted` | A layer was inserted at a specific index. |
| `lnLayerDeleted` | A layer was removed/deleted from the collection. |
| `lnCleared` | The layer collection was cleared. |
