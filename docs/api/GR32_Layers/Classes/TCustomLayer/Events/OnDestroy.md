---
layout: doc
docType: api
unit: GR32_Layers
parent: TCustomLayer
entity: TCustomLayer.OnDestroy
kind: Event
scope: Published
declaration: "property OnDestroy: TNotifyEvent read ... write ...;"
summary: "Fired immediately prior to layer destruction."
parameters:
  - name: Sender
    type: TObject
    description: "The layer instance being destroyed."
---

## Description

`OnDestroy` allows cleanup before the layer instance is freed.
