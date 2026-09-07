---
layout: doc
docType: api
unit: GR32_Layers
parent: TLayerCollection
entity: TLayerCollection.Subscribe
kind: Method
scope: Public
declaration: "procedure Subscribe(const ASubscriber: IInterface);"
summary: "Registers an interface subscriber to receive layer notification callbacks."
parameters:
  - name: ASubscriber
    type: IInterface
    description: "Subscriber object implementing layer notification interfaces."
---

## Description

`Subscribe` adds `ASubscriber` to the internal listener list. Subscribers implementing `ILayerNotification`, `IUpdateRectNotification`, `ILayerUpdateNotification`, or `ILayerListNotification` will receive change callbacks.
