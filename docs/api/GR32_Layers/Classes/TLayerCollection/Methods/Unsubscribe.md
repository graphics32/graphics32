---
layout: doc
docType: api
unit: GR32_Layers
parent: TLayerCollection
entity: TLayerCollection.Unsubscribe
kind: Method
scope: Public
declaration: "procedure Unsubscribe(const ASubscriber: IInterface);"
summary: "Unregisters a previously subscribed layer notification listener."
parameters:
  - name: ASubscriber
    type: IInterface
    description: "Subscriber object to unregister."
---

## Description

`Unsubscribe` removes `ASubscriber` from the subscriber list.
