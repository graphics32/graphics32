---
layout: doc
docType: api
unit: GR32_Layers
parent: TLayerCollection
entity: TLayerCollection.Add
kind: Method
scope: Public
summary: "Creates a new layer instance of the specified layer class and appends it to the collection."
overloads:
  - signature: "function Add(ItemClass: TLayerClass): TCustomLayer; overload;"
    summary: "Instantiates a new layer of class ItemClass and appends it to the collection."
    parameters:
      - name: ItemClass
        type: TLayerClass
        description: "The class of layer to instantiate."
    returns:
      - type: TCustomLayer
        description: "The newly created layer instance."
  - signature: "function Add<T: TCustomLayer>: T; overload;"
    summary: "Generic overload instantiating and appending a layer of type T."
    returns:
      - type: T
        description: "The newly created layer instance cast to type T."
---

## Description

`Add` instantiates a new layer object and adds it to the top (end) of the collection list.
