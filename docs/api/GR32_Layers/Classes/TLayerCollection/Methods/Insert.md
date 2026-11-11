---
layout: doc
docType: api
unit: GR32_Layers
parent: TLayerCollection
entity: TLayerCollection.Insert
kind: Method
scope: Public
summary: "Instantiates a new layer and inserts it at the specified index position."
overloads:
  - signature: "function Insert(Index: Integer; ItemClass: TLayerClass): TCustomLayer; overload;"
    summary: "Creates a new layer of ItemClass and inserts it at Index."
    parameters:
      - name: Index
        type: Integer
        description: "Zero-based position where the layer will be inserted."
      - name: ItemClass
        type: TLayerClass
        description: "Layer class to instantiate."
    returns:
      - type: TCustomLayer
        description: "The created layer instance."
  - signature: "function Insert<T: TCustomLayer>(Index: Integer): T; overload;"
    summary: "Generic overload instantiating and inserting a layer of type T at Index."
    parameters:
      - name: Index
        type: Integer
        description: "Zero-based insertion index."
    returns:
      - type: T
        description: "The created layer instance cast to type T."
---

## Description

`Insert` creates a new layer instance and inserts it into the Z-order list at `Index`.
