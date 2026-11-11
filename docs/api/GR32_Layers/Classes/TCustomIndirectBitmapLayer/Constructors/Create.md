---
layout: doc
docType: api
unit: GR32_Layers
parent: TCustomIndirectBitmapLayer
entity: TCustomIndirectBitmapLayer.Create
kind: Constructor
scope: Public
summary: "Creates a new indirect bitmap layer instance."
overloads:
  - signature: "constructor Create(ALayerCollection: TLayerCollection); overload; override;"
    summary: "Creates a layer attached to ALayerCollection with no bitmap assigned."
    parameters:
      - name: ALayerCollection
        type: TLayerCollection
        description: "Layer collection to which this layer belongs."
  - signature: "constructor Create(ALayerCollection: TLayerCollection; ABitmap: TCustomBitmap32); reintroduce; overload;"
    summary: "Creates a layer attached to ALayerCollection referencing ABitmap."
    parameters:
      - name: ALayerCollection
        type: TLayerCollection
        description: "Layer collection to which this layer belongs."
      - name: ABitmap
        type: TCustomBitmap32
        description: "External bitmap to reference."
---

## Description

`Create` instantiates an indirect bitmap layer and optionally binds it to `ABitmap`.
