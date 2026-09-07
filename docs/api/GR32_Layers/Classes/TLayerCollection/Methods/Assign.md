---
layout: doc
docType: api
unit: GR32_Layers
parent: TLayerCollection
entity: TLayerCollection.Assign
kind: Method
scope: Public
declaration: "procedure Assign(Source: TPersistent); override;"
summary: "Copies layers from another TLayerCollection instance."
parameters:
  - name: Source
    type: TPersistent
    description: "Source persistent object (must be a TLayerCollection)."
---

## Description

`Assign` clears the existing layers and copies layer instances from `Source`.
