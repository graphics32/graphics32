---
layout: doc
docType: api
unit: GR32_Layers
entity: TLayerCollection
kind: Class
aliases: [TLayerCollectionClass]
declaration: |
  TLayerCollection = class(TPersistent)
  TLayerCollectionClass = class of TLayerCollection;
inheritance:
  - TPersistent
  - TLayerCollection
summary: "Manages a collection of visual layers, Z-ordering, coordinate conversions, and layer notifications."
---

## Description

`TLayerCollection` is a collection container for `TCustomLayer` objects. It manages the ordered list of layers rendered on top of a control or bitmap canvas, handling layer insertion, removal, Z-order reordering, mouse message dispatching, and coordinate space transformations.

[members]
