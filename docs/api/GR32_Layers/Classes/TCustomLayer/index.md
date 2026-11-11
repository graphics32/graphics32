---
layout: doc
docType: api
unit: GR32_Layers
entity: TCustomLayer
kind: Class
aliases: [TLayerClass]
declaration: |
  TCustomLayer = class(TNotifiablePersistent)
  TLayerClass = class of TCustomLayer;
inheritance:
  - TPersistent
  - TNotifiablePersistent
  - TCustomLayer
summary: "Abstract base class for visual and interactive layers managed within a TLayerCollection."
---

## Description

`TCustomLayer` is the base class for all visual and interactive layers in Graphics32. It provides common layer options (`LayerOptions`), hit testing, visibility control, Z-ordering operations (`BringToFront`, `SendToBack`), coordinate transformations, and mouse/keyboard event delegates.

[members]
