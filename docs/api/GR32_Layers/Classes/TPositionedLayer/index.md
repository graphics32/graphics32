---
layout: doc
docType: api
unit: GR32_Layers
entity: TPositionedLayer
kind: Class
inheritance:
  - TPersistent
  - TNotifiablePersistent
  - TCustomLayer
  - TPositionedLayer
summary: "Base class for layers that possess position, size, and scaling properties."
---

## Description

`TPositionedLayer` inherits from `TCustomLayer` and adds rectangular positioning (`Location`), scaling (`Scaled`), adjusted coordinate mapping, and update region calculations (`GetAdjustedLocation`, `OnGetUpdateRect`).

[members]
