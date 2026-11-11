---
layout: doc
docType: api
unit: GR32_Layers
parent: TPositionedLayer
entity: TPositionedLayer.Location
kind: Property
scope: Public
declaration: "property Location: TFloatRect read FLocation write SetLocation;"
summary: "Specifies the position and size rectangle of the layer."
---

## Description

`Location` defines the bounding rectangle of the layer. If `Scaled` is `True`, coordinates are relative to local bitmap space; if `Scaled` is `False`, coordinates are in control/viewport space.
