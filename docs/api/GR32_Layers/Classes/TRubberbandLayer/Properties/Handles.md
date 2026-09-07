---
layout: doc
docType: api
unit: GR32_Layers
parent: TRubberbandLayer
entity: TRubberbandLayer.Handles
kind: Property
scope: Public
declaration: "property Handles: TRBHandles read FHandles write SetHandles;"
summary: "Specifies visible/enabled rubberband elements."
seealso:
  - "[[TRBHandles]]"
---

## Description

`Handles` configures which selection handles (corners, sides, frame, center) are active on the layer.<br>
The default value is `[rhCenter, rhSides, rhCorners, rhFrame]`.
