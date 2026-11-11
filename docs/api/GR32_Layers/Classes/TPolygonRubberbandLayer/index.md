---
layout: doc
docType: api
unit: GR32_Layers
entity: TPolygonRubberbandLayer
kind: Class
inheritance:
  - TPersistent
  - TNotifiablePersistent
  - TCustomLayer
  - TPositionedLayer
  - TCustomRubberBandLayer
  - TPolygonRubberbandLayer
summary: "Rubberband selection layer presenting an arbitrary multi-vertex polygon."
seealso:
  - "[[TRubberbandLayer]]"
---

## Description

`TPolygonRubberbandLayer` promotes public access to `Vertices`, enabling interactive editing of arbitrary polygon shapes.
`TPolygonRubberbandLayer` allows arbitrary polygonal vertex sets ([[TArrayOfFloatPoint]]) to be assigned and interactively manipulated using vertex handles.

[members]
