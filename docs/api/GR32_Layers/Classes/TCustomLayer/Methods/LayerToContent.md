---
layout: doc
docType: api
unit: GR32_Layers
parent: TCustomLayer
entity: TCustomLayer.LayerToContent
kind: Method
scope: Public
summary: "Converts coordinates from local layer space to internal content space."
overloads:
  - signature: "function LayerToContent(const APoint: TPoint): TPoint; overload; virtual;"
    summary: "Converts an integer point from layer coordinates to content coordinates."
    parameters:
      - name: APoint
        type: TPoint
        description: "Layer coordinate point."
    returns:
      - type: TPoint
        description: "Converted point in internal content coordinates."
  - signature: "function LayerToContent(const APoint: TFloatPoint): TFloatPoint; overload; virtual;"
    summary: "Converts a floating-point point from layer coordinates to content coordinates."
    parameters:
      - name: APoint
        type: TFloatPoint
        description: "Floating-point point in layer coordinates."
    returns:
      - type: TFloatPoint
        description: "Converted floating-point point in content coordinates."
---

## Description

`LayerToContent` maps local layer coordinates into internal content coordinate space.
