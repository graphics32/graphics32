---
layout: doc
docType: api
unit: GR32_Layers
parent: TCustomLayer
entity: TCustomLayer.ContentToLayer
kind: Method
scope: Public
summary: "Converts coordinates from internal layer content space to layer space."
overloads:
  - signature: "function ContentToLayer(const APoint: TPoint): TPoint; overload; virtual;"
    summary: "Converts an integer point from content space to layer space."
    parameters:
      - name: APoint
        type: TPoint
        description: "Content coordinate point."
    returns:
      - type: TPoint
        description: "Converted point in layer coordinates."
  - signature: "function ContentToLayer(const APoint: TFloatPoint): TFloatPoint; overload; virtual;"
    summary: "Converts a floating-point point from content space to layer space."
    parameters:
      - name: APoint
        type: TFloatPoint
        description: "Floating-point content coordinate point."
    returns:
      - type: TFloatPoint
        description: "Converted floating-point point in layer coordinates."
---

## Description

`ContentToLayer` translates coordinates from internal layer content coordinates (such as pixel positions in an unstretched source bitmap) to layer local space.
