---
layout: doc
docType: api
unit: GR32_Layers
parent: TCustomLayer
entity: TCustomLayer.ControlToLayer
kind: Method
scope: Public
summary: "Converts viewport or control coordinates to local layer coordinates."
overloads:
  - signature: "function ControlToLayer(const p: TPoint): TPoint; overload; virtual;"
    summary: "Converts an integer point from control space to layer space."
    parameters:
      - name: p
        type: TPoint
        description: "Point in control coordinates."
    returns:
      - type: TPoint
        description: "Converted point in layer coordinates."
  - signature: "function ControlToLayer(const r: TRect): TRect; overload; virtual;"
    summary: "Converts an integer rectangle from control space to layer space."
    parameters:
      - name: r
        type: TRect
        description: "Rectangle in control coordinates."
    returns:
      - type: TRect
        description: "Converted rectangle in layer coordinates."
  - signature: "function ControlToLayer(const p: TFloatPoint): TFloatPoint; overload; virtual;"
    summary: "Converts a floating-point point from control space to layer space."
    parameters:
      - name: p
        type: TFloatPoint
        description: "Floating-point coordinates in control space."
    returns:
      - type: TFloatPoint
        description: "Converted point in layer coordinates."
  - signature: "function ControlToLayer(const r: TFloatRect): TFloatRect; overload; virtual;"
    summary: "Converts a floating-point rectangle from control space to layer space."
    parameters:
      - name: r
        type: TFloatRect
        description: "Floating-point rectangle in control space."
    returns:
      - type: TFloatRect
        description: "Converted rectangle in layer coordinates."
---

## Description

`ControlToLayer` transforms viewport or control coordinates into local layer space.
