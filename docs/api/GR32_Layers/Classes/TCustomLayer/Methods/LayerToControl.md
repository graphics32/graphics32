---
layout: doc
docType: api
unit: GR32_Layers
parent: TCustomLayer
entity: TCustomLayer.LayerToControl
kind: Method
scope: Public
summary: "Converts local layer coordinates to viewport or control coordinates."
overloads:
  - signature: "function LayerToControl(const p: TPoint): TPoint; overload; virtual;"
    summary: "Converts an integer point from layer space to control space."
    parameters:
      - name: p
        type: TPoint
        description: "Point in layer coordinates."
    returns:
      - type: TPoint
        description: "Converted point in control space."
  - signature: "function LayerToControl(const r: TRect): TRect; overload; virtual;"
    summary: "Converts an integer rectangle from layer space to control space."
    parameters:
      - name: r
        type: TRect
        description: "Rectangle in layer coordinates."
    returns:
      - type: TRect
        description: "Converted rectangle in control space."
  - signature: "function LayerToControl(const p: TFloatPoint): TFloatPoint; overload; virtual;"
    summary: "Converts a floating-point point from layer space to control space."
    parameters:
      - name: p
        type: TFloatPoint
        description: "Floating-point coordinates in layer space."
    returns:
      - type: TFloatPoint
        description: "Converted point in control space."
  - signature: "function LayerToControl(const r: TFloatRect): TFloatRect; overload; virtual;"
    summary: "Converts a floating-point rectangle from layer space to control space."
    parameters:
      - name: r
        type: TFloatRect
        description: "Floating-point rectangle in layer space."
    returns:
      - type: TFloatRect
        description: "Converted rectangle in control space."
---

## Description

`LayerToControl` transforms coordinates from local layer space into viewport or control coordinate space.
