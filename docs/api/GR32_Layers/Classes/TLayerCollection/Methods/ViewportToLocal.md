---
layout: doc
docType: api
unit: GR32_Layers
parent: TLayerCollection
entity: TLayerCollection.ViewportToLocal
kind: Method
scope: Public
summary: "Converts buffer/control (viewport) coordinates to local bitmap coordinates."
overloads:
  - signature: "function ViewportToLocal(const APoint: TFloatPoint; AScaled: Boolean): TFloatPoint; overload;"
    summary: "Converts floating-point viewport coordinates to local bitmap coordinates."
    parameters:
      - name: APoint
        type: TFloatPoint
        description: "Viewport coordinates."
      - name: AScaled
        type: Boolean
        description: "Whether viewport scale and shift transforms should be inverted."
    returns:
      - type: TFloatPoint
        description: "Converted point in local bitmap coordinates."
  - signature: "function ViewportToLocal(const APoint: TPoint; AScaled: Boolean): TFloatPoint; overload;"
    summary: "Converts integer viewport coordinates to local bitmap coordinates."
    parameters:
      - name: APoint
        type: TPoint
        description: "Integer viewport coordinates."
      - name: AScaled
        type: Boolean
        description: "Whether viewport scale and shift transforms should be inverted."
    returns:
      - type: TFloatPoint
        description: "Converted floating-point point in local bitmap coordinates."
---

## Description

`ViewportToLocal` converts screen/control viewport coordinates back into local bitmap coordinate space.
