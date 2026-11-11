---
layout: doc
docType: api
unit: GR32_Layers
parent: TLayerCollection
entity: TLayerCollection.LocalToViewport
kind: Method
scope: Public
summary: "Converts local bitmap coordinates to buffer/control (viewport) coordinates."
overloads:
  - signature: "function LocalToViewport(const APoint: TFloatPoint; AScaled: Boolean): TFloatPoint; overload;"
    summary: "Converts a floating-point point from local bitmap coordinates to viewport coordinates."
    parameters:
      - name: APoint
        type: TFloatPoint
        description: "Local bitmap coordinates."
      - name: AScaled
        type: Boolean
        description: "Whether viewport scale and shift transforms should be applied."
    returns:
      - type: TFloatPoint
        description: "Converted point in viewport coordinates."
  - signature: "function LocalToViewport(const APoint: TPoint; AScaled: Boolean): TFloatPoint; overload;"
    summary: "Converts an integer point from local bitmap coordinates to viewport coordinates."
    parameters:
      - name: APoint
        type: TPoint
        description: "Local integer bitmap coordinates."
      - name: AScaled
        type: Boolean
        description: "Whether viewport scale and shift transforms should be applied."
    returns:
      - type: TFloatPoint
        description: "Converted floating-point point in viewport coordinates."
---

## Description

`LocalToViewport` transforms coordinates from local bitmap space to screen/viewport space based on active viewport scale and translation settings.
