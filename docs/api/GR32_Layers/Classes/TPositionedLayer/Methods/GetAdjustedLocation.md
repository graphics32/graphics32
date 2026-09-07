---
layout: doc
docType: api
unit: GR32_Layers
parent: TPositionedLayer
entity: TPositionedLayer.GetAdjustedLocation
kind: Method
scope: Public
declaration: "function GetAdjustedLocation: TFloatRect;"
summary: "Returns the layer's Location converted to viewport (buffer/control) coordinates."
returns:
  - type: TFloatRect
    description: "Bounding location rectangle in viewport coordinates."
---

## Description

`GetAdjustedLocation` applies viewport scale and shift transforms (if `Scaled` is `True`) to return the layer's actual screen/viewport location.
