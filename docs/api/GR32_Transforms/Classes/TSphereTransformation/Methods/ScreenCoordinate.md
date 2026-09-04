---
layout: doc
docType: api
unit: GR32_Transforms
parent: TSphereTransformation
entity: TSphereTransformation.ScreenCoordinate
kind: Method
declaration: "function ScreenCoordinate(var X, Y: TFloat): boolean;"
summary: "Projects spherical longitude and latitude coordinates back onto 2D screen space."
parameters:
  - name: X, Y
    type: TFloat
    description: "Input longitude and latitude coordinates; modified in place to screen coordinates."
returns:
  - type: boolean
    description: "Returns `True` if the projected point lies on the visible hemisphere face; otherwise `False`."
---

## Description

`ScreenCoordinate` converts spherical longitude and latitude coordinates $(X, Y)$ into 2D destination screen coordinates. Returns `True` if the point lies on the visible hemisphere face.
