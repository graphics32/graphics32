---
layout: doc
docType: api
unit: GR32_Transforms
parent: TSphereTransformation
entity: TSphereTransformation.IsInSphere
kind: Method
declaration: "function IsInSphere(CartesianX, CartesianY: TFloat): boolean;"
summary: "Returns True if specified Cartesian point falls within the sphere projection boundary."
parameters:
  - name: CartesianX, CartesianY
    type: TFloat
    description: "Cartesian screen coordinates."
returns:
  - type: boolean
    description: "Returns `True` if coordinate `(CartesianX, CartesianY)` falls within the sphere projection circle; otherwise `False`."
---

## Description

`IsInSphere` checks whether destination coordinate $(CartesianX, CartesianY)$ falls inside the circle defined by `Center` and `Radius`.
