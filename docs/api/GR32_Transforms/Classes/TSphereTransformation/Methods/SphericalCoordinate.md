---
layout: doc
docType: api
unit: GR32_Transforms
parent: TSphereTransformation
entity: TSphereTransformation.SphericalCoordinate
kind: Method
declaration: "function SphericalCoordinate(CartesianX, CartesianY: TFloat): TFloatPoint;"
summary: "Converts Cartesian screen coordinates to spherical latitude and longitude."
parameters:
  - name: CartesianX, CartesianY
    type: TFloat
    description: "Cartesian screen coordinates."
---

## Description

`SphericalCoordinate` converts screen point $(CartesianX, CartesianY)$ into latitude and longitude angles on the sphere surface.
