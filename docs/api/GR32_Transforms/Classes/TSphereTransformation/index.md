---
layout: doc
docType: api
unit: GR32_Transforms
entity: TSphereTransformation
kind: Class
declaration: "TSphereTransformation = class(TTransformation)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TTransformation
  - TSphereTransformation
summary: "Projects 2D planisphere map coordinates onto a 3D spherical projection surface."
---

## Description

`TSphereTransformation` projects a 2D planar rectangular map (planisphere) onto a 3D spherical projection surface defined by `Center`, `Radius`, `Latitude`, and `Longitude`.

[members]
