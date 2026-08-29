---
layout: doc
docType: api
unit: GR32_Transforms
entity: TBloatTransformation
kind: Class
declaration: "TBloatTransformation = class(TTransformation)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TTransformation
  - TBloatTransformation
summary: "Applies a spherical bloat / pinch radial distortion to coordinates."
---

## Description

`TBloatTransformation` applies a non-linear radial bulge (bloat) or pinch deformation controlled by `BloatPower`.

Setting bloat power to a positive value gives a fisheye-like distortion, but with less spheric effect (as seen in the left image below). Setting the bloat power to a negative value will affect the transformation in the opposite way (as seen in the right image below).

![Bloat Transformation](/images/bloat-transformation.png)
[members]
