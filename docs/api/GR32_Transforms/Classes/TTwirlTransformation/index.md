---
layout: doc
docType: api
unit: GR32_Transforms
entity: TTwirlTransformation
kind: Class
declaration: "TTwirlTransformation = class(TTransformation)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TTransformation
  - TTwirlTransformation
summary: "Applies a spiral twirl deformation to image coordinates."
---

## Description

`TTwirlTransformation` applies a non-linear rotational spiral warp around the image center. The rotation angle increases with distance from the center according to `Twirl`.

![Twirl Transformation](/images/twirl-transformation.png)

::: info Note
In the above image, a very low amount of twirl was used. With high twirl frequencies (more extreme settings of the `Twirl` property), it is recommended that antialiasing is applied (e.g. by supersampling).
:::

[members]
