---
layout: doc
docType: api
unit: GR32_Transforms
entity: TFishEyeTransformation
kind: Class
declaration: "TFishEyeTransformation = class(TTransformation)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TTransformation
  - TFishEyeTransformation
summary: "Applies a wide-angle fish-eye lens optical distortion effect."
---

## Description

`TFishEyeTransformation` applies a wide-angle fish-eye optical lens distortion around the center of `SrcRect`.

![Fish-Eye Transformation](/images/fisheye-transformation-png.png)

[members]
