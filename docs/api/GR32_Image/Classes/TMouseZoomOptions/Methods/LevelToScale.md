---
layout: doc
docType: api
unit: GR32_Image
parent: TMouseZoomOptions
entity: TMouseZoomOptions.LevelToScale
kind: Method
declaration: "function LevelToScale(ALevel: Integer): Single;"
summary: "Converts discrete zoom step level to scale value."
parameters:
  - name: ALevel
    type: Integer
    description: "Zero-based step level index."
returns:
  - type: Single
    description: "Corresponding floating-point scale factor."
seealso:
  - "[[ScaleToLevel]]"
---

## Description

`LevelToScale` maps a step level index to its corresponding floating-point scale factor within logarithmic range [`MinScale`..`MaxScale`].
