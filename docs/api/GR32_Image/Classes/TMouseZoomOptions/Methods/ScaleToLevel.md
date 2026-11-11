---
layout: doc
docType: api
unit: GR32_Image
parent: TMouseZoomOptions
entity: TMouseZoomOptions.ScaleToLevel
kind: Method
declaration: "function ScaleToLevel(AScale: Single): Integer;"
summary: "Converts scale value to discrete zoom step level."
parameters:
  - name: AScale
    type: Single
    description: "Floating-point scale factor."
returns:
  - type: Integer
    description: "Zero-based step level index."
seealso:
  - "[[LevelToScale]]"
---

## Description

`ScaleToLevel` maps a floating-point scale factor to a discrete logarithmic step level between `0` and `Steps - 1`.
