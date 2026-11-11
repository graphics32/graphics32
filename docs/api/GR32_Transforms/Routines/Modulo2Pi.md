---
layout: doc
docType: api
unit: GR32_Transforms
entity: Modulo2Pi
kind: Procedure
declaration: "procedure Modulo2Pi(var Angle: TFloat);"
summary: "Normalizes an angle in radians to the range [0, 2π)."
parameters:
  - name: Angle
    type: TFloat
    description: "Angle variable to be normalized in place."
---

## Description

`Modulo2Pi` wraps `Angle` into the interval $[0, 2\pi)$ in place.
