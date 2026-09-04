---
layout: doc
docType: api
unit: GR32_Resamplers
parent: TWindowedKernel
entity: TWindowedKernel.Window
kind: Method
scope: Protected
declaration: "function Window(Value: TFloat): TFloat; virtual; abstract;"
summary: "Calculates the window weighting function value for a given distance within the window radius."
parameters:
  - name: Value
    type: TFloat
    description: "The normalized distance from the kernel center (in pixels)."
returns:
  - type: TFloat
    description: "The calculated window weighting factor $w(x)$ for the given radial offset `Value`."
---

## Description

`Window` is a protected abstract method that descendant classes override to compute the window weighting value $w(x)$ for a given radial offset `Value`.

The base [[TWindowedKernel.Filter]] method checks whether `Value` falls within the range $[-Width, Width]$. If `Value` is within bounds, `Filter` calls `Window(Value)` to retrieve the window attenuation factor.
