---
layout: doc
docType: api
unit: GR32_Resamplers
parent: TCubicKernel
entity: TCubicKernel.Coeff
kind: Property
scope: Published
declaration: "property Coeff: TFloat read FCoeff write SetCoeff;"
summary: "Specifies the cubic convolution coefficient parameter (default -0.5)."
---

## Description

`Coeff` controls the derivative tension at grid boundaries. Setting `Coeff` to $-0.5$ produces a standard Catmull-Rom spline, while $-0.75$ produces standard cubic interpolation.
