---
layout: doc
docType: api
unit: GR32_Resamplers
parent: TCustomKernel
entity: TCustomKernel.Filter
kind: Method
scope: Public
declaration: "function Filter(Value: TFloat): TFloat; virtual; abstract;"
summary: "Evaluates the 1D spatial kernel weight for a given distance offset Value."
parameters:
  - name: Value
    type: TFloat
    description: "Distance offset x from kernel center in sampling units."
---

## Description

`Filter` calculates the weighting factor $f(x)$ for a distance offset $x$. Derived classes override `Filter` to implement specific interpolation functions.
