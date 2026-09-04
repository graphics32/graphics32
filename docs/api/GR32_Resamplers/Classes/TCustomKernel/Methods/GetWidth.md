---
layout: doc
docType: api
unit: GR32_Resamplers
parent: TCustomKernel
entity: TCustomKernel.GetWidth
kind: Method
scope: Public
declaration: "function GetWidth: TFloat; virtual; abstract;"
summary: "Returns the effective spatial radius (half-width) of the kernel."
returns:
  - type: TFloat
    description: "The effective spatial radius (half-width) of the kernel in pixels."
---

## Description

`GetWidth` returns the kernel half-width radius $W$. Beyond distance $x \ge W$, the filter response is identically zero.
