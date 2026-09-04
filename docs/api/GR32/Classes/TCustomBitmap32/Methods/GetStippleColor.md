---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.GetStippleColor
kind: Method
scope: Public
declaration: "function GetStippleColor(Advance: Boolean = True): TColor32;"
summary: "Evaluates and returns current stipple color sample at active stipple phase."
parameters:
  - name: Advance
    type: Boolean
    description: "If True, advances StippleCounter after reading color."
returns:
  - type: TColor32
    description: "The 32-bit ARGB `TColor32` stipple pattern color at the current position."
---

## Description

`GetStippleColor` returns the `TColor32` at the active stipple phase.
