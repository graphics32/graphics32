---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.Roll
kind: Method
scope: Public
declaration: "procedure Roll(Dx, Dy: Integer; FillBack: Boolean; FillColor: TColor32);"
summary: "Scrolls/shifts pixel contents horizontally and vertically with optional background fill."
parameters:
  - name: Dx, Dy
    type: Integer
    description: "Horizontal and vertical pixel scroll deltas."
  - name: FillBack
    type: Boolean
    description: "If True, fills exposed empty areas with FillColor."
  - name: FillColor
    type: TColor32
    description: "Color used for filling exposed background areas."
---

## Description

`Roll` shifts bitmap pixel data by $(Dx, Dy)$.
