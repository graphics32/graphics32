---
layout: doc
docType: api
unit: GR32_Image
parent: TCustomImage32
entity: TCustomImage32.Scroll
kind: Method
overloads:
  - signature: "procedure Scroll(Dx, Dy: Integer); overload;"
    summary: "Scrolls viewport offsets by integer displacement."
    parameters:
      - name: Dx, Dy
        type: Integer
        description: "Horizontal and vertical displacement."
  - signature: "procedure Scroll(Dx, Dy: Single); overload; virtual;"
    summary: "Scrolls viewport offsets by floating-point displacement."
    parameters:
      - name: Dx, Dy
        type: Single
        description: "Sub-pixel horizontal and vertical displacement."
seealso:
  - "[[ScrollToCenter]]"
---

## Description

`Scroll` adjusts `OffsetHorz` and `OffsetVert` by displacing them by (`Dx`, `Dy`).
