---
layout: doc
docType: api
unit: GR32_Image
parent: TCustomImage32
entity: TCustomImage32.ScrollToCenter
kind: Method
overloads:
  - signature: "procedure ScrollToCenter; overload;"
    summary: "Scrolls viewport to center bitmap center point."
  - signature: "procedure ScrollToCenter(X, Y: Integer); overload; virtual;"
    summary: "Scrolls viewport to center specified bitmap pixel coordinate."
    parameters:
      - name: X, Y
        type: Integer
        description: "Bitmap pixel coordinate to center in viewport."
seealso:
  - "[[Scroll]]"
---

## Description

`ScrollToCenter` adjusts `OffsetHorz` and `OffsetVert` to center either the entire bitmap or a specific bitmap pixel `(X, Y)` inside the control viewport.
