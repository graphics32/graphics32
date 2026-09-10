---
layout: doc
docType: api
unit: GR32_Image
parent: TCustomImage32
entity: TCustomImage32.Invalidate
kind: Method
overloads:
  - signature: "procedure Invalidate; overload; override;"
    summary: "Invalidates entire control surface and resets viewport cache."
  - signature: "procedure Invalidate(const Rect: TRect); reintroduce; overload; virtual;"
    summary: "Invalidates specified sub-rectangle in viewport space."
    parameters:
      - name: Rect
        type: TRect
        description: "Sub-rectangle in viewport coordinates to invalidate."
---

## Description

`Invalidate` invalidates either the full control surface or a targeted sub-rectangle (`Rect`) in control viewport space.
