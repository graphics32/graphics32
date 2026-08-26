---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.RaiseRectTS
kind: Method
scope: Public
summary: "Draws a 3D bevelled button/panel edge around a rectangle."
overloads:
  - signature: "procedure RaiseRectTS(X1, Y1, X2, Y2: Integer; Contrast: Integer); overload;"
    summary: "Draws 3D bevel specified by integer coordinates."
    parameters:
      - name: X1, Y1, X2, Y2
        type: Integer
        description: "Rectangle corner coordinates."
      - name: Contrast
        type: Integer
        description: "Bevel highlight and shadow contrast intensity."
  - signature: "procedure RaiseRectTS(const ARect: TRect; Contrast: Integer); overload;"
    summary: "Draws 3D bevel specified by TRect."
    parameters:
      - name: ARect
        type: TRect
        description: "Target TRect."
      - name: Contrast
        type: Integer
        description: "Bevel highlight/shadow contrast intensity."
---

## Description

`RaiseRectTS` renders raised 3D panel bevel edges.
