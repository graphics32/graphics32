---
layout: doc
docType: api
unit: GR32_Image
parent: TCustomPaintBox32
entity: TCustomPaintBox32.SetBounds
kind: Method
scope: Public
declaration: "procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;"
summary: "Sets control position and dimensions, adjusting internal rendering buffer."
parameters:
  - name: ALeft
    type: Integer
    description: "Left coordinate position relative to parent."
  - name: ATop
    type: Integer
    description: "Top coordinate position relative to parent."
  - name: AWidth
    type: Integer
    description: "Control width in pixels."
  - name: AHeight
    type: Integer
    description: "Control height in pixels."
---

## Description

`SetBounds` updates the control position and size and resizes the underlying double-buffered surface (`Buffer`) accordingly.
