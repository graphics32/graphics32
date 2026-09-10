---
layout: doc
docType: api
unit: GR32_Image
parent: TCustomImage32
entity: TCustomImage32.SetBounds
kind: Method
declaration: "procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;"
summary: "Sets control bounds and invalidates cached bitmap rectangle."
parameters:
  - name: ALeft, ATop
    type: Integer
    description: "New top-left position."
  - name: AWidth, AHeight
    type: Integer
    description: "New control dimensions."
---

## Description

`SetBounds` updates position and size, invalidating internal viewport scale/offset caches.
