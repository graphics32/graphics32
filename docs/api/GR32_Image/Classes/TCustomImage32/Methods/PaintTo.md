---
layout: doc
docType: api
unit: GR32_Image
parent: TCustomImage32
entity: TCustomImage32.PaintTo
kind: Method
declaration: "procedure PaintTo(Dest: TBitmap32; DestRect: TRect); virtual;"
summary: "Exports image control content (bitmap and layers) onto a target TBitmap32 surface."
parameters:
  - name: Dest
    type: TBitmap32
    description: "Target bitmap surface to render onto."
  - name: DestRect
    type: TRect
    description: "Target destination rectangle on Dest bitmap."
---

## Description

`PaintTo` executes "export" paint stages (`psmExport`) to render bitmap and layers onto external `Dest` bitmap within `DestRect`.
