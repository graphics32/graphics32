---
layout: doc
docType: api
unit: GR32_Image
parent: TCustomImage32
entity: TCustomImage32.SetupBitmap
kind: Method
declaration: "procedure SetupBitmap(DoClear: Boolean = False; ClearColor: TColor32 = $FF000000); virtual;"
summary: "Resizes internal Bitmap to match viewport dimensions."
parameters:
  - name: DoClear
    type: Boolean
    description: "If True, fills bitmap with ClearColor."
  - name: ClearColor
    type: TColor32
    description: "Color used to fill bitmap if DoClear is True."
---

## Description

`SetupBitmap` resizes `Bitmap` to match the control viewport width and height.
