---
layout: doc
docType: api
unit: GR32_Backends_VCL
parent: TGDIBackend
entity: TGDIBackend.DoPaint
kind: Method
scope: Public
summary: "Blits invalid bitmap rectangular regions onto a target TCanvas."
overloads:
  - signature: "procedure DoPaint(ABuffer: TBitmap32; AInvalidRects: TRectList; ACanvas: TCanvas); overload;"
    summary: "Blits multiple invalid rectangles onto ACanvas using BitBlt."
    parameters:
      - name: ABuffer
        type: TBitmap32
        description: "Source bitmap."
      - name: AInvalidRects
        type: TRectList
        description: "List of invalid rectangular areas."
      - name: ACanvas
        type: TCanvas
        description: "Destination VCL canvas."

  - signature: "procedure DoPaint(ABuffer: TBitmap32; const AInvalidRect: TRect; ACanvas: TCanvas); overload;"
    summary: "Blits a single invalid rectangle onto ACanvas."
    parameters:
      - name: ABuffer
        type: TBitmap32
        description: "Source bitmap."
      - name: AInvalidRect
        type: TRect
        description: "Invalid rectangle bounds."
      - name: ACanvas
        type: TCanvas
        description: "Destination VCL canvas."
---

# TGDIBackend.DoPaint

`DoPaint` transfers modified pixel regions from `ABuffer` onto `ACanvas` using GDI `BitBlt`.
