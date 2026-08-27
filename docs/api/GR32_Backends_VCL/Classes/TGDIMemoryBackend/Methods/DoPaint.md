---
layout: doc
docType: api
unit: GR32_Backends_VCL
parent: TGDIMemoryBackend
entity: TGDIMemoryBackend.DoPaint
kind: Method
scope: Public
summary: "Paints invalid surface regions directly to TCanvas using SetDIBitsToDevice."
overloads:
  - signature: "procedure DoPaint(ABuffer: TBitmap32; AInvalidRects: TRectList; ACanvas: TCanvas); overload;"
    summary: "Paints multiple invalid rectangles onto ACanvas."
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
    summary: "Paints a single invalid rectangle onto ACanvas."
    parameters:
      - name: ABuffer
        type: TBitmap32
        description: "Source bitmap."
      - name: AInvalidRect
        type: TRect
        description: "Invalid rectangle."
      - name: ACanvas
        type: TCanvas
        description: "Destination VCL canvas."
---

# TGDIMemoryBackend.DoPaint

`DoPaint` renders heap pixel memory onto `ACanvas` using `SetDIBitsToDevice`.
