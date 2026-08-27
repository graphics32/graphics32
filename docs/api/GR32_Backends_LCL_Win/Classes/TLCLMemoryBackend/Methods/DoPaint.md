---
layout: doc
docType: api
unit: GR32_Backends_LCL_Win
parent: TLCLMemoryBackend
entity: TLCLMemoryBackend.DoPaint
kind: Method
scope: Public
summary: "Paints invalid surface regions directly onto an LCL TCanvas."
overloads:
  - signature: "procedure DoPaint(ABuffer: TBitmap32; AInvalidRects: TRectList; ACanvas: TCanvas); overload;"
    summary: "Paints multiple invalid rectangles."
    parameters:
      - name: ABuffer
        type: TBitmap32
        description: "Source bitmap."
      - name: AInvalidRects
        type: TRectList
        description: "Invalid rectangles."
      - name: ACanvas
        type: TCanvas
        description: "Destination canvas."

  - signature: "procedure DoPaint(ABuffer: TBitmap32; const AInvalidRect: TRect; ACanvas: TCanvas); overload;"
    summary: "Paints a single invalid rectangle."
    parameters:
      - name: ABuffer
        type: TBitmap32
        description: "Source bitmap."
      - name: AInvalidRect
        type: TRect
        description: "Invalid rectangle."
      - name: ACanvas
        type: TCanvas
        description: "Destination canvas."
---

# TLCLMemoryBackend.DoPaint

`DoPaint` renders heap pixel memory onto `ACanvas` using `SetDIBitsToDevice`.
