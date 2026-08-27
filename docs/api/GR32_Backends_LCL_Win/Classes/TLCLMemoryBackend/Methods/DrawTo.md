---
layout: doc
docType: api
unit: GR32_Backends_LCL_Win
parent: TLCLMemoryBackend
entity: TLCLMemoryBackend.DrawTo
kind: Method
scope: Public
summary: "Blits heap memory pixels to an HDC using SetDIBitsToDevice."
overloads:
  - signature: "procedure DrawTo(hDst: HDC; DstX, DstY: Integer); overload;"
    summary: "Blits full surface to (DstX, DstY) on hDst."
    parameters:
      - name: hDst
        type: HDC
        description: "Destination DC."
      - name: DstX, DstY
        type: Integer
        description: "Top-left position."

  - signature: "procedure DrawTo(hDst: HDC; const DstRect, SrcRect: TRect); overload;"
    summary: "Blits SrcRect to DstRect on hDst."
    parameters:
      - name: hDst
        type: HDC
        description: "Destination DC."
      - name: DstRect, SrcRect
        type: TRect
        description: "Target and source bounds."
---

# TLCLMemoryBackend.DrawTo

`DrawTo` outputs heap pixel buffers directly to an external `HDC` handle using `SetDIBitsToDevice`.
