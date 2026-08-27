---
layout: doc
docType: api
unit: GR32_Backends_VCL
parent: TGDIMemoryBackend
entity: TGDIMemoryBackend.DrawTo
kind: Method
scope: Public
summary: "Blits heap memory pixels to a target HDC using SetDIBitsToDevice."
overloads:
  - signature: "procedure DrawTo(hDst: HDC; DstX, DstY: Integer); overload;"
    summary: "Blits full surface to (DstX, DstY) on hDst."
    parameters:
      - name: hDst
        type: HDC
        description: "Destination DC handle."
      - name: DstX, DstY
        type: Integer
        description: "Destination position."

  - signature: "procedure DrawTo(hDst: HDC; const DstRect, SrcRect: TRect); overload;"
    summary: "Blits sub-rectangle SrcRect to DstRect on hDst."
    parameters:
      - name: hDst
        type: HDC
        description: "Destination DC handle."
      - name: DstRect, SrcRect
        type: TRect
        description: "Target and source bounds."
---

# TGDIMemoryBackend.DrawTo

`DrawTo` outputs raw heap pixel buffers directly to an external `HDC` handle using `SetDIBitsToDevice`.
