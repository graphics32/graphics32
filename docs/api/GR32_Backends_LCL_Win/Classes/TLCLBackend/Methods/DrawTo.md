---
layout: doc
docType: api
unit: GR32_Backends_LCL_Win
parent: TLCLBackend
entity: TLCLBackend.DrawTo
kind: Method
scope: Public
summary: "Blits or stretches surface pixels to a target device context."
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
    summary: "Stretches SrcRect onto DstRect on hDst."
    parameters:
      - name: hDst
        type: HDC
        description: "Destination DC."
      - name: DstRect, SrcRect
        type: TRect
        description: "Target and source bounds."
---

# TLCLBackend.DrawTo

`DrawTo` transfers surface pixel data to `hDst`.
