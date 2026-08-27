---
layout: doc
docType: api
unit: GR32_Backends_VCL
parent: TGDIBackend
entity: TGDIBackend.DrawTo
kind: Method
scope: Public
summary: "Blits or stretches surface pixels onto a target destination device context."
overloads:
  - signature: "procedure DrawTo(hDst: HDC; DstX, DstY: Integer); overload;"
    summary: "Renders the full surface at top-left location (DstX, DstY) on hDst using StretchDIBits."
    parameters:
      - name: hDst
        type: HDC
        description: "Destination device context handle."
      - name: DstX, DstY
        type: Integer
        description: "Target top-left pixel position."

  - signature: "procedure DrawTo(hDst: HDC; const DstRect, SrcRect: TRect); overload;"
    summary: "Stretches sub-rectangle SrcRect onto DstRect on hDst using StretchBlt."
    parameters:
      - name: hDst
        type: HDC
        description: "Destination device context handle."
      - name: DstRect
        type: TRect
        description: "Destination rectangle."
      - name: SrcRect
        type: TRect
        description: "Source sub-rectangle."
---

# TGDIBackend.DrawTo

`DrawTo` transfers surface pixel contents to an external GDI device context `hDst`.
