---
layout: doc
docType: api
unit: GR32_Backends_VCL
parent: TGDIBackend
entity: TGDIBackend.Draw
kind: Method
scope: Public
declaration: "procedure Draw(const DstRect, SrcRect: TRect; hSrc: HDC); overload;"
summary: "Blits pixel content from an external source device context onto this backend surface."
parameters:
  - name: DstRect
    type: TRect
    description: "Target rectangle on this surface."
  - name: SrcRect
    type: TRect
    description: "Source sub-rectangle on hSrc."
  - name: hSrc
    type: HDC
    description: "Source GDI device context handle."
---

# TGDIBackend.Draw

`Draw` uses GDI `StretchBlt` to copy pixel contents from `hSrc` onto the backend device context `Handle`.
