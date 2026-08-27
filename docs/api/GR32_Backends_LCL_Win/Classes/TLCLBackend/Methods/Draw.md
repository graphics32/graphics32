---
layout: doc
docType: api
unit: GR32_Backends_LCL_Win
parent: TLCLBackend
entity: TLCLBackend.Draw
kind: Method
scope: Public
declaration: "procedure Draw(const DstRect, SrcRect: TRect; hSrc: HDC); overload;"
summary: "Blits pixel content from an external device context."
parameters:
  - name: DstRect, SrcRect
    type: TRect
    description: "Target and source bounds."
  - name: hSrc
    type: HDC
    description: "Source device context."
---

# TLCLBackend.Draw

`Draw` uses `Windows.StretchBlt` to copy pixels from `hSrc` onto `Handle`.
