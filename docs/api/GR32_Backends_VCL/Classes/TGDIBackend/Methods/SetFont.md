---
layout: doc
docType: api
unit: GR32_Backends_VCL
parent: TGDIBackend
entity: TGDIBackend.SetFont
kind: Method
scope: Public
declaration: "procedure SetFont(const Font: TFont);"
summary: "Assigns properties from Font to the internal font object."
parameters:
  - name: Font
    type: TFont
    description: "Source font."
---

# TGDIBackend.SetFont

`SetFont` copies font parameters from `Font` using `FFont.Assign(Font)` and triggers `FontChanged`.
