---
layout: doc
docType: api
unit: GR32_Backends_VCL
parent: TGDIBackend
entity: TGDIBackend.TextExtent
kind: Method
scope: Public
declaration: "function TextExtent(const Text: string): TSize;"
summary: "Calculates the width and height of Text in pixels using GDI GetTextExtentPoint32."
parameters:
  - name: Text
    type: string
    description: "String to measure."
---

# TGDIBackend.TextExtent

`TextExtent` measures the pixel extent of `Text` formatted with the current font.
