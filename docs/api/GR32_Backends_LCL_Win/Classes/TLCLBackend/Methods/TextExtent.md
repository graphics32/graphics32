---
layout: doc
docType: api
unit: GR32_Backends_LCL_Win
parent: TLCLBackend
entity: TLCLBackend.TextExtent
kind: Method
scope: Public
declaration: "function TextExtent(const Text: string): TSize;"
summary: "Calculates the width and height of Text in pixels."
parameters:
  - name: Text
    type: string
    description: "String to measure."
---

# TLCLBackend.TextExtent

`TextExtent` measures the pixel dimensions of `Text` using `GetTextExtentPoint32`.
