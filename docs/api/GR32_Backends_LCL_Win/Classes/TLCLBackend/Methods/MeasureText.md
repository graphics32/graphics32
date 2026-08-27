---
layout: doc
docType: api
unit: GR32_Backends_LCL_Win
parent: TLCLBackend
entity: TLCLBackend.MeasureText
kind: Method
scope: Public
declaration: "function MeasureText(const DstRect: TFloatRect; const Text: string; Flags: Cardinal): TFloatRect;"
summary: "Measures floating-point text bounding box."
parameters:
  - name: DstRect
    type: TFloatRect
    description: "Target box."
  - name: Text
    type: string
    description: "Text string."
  - name: Flags
    type: Cardinal
    description: "Flags."
---

# TLCLBackend.MeasureText

`MeasureText` calculates the floating-point bounding rectangle of `Text`.
