---
layout: doc
docType: api
unit: GR32_Backends_VCL
parent: TGDIBackend
entity: TGDIBackend.MeasureText
kind: Method
scope: Public
summary: "Measures floating-point text bounding rectangles."
overloads:
  - signature: "function MeasureText(const DstRect: TFloatRect; const Text: string; Flags: Cardinal): TFloatRect; overload;"
    summary: "Measures text bounds given alignment flags."
    parameters:
      - name: DstRect
        type: TFloatRect
        description: "Constraints box."
      - name: Text
        type: string
        description: "Text string."
      - name: Flags
        type: Cardinal
        description: "Flags."

  - signature: "function MeasureText(const DstRect: TFloatRect; const Text: string; const Layout: TTextLayout): TFloatRect; overload;"
    summary: "Measures text bounds given TTextLayout."
    parameters:
      - name: DstRect
        type: TFloatRect
        description: "Constraints box."
      - name: Text
        type: string
        description: "Text string."
      - name: Layout
        type: TTextLayout
        description: "Text layout."
---

# TGDIBackend.MeasureText

`MeasureText` calculates the floating-point bounding box of `Text`.
