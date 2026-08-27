---
layout: doc
docType: api
unit: GR32_Backends_LCL_Win
parent: TLCLBackend
entity: TLCLBackend.Textout
kind: Method
scope: Public
summary: "Renders text on the LCL device context."
overloads:
  - signature: "procedure Textout(X, Y: Integer; const Text: string); overload;"
    summary: "Renders Text at (X, Y)."
    parameters:
      - name: X, Y
        type: Integer
        description: "Target position."
      - name: Text
        type: string
        description: "Text string."

  - signature: "procedure Textout(X, Y: Integer; const ClipRect: TRect; const Text: string); overload;"
    summary: "Renders Text clipped to ClipRect."
    parameters:
      - name: X, Y
        type: Integer
        description: "Target position."
      - name: ClipRect
        type: TRect
        description: "Clipping box."
      - name: Text
        type: string
        description: "Text string."

  - signature: "procedure Textout(var DstRect: TRect; const Flags: Cardinal; const Text: string); overload;"
    summary: "Renders Text formatted using DrawText flags."
    parameters:
      - name: DstRect
        type: TRect
        description: "Target box."
      - name: Flags
        type: Cardinal
        description: "DrawText flags."
      - name: Text
        type: string
        description: "Text string."
---

# TLCLBackend.Textout

`Textout` renders `Text` using `ExtTextout` or `DrawText` on `Handle`.
