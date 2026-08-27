---
layout: doc
docType: api
unit: GR32_Backends_VCL
parent: TGDIBackend
entity: TGDIBackend.Textout
kind: Method
scope: Public
summary: "Renders a string of text onto the GDI device context."
overloads:
  - signature: "procedure Textout(X, Y: Integer; const Text: string); overload;"
    summary: "Renders Text at pixel position (X, Y)."
    parameters:
      - name: X, Y
        type: Integer
        description: "Target coordinates."
      - name: Text
        type: string
        description: "Text string."

  - signature: "procedure Textout(X, Y: Integer; const ClipRect: TRect; const Text: string); overload;"
    summary: "Renders Text clipped to ClipRect."
    parameters:
      - name: X, Y
        type: Integer
        description: "Target coordinates."
      - name: ClipRect
        type: TRect
        description: "Clipping rectangle."
      - name: Text
        type: string
        description: "Text string."

  - signature: "procedure Textout(var DstRect: TRect; const Flags: Cardinal; const Text: string); overload;"
    summary: "Renders Text formatted according to WinAPI DrawText alignment flags."
    parameters:
      - name: DstRect
        type: TRect
        description: "Target rectangle."
      - name: Flags
        type: Cardinal
        description: "WinAPI DrawText alignment flags."
      - name: Text
        type: string
        description: "Text string."
---

# TGDIBackend.Textout

`Textout` executes GDI string rendering (`ExtTextOut` or `DrawText`) using the backend's active font on `Handle`.
