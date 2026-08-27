---
layout: doc
docType: api
unit: GR32_Backends_LCL_Win
parent: TLCLBackend
entity: TLCLBackend.TextToPath
kind: Method
scope: Public
summary: "Converts text glyph outlines into vector paths."
overloads:
  - signature: "procedure TextToPath(Path: TCustomPath; const X, Y: TFloat; const Text: string; Flags: Cardinal); overload;"
    summary: "Appends glyph outlines of Text at (X, Y) to Path."
    parameters:
      - name: Path
        type: TCustomPath
        description: "Target path."
      - name: X, Y
        type: TFloat
        description: "Top-left position."
      - name: Text
        type: string
        description: "Text string."
      - name: Flags
        type: Cardinal
        description: "Flags."

  - signature: "procedure TextToPath(Path: TCustomPath; const DstRect: TFloatRect; const Text: string; Flags: Cardinal); overload;"
    summary: "Appends glyph outlines of Text within DstRect to Path."
    parameters:
      - name: Path
        type: TCustomPath
        description: "Target path."
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

# TLCLBackend.TextToPath

`TextToPath` converts character outlines of `Text` into vector contours in `Path`.
