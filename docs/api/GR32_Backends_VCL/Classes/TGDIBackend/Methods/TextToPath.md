---
layout: doc
docType: api
unit: GR32_Backends_VCL
parent: TGDIBackend
entity: TGDIBackend.TextToPath
kind: Method
scope: Public
summary: "Converts text glyph outlines into vector paths using WinAPI GDI font outlines."
overloads:
  - signature: "procedure TextToPath(Path: TCustomPath; const X, Y: TFloat; const Text: string; Flags: Cardinal = 0); overload;"
    summary: "Appends glyph outlines of Text at (X, Y) to Path."
    parameters:
      - name: Path
        type: TCustomPath
        description: "Target vector path."
      - name: X, Y
        type: TFloat
        description: "Top-left position."
      - name: Text
        type: string
        description: "Text string."
      - name: Flags
        type: Cardinal
        description: "Alignment flags."

  - signature: "procedure TextToPath(Path: TCustomPath; const DstRect: TFloatRect; const Text: string; Flags: Cardinal); overload;"
    summary: "Appends glyph outlines of Text within DstRect to Path."
    parameters:
      - name: Path
        type: TCustomPath
        description: "Target vector path."
      - name: DstRect
        type: TFloatRect
        description: "Target rectangle."
      - name: Text
        type: string
        description: "Text string."
      - name: Flags
        type: Cardinal
        description: "Alignment flags."

  - signature: "procedure TextToPath(Path: TCustomPath; const X, Y: TFloat; const Text: string; const Layout: TTextLayout); overload;"
    summary: "Appends glyph outlines of Text at (X, Y) using TTextLayout to Path."
    parameters:
      - name: Path
        type: TCustomPath
        description: "Target vector path."
      - name: X, Y
        type: TFloat
        description: "Top-left position."
      - name: Text
        type: string
        description: "Text string."
      - name: Layout
        type: TTextLayout
        description: "Text layout configuration."

  - signature: "procedure TextToPath(Path: TCustomPath; const DstRect: TFloatRect; const Text: string; const Layout: TTextLayout); overload;"
    summary: "Appends glyph outlines of Text within DstRect using TTextLayout to Path."
    parameters:
      - name: Path
        type: TCustomPath
        description: "Target vector path."
      - name: DstRect
        type: TFloatRect
        description: "Target rectangle."
      - name: Text
        type: string
        description: "Text string."
      - name: Layout
        type: TTextLayout
        description: "Text layout configuration."
---

# TGDIBackend.TextToPath

`TextToPath` converts character outlines of `Text` rendered with `Font` into vector bezier/line segments in `Path`.
