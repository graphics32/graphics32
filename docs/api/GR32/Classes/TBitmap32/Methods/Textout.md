---
layout: doc
docType: api
unit: GR32
parent: TBitmap32
entity: TBitmap32.Textout
kind: Method
scope: Public
summary: "Renders text string onto the bitmap surface at specified coordinates or bounding rectangle using the current Font."
overloads:
  - signature: "procedure Textout(X, Y: Integer; const Text: string); overload;"
    summary: "Renders a text string at position (X, Y)."
    parameters:
      - name: X, Y
        type: Integer
        description: "Top-left destination pixel coordinate for text rendering."
      - name: Text
        type: string
        description: "Text string to render."

  - signature: "procedure Textout(X, Y: Integer; const ClipRect: TRect; const Text: string); overload;"
    summary: "Renders a text string at position (X, Y) constrained within a clipping rectangle."
    parameters:
      - name: X, Y
        type: Integer
        description: "Top-left destination pixel coordinate for text rendering."
      - name: ClipRect
        type: TRect
        description: "Clipping rectangle bounding text output."
      - name: Text
        type: string
        description: "Text string to render."

  - signature: "procedure Textout(var DstRect: TRect; const Flags: Cardinal; const Text: string); overload;"
    summary: "Renders formatted text within a destination rectangle using Win32 API DT_* formatting flags."
    parameters:
      - name: DstRect
        type: TRect
        description: "Destination rectangle for text layout and rendering."
      - name: Flags
        type: Cardinal
        description: "DrawText formatting flags (e.g. DT_CENTER, DT_WORDBREAK, DT_CALCRECT)."
      - name: Text
        type: string
        description: "Text string to render."
seealso:
- "[[TextExtent]]"
- "[[TextHeight]]"
- "[[TextWidth]]"
- "[[RenderText]]"
- "[[TCanvas32.RenderText]]"
---

## Description

`Textout` draws text on the bitmap using backend text support (`ITextSupport`).

::: warning
`Textout` does not support transparency.

Internally `Textout` delegates drawing of text to the backend[^1]. Because the backend very likely does **not** support alpha-blending:
- The background (i.e. the bitmap) **must** be completely opaque (i.e. Alpha=255).
- The alpha of the text color is ignored (i.e. any use of semi-transparent text colors is pointless).

Otherwise the text will not blend correctly onto the background and text antialiasing will not work.

If you need transparency then use either [[RenderText]] or [[TCanvas32.RenderText]] instead.

[^1]: For example, on Windows the backend uses [TextOut](https://learn.microsoft.com/en-us/windows/win32/api/wingdi/nf-wingdi-textoutw) or [DrawText](https://learn.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-drawtext) for text output.
:::
