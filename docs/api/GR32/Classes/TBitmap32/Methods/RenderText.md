---
layout: doc
docType: api
unit: GR32
parent: TBitmap32
entity: TBitmap32.RenderText
kind: Method
scope: Public
summary: "Renders anti-aliased text directly into the bitmap buffer using specified color."
overloads:
  - signature: "procedure RenderText(X, Y: Integer; const Text: string; Color: TColor32; AntiAlias: boolean); overload;"
    summary: "Renders text at (X, Y) with optional anti-aliasing."
    parameters:
      - name: X, Y
        type: Integer
        description: "Top-left destination pixel coordinate."
      - name: Text
        type: string
        description: "Text string to render."
      - name: Color
        type: TColor32
        description: "Color32 value used to draw text."
      - name: AntiAlias
        type: boolean
        description: "Enables or disables anti-aliasing for font rendering."

  - signature: "procedure RenderText(X, Y: Integer; const Text: string; Color: TColor32); overload;"
    summary: "Renders text at (X, Y) using specified Color32 color with font's current anti-aliasing settings."
    parameters:
      - name: X, Y
        type: Integer
        description: "Top-left destination pixel coordinate."
      - name: Text
        type: string
        description: "Text string to render."
      - name: Color
        type: TColor32
        description: "Color32 value used to draw text."
seealso:
- "[[Textout]]"
- "[[Font]]"
- "[[TCanvas32.RenderText]]"
---

## Description

`RenderText` draws high-quality alpha-blended or anti-aliased text directly onto the bitmap using Graphics32 pixel blending.

::: info
Internally `RenderText` uses [[Textout]] to render the text but the limitations of `Textout`, with regard to transparency and blending, does not apply to `RenderText`.
:::

::: tip
For highest quality text output it is recommended that you use [[TCanvas32.RenderText]] instead of `RenderText`.
:::
