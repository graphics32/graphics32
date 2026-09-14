---
layout: doc
docType: api
unit: GR32
entity: InvertColor
kind: Function
declaration: "function InvertColor(Color32: TColor32): TColor32;"
summary: "Inverts the Red, Green, and Blue color channels of a 32-bit ARGB color while preserving the original alpha value."
parameters:
  - name: Color32
    type: TColor32
    description: "Input 32-bit ARGB color to invert."
returns:
  - type: TColor32
    description: "The color with inverted RGB components ($255 - R, 255 - G, 255 - B) and unchanged Alpha."
seealso:
  - "[[TColor32]]"
  - "[[SetAlpha]]"
---

## Description

`InvertColor` returns a new [[TColor32]] value with inverted color channels:
$$R_{\text{result}} = 255 - R_{\text{input}}$$
$$G_{\text{result}} = 255 - G_{\text{input}}$$
$$B_{\text{result}} = 255 - B_{\text{input}}$$
The alpha transparency channel ($A$) is preserved without modification.
