---
layout: doc
docType: api
unit: GR32
parent: TBitmap32
entity: TBitmap32.TextExtent
kind: Method
scope: Public
declaration: "function TextExtent(const Text: string): TSize;"
summary: "Calculates the pixel width and height dimensions of a text string when rendered using current Font."
parameters:
  - name: Text
    type: string
    description: "Text string to measure."
returns:
  - type: TSize
    description: "A TSize record containing calculated width (cX) and height (cY) in pixels."
seealso:
  - "[[TextWidth]]"
  - "[[TextHeight]]"
  - "[[Textout]]"
---

## Description

`TextExtent` queries backend text support (`ITextSupport`) to determine the pixel dimensions required to render `Text` using the bitmap's current `Font`.
