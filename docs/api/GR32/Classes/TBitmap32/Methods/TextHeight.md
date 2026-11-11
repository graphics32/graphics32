---
layout: doc
docType: api
unit: GR32
parent: TBitmap32
entity: TBitmap32.TextHeight
kind: Method
scope: Public
declaration: "function TextHeight(const Text: string): Integer;"
summary: "Returns the height in pixels of a text string when rendered using current Font."
parameters:
  - name: Text
    type: string
    description: "Text string to measure."
returns:
  - type: Integer
    description: "Height in pixels."
seealso:
  - "[[TextExtent]]"
  - "[[TextWidth]]"
  - "[[Textout]]"
---

## Description

`TextHeight` returns the vertical extent (height) in pixels required to render `Text` using the bitmap's current `Font`.
