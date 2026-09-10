---
layout: doc
docType: api
unit: GR32
parent: TBitmap32
entity: TBitmap32.TextWidth
kind: Method
scope: Public
declaration: "function TextWidth(const Text: string): Integer;"
summary: "Returns the width in pixels of a text string when rendered using current Font."
parameters:
  - name: Text
    type: string
    description: "Text string to measure."
returns:
  - type: Integer
    description: "Width in pixels."
seealso:
  - "[[TextExtent]]"
  - "[[TextHeight]]"
  - "[[Textout]]"
---

## Description

`TextWidth` returns the horizontal extent (width) in pixels required to render `Text` using the bitmap's current `Font`.
