---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: Rectangle
kind: Function
summary: "Generates rectangular polygon contours."
overloads:
  - signature: "function Rectangle(const R: TRect): TArrayOfFloatPoint; overload;"
    summary: "Generates a floating-point rectangular polygon from TRect R."
    parameters:
      - name: R
        type: TRect
        description: "Input rectangle."

  - signature: "function Rectangle(const R: TFloatRect): TArrayOfFloatPoint; overload;"
    summary: "Generates a floating-point rectangular polygon from TFloatRect R."
    parameters:
      - name: R
        type: TFloatRect
        description: "Input float rectangle."
---

## Description

`Rectangle` creates a 4-point closed rectangular polygon array representing `R`.
