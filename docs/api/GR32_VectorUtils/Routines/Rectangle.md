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

    returns:
      - type: TArrayOfFloatPoint
        description: "A [[TArrayOfFloatPoint]] array containing generated polygon coordinates."
  - signature: "function Rectangle(const R: TFloatRect): TArrayOfFloatPoint; overload;"
    summary: "Generates a floating-point rectangular polygon from TFloatRect R."
    parameters:
      - name: R
        type: TFloatRect
        description: "Input float rectangle."

    returns:
      - type: TArrayOfFloatPoint
        description: "A [[TArrayOfFloatPoint]] array containing generated polygon coordinates."
---

## Description

`Rectangle` creates a 4-point closed rectangular polygon array representing `R`.
