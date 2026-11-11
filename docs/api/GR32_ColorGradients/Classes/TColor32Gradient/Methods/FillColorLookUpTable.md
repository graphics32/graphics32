---
layout: doc
docType: api
unit: GR32_ColorGradients
parent: TColor32Gradient
entity: TColor32Gradient.FillColorLookUpTable
kind: Method
summary: "Fills a destination color array or lookup table object with interpolated gradient entries."
overloads:
  - signature: "procedure FillColorLookUpTable(var ColorLUT: array of TColor32); overload;"
    summary: "Fills an open array of TColor32 values."
    parameters:
      - name: ColorLUT
        type: array of TColor32
        description: "Target color array."
  - signature: "procedure FillColorLookUpTable(ColorLUT: PColor32Array; Count: Integer); overload;"
    summary: "Fills a pointer buffer with Count pre-calculated color entries."
    parameters:
      - name: ColorLUT
        type: PColor32Array
        description: "Pointer to destination color array memory block."
      - name: Count
        type: Integer
        description: "Number of color entries to generate."
  - signature: "procedure FillColorLookUpTable(ColorLUT: TColor32LookupTable); overload;"
    summary: "Fills a TColor32LookupTable instance."
    parameters:
      - name: ColorLUT
        type: TColor32LookupTable
        description: "Target lookup table instance."
---

## Description

Fills pre-allocated color tables by sampling `GetColorAt` at $N$ equidistant intervals across $[0.0, 1.0]$.
