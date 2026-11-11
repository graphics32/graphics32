---
layout: doc
docType: api
unit: GR32_ColorGradients
parent: TColor32Gradient
entity: TColor32Gradient.SetColors
kind: Method
summary: "Populates gradient color stops from arrays or palette sources."
overloads:
  - signature: "procedure SetColors(const GradientColors: TArrayOfColor32GradientStop); overload;"
    summary: "Populates stops from an array of TColor32GradientStop records."
    parameters:
      - name: GradientColors
        type: TArrayOfColor32GradientStop
        description: "Array of color stop offset/color definitions."
  - signature: "procedure SetColors(const GradientColors: TArrayOfColor32); overload;"
    summary: "Populates stops from an array of TColor32 colors, distributing offsets equidistantly between 0.0 and 1.0."
    parameters:
      - name: GradientColors
        type: TArrayOfColor32
        description: "Array of colors."
  - signature: "procedure SetColors(const Palette: TPalette32); overload;"
    summary: "Populates stops from entries in a TPalette32 instance."
    parameters:
      - name: Palette
        type: TPalette32
        description: "Palette source."
---

## Description

Replaces all existing color stops with newly provided color entries.
