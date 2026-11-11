---
layout: doc
docType: api
unit: GR32_ColorGradients
parent: TColor32Gradient
entity: TColor32Gradient.Create
kind: Constructor
summary: "Initializes a TColor32Gradient instance with solid colors, a 2-stop linear range, or a stop array."
overloads:
  - signature: "constructor Create(Color: TColor32); overload;"
    summary: "Creates a solid gradient where all offsets evaluate to Color."
    parameters:
      - name: Color
        type: TColor32
        description: "Solid 32-bit ARGB color used across the entire gradient domain."
  - signature: "constructor Create(StartColor, EndColor: TColor32); overload;"
    summary: "Creates a standard two-stop linear gradient from StartColor (offset 0.0) to EndColor (offset 1.0)."
    parameters:
      - name: StartColor
        type: TColor32
        description: "Color assigned to offset 0.0."
      - name: EndColor
        type: TColor32
        description: "Color assigned to offset 1.0."
  - signature: "constructor Create(const GradientColors: TArrayOfColor32GradientStop); overload;"
    summary: "Creates a multi-stop gradient from an array of TColor32GradientStop records."
    parameters:
      - name: GradientColors
        type: TArrayOfColor32GradientStop
        description: "Array of color stop definitions specifying offset positions and colors."
---

## Description

Constructs and initializes a new [[TColor32Gradient]] instance. It sets up internal color stop lists (`FGradientColors`) and sorts stops in ascending order of offset.
