---
layout: doc
docType: api
unit: GR32_ColorGradients
parent: TColor32Gradient
entity: TColor32Gradient.GetColorAt
kind: Method
declaration: "function GetColorAt(Offset: TFloat): TColor32;"
summary: "Calculates the linear interpolated 32-bit ARGB color at a specified offset position."
parameters:
  - name: Offset
    type: TFloat
    description: "Normalized position offset in range [0.0, 1.0]."
returns:
  - type: TColor32
    description: "The interpolated 32-bit ARGB `TColor32` color at the given offset position."
---

## Description

`GetColorAt` evaluates the exact color at position `Offset`.

1. If `Offset <= 0.0`, returns `StartColor`.
2. If `Offset >= 1.0`, returns `EndColor`.
3. For $0.0 < \text{Offset} < 1.0$, locates bounding stops $S_i$ and $S_{i+1}$ such that $S_i.\text{Offset} \le \text{Offset} \le S_{i+1}.\text{Offset}$.
4. Computes interpolation factor $t = \frac{\text{Offset} - S_i.\text{Offset}}{S_{i+1}.\text{Offset} - S_i.\text{Offset}}$ and performs linear interpolation across Alpha, Red, Green, and Blue channels.
