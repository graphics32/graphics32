---
layout: doc
docType: api
unit: GR32_ColorGradients
entity: TColor32GradientStop
kind: Type
aliases: [TArrayOfColor32GradientStop]
declaration: |
  TColor32GradientStop = record
    Offset: TFloat;
    Color32: TColor32;
  end;
  TArrayOfColor32GradientStop = array of TColor32GradientStop;
summary: "Defines a color stop position offset and 32-bit ARGB color value along a color gradient."
---

## Description

`TColor32GradientStop` specifies a key position (offset) and corresponding color along a color gradient path.

The `Offset` parameter indicates relative distance along the normalized gradient domain, where `0.0` represents the start of the gradient and `1.0` represents the end.

## Record Fields

| Field | Type | Description |
| --- | --- | --- |
| `Offset` | [[TFloat]] | Relative offset position along the gradient in the normalized range $[0.0, 1.0]$. |
| `Color32` | [[TColor32]] | 32-bit ARGB color assigned to this gradient stop position. |

## See Also
- [[TColor32Gradient]]
- [[Color32GradientStop]]
