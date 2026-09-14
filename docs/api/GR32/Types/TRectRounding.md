---
layout: doc
docType: api
unit: GR32
entity: TRectRounding
kind: Type
declaration: "type TRectRounding = (rrClosest, rrOutside, rrInside, rrLine);"
summary: "Specifies the coordinate rounding strategy when converting floating-point or fixed-point rectangles to integer TRect structures."
seealso:
  - "[[MakeRect]]"
  - "[[TFloatRect]]"
  - "[[TFixedRect]]"
---

## Description

`TRectRounding` specifies how fractional floating-point ([[TFloatRect]]) or fixed-point ([[TFixedRect]]) bounds are converted to integer `TRect` structures in [[MakeRect]] functions.

| Value | Description |
| --- | --- |
| `rrClosest` | Rounds each coordinate (`Left`, `Top`, `Right`, `Bottom`) independently to the nearest integer. |
| `rrOutside` | Expands the rectangle outward to enclose all fractional bounds (`Left` and `Top` floored, `Right` and `Bottom` ceiled). |
| `rrInside` | Shrinks the rectangle inward to fit strictly within the fractional bounds (`Left` and `Top` ceiled, `Right` and `Bottom` floored). |
| `rrLine` | Preserves line span dimensions when converting thin or zero-area bounding rectangles. Corresponds to `rrOutside` on a normalized rectangle. |
