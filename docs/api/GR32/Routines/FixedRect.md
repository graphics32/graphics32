---
layout: doc
docType: api
unit: GR32
entity: FixedRect
kind: Function
declaration: |
  function FixedRect(const L, T, R, B: TFixed): TFixedRect; overload;
  function FixedRect(const TopLeft, BottomRight: TFixedPoint): TFixedRect; overload;
  function FixedRect(const ARect: TRect): TFixedRect; overload;
  function FixedRect(const FR: TFloatRect): TFixedRect; overload;
summary: "Constructs a TFixedRect structure in 16.16 fixed-point precision from individual coordinates, point pairs, integer rectangles, or floating-point rectangles."
parameters:
  - name: L, T, R, B
    type: TFixed
    description: "Left, top, right, and bottom coordinates in 16.16 fixed-point format."
  - name: TopLeft, BottomRight
    type: TFixedPoint
    description: "Top-left and bottom-right corner points."
  - name: ARect
    type: TRect
    description: "Source integer rectangle to convert."
  - name: FR
    type: TFloatRect
    description: "Source floating-point rectangle to convert."
returns:
  - type: TFixedRect
    description: "Constructed fixed-point rectangle structure."
seealso:
  - "[[TFixedRect]]"
  - "[[MakeRect]]"
  - "[[FloatRect]]"
---

## Description

The `FixedRect` functions construct 16.16 fixed-point [[TFixedRect]] structures from individual fixed-point coordinates, corner point pairs, integer `TRect` structures, or floating-point [[TFloatRect]] structures.

## Variants

| Signature | Input Source | Description |
| --- | --- | --- |
| `FixedRect(L, T, R, B: TFixed)` | Fixed-point coordinates | Direct construction from 16.16 fixed-point values. |
| `FixedRect(TopLeft, BottomRight: TFixedPoint)` | Point pair | Constructs rectangle from top-left and bottom-right points. |
| `FixedRect(ARect: TRect)` | Integer `TRect` | Converts integer pixel coordinates to 16.16 fixed-point format (shifted left by 16 bits). |
| `FixedRect(FR: TFloatRect)` | Floating-point `TFloatRect` | Converts floating-point coordinates to 16.16 fixed-point format (scaled by 65536). |
