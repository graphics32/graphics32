---
layout: doc
docType: api
unit: GR32
entity: FloatRect
kind: Function
declaration: |
  function FloatRect(const L, T, R, B: TFloat): TFloatRect; overload;
  function FloatRect(const TopLeft, BottomRight: TFloatPoint): TFloatRect; overload;
  function FloatRect(const ARect: TRect): TFloatRect; overload;
  function FloatRect(const FXR: TFixedRect): TFloatRect; overload;
summary: "Constructs a TFloatRect structure in floating-point precision from individual coordinates, point pairs, integer rectangles, or fixed-point rectangles."
parameters:
  - name: L, T, R, B
    type: TFloat
    description: "Left, top, right, and bottom floating-point coordinates."
  - name: TopLeft, BottomRight
    type: TFloatPoint
    description: "Top-left and bottom-right corner points."
  - name: ARect
    type: TRect
    description: "Source integer rectangle to convert."
  - name: FXR
    type: TFixedRect
    description: "Source fixed-point rectangle to convert."
returns:
  - type: TFloatRect
    description: "Constructed floating-point rectangle structure."
seealso:
  - "[[TFloatRect]]"
  - "[[MakeRect]]"
  - "[[FixedRect]]"
---

## Description

The `FloatRect` functions construct single-precision floating-point [[TFloatRect]] structures from individual floating-point coordinates, corner point pairs, integer `TRect` structures, or fixed-point [[TFixedRect]] structures.

## Variants

| Signature | Input Source | Description |
| --- | --- | --- |
| `FloatRect(L, T, R, B: TFloat)` | Floating-point coordinates | Direct construction from floating-point values. |
| `FloatRect(TopLeft, BottomRight: TFloatPoint)` | Point pair | Constructs rectangle from top-left and bottom-right points. |
| `FloatRect(ARect: TRect)` | Integer `TRect` | Converts integer pixel coordinates to floating-point representation. |
| `FloatRect(FXR: TFixedRect)` | Fixed-point `TFixedRect` | Converts 16.16 fixed-point coordinates to floating-point representation. |
