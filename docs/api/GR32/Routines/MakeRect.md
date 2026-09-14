---
layout: doc
docType: api
unit: GR32
entity: MakeRect
kind: Function
declaration: |
  function MakeRect(const L, T, R, B: Integer): TRect; overload;
  function MakeRect(const L, T, R, B: TFloat; Rounding: TRectRounding = rrClosest): TRect; overload;
  function MakeRect(const FR: TFloatRect; Rounding: TRectRounding = rrClosest): TRect; overload;
  function MakeRect(const FXR: TFixedRect; Rounding: TRectRounding = rrClosest): TRect; overload;
summary: "Constructs an integer TRect structure from explicit coordinates, floating-point bounds, or fixed-point bounds with optional rounding."
parameters:
  - name: L, T, R, B
    type: "-"
    description: "Left, top, right, and bottom boundary coordinates."
  - name: FR
    type: TFloatRect
    description: "Source floating-point rectangle."
  - name: FXR
    type: TFixedRect
    description: "Source fixed-point rectangle."
  - name: Rounding
    type: TRectRounding
    description: "Rounding mode strategy (rrClosest, rrOutside, rrInside, rrLine)."
returns:
  - type: TRect
    description: "Constructed integer rectangle structure."
seealso:
  - "[[TRectRounding]]"
  - "[[FixedRect]]"
  - "[[FloatRect]]"
---

## Description

The `MakeRect` functions construct integer `TRect` structures from individual coordinates or existing coordinate structures.

When constructing from floating-point (`TFloat`, [[TFloatRect]]) or fixed-point ([[TFixedRect]]) coordinates, the `Rounding` parameter specifies how fractional coordinate boundaries are mapped to integer pixel bounds.

## Variants

| Signature | Input Source | Rounding |
| --- | --- | --- |
| `MakeRect(L, T, R, B: Integer)` | Integer coordinates | Direct conversion |
| `MakeRect(L, T, R, B: TFloat; Rounding)` | Floating-point coordinates | Applies specified `Rounding` mode |
| `MakeRect(FR: TFloatRect; Rounding)` | Floating-point rectangle | Applies specified `Rounding` mode |
| `MakeRect(FXR: TFixedRect; Rounding)` | Fixed-point rectangle | Applies specified `Rounding` mode |
