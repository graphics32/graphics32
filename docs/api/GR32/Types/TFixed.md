---
layout: doc
docType: api
unit: GR32
entity: TFixed
kind: Type
summary: "32-bit signed integer type representing numbers in 16.16 fixed-point precision."
declaration: "type TFixed = type Integer;"
---

## Description

`TFixed` is a 32-bit signed integer type holding fixed-point values with 16 bits of integer precision and 16 bits of fractional precision (16.16 format).

## Related Types & Arrays

| Type | Declaration | Description |
| --- | --- | --- |
| `PFixed` | `^TFixed` | Pointer to a `TFixed` value. |
| `PFixedArray` | `^TFixedArray` | Pointer to an un-sized array of fixed-point values. |
| `TFixedArray` | `array [0..0] of TFixed` | Static un-sized array type. |
| `TArrayOfFixed` | `array of TFixed` | Dynamic array of 16.16 fixed-point values. |
| `TArrayOfArrayOfFixed` | `array of TArrayOfFixed` | 2D dynamic array of fixed-point values. |
