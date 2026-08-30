---
layout: doc
docType: api
unit: GR32_VectorMaps
entity: Vector Types
kind: Type
aliases: [TFixedVector, TFloatVector, PFixedVector, PFloatVector, TArrayOfFixedVector, PArrayOfFixedVector, TArrayOfFloatVector, PArrayOfFloatVector]
summary: "2D displacement vector types, pointers, and dynamic arrays in fixed and floating point precision."
---

## Description

The `GR32_VectorMaps` unit defines 2D displacement vector structures in both 16.16 fixed-point (`TFixedVector`) and single-precision floating-point (`TFloatVector`) formats, along with their pointer and dynamic array type aliases.

`TFixedVector` is equivalent to `TFixedPoint`, and `TFloatVector` is equivalent to `TFloatPoint`.

## Types

| Type Identifier | Declaration / Base Type | Description |
| --- | --- | --- |
| `TFixedVector` | `TFixedPoint` | 2D vector in 16.16 fixed-point precision. |
| `PFixedVector` | `^TFixedVector` | Pointer to a `TFixedVector` structure. |
| `TFloatVector` | `TFloatPoint` | 2D vector in floating-point precision. |
| `PFloatVector` | `^TFloatVector` | Pointer to a `TFloatVector` structure. |
| `TArrayOfFixedVector` | `array of TFixedVector` | Dynamic array of fixed-point 2D vectors. |
| `PArrayOfFixedVector` | `^TArrayOfFixedVector` | Pointer to a `TArrayOfFixedVector` array. |
| `TArrayOfFloatVector` | `array of TFloatVector` | Dynamic array of floating-point 2D vectors. |
| `PArrayOfFloatVector` | `^TArrayOfFixedVector` | Pointer to a `TArrayOfFloatVector` array. |
