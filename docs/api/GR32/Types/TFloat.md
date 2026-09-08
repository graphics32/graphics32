---
layout: doc
docType: api
unit: GR32
entity: TFloat
kind: Type
summary: "32-bit single-precision floating point number."
declaration: |
  type
    TFloat = Single;
    PFloat = ^TFloat;
aliases: [PFloat, PFloatArray, TFloatArray, TArrayOfFloat, TArrayOfArrayOfFloat]
---

## Description

`TFloat` is a type alias for `Single` and is used in Graphics32 for historical reasons.

::: info
The original purpose of `TFloat` was to be able to switch between single- and double-precision by simply redeclaring `TFloat` as either `Single` or `Double`.<br>
However, while this would work most of the time for pure Pascal code, it would in most cases require that all assembler code be duplicated with different versions for the two types.

Today, you can think of `TFloat` simply as `Single`. All existing code relies on it and it will not change.
:::

## Related Types & Arrays

| Type | Declaration | Description |
| --- | --- | --- |
| `PFloat` | `^TFloat` | Pointer to a `TFloat` value. |
| `TFloatArray` | `array [0..0] of TFloat` | Static un-sized array type. |
| `PFloatArray` | `^TFloatArray` | Pointer to an un-sized array of Single values. |
| `TArrayOfFloat` | `array of TFloat` | Dynamic array of Single values. |
