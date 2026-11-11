---
layout: doc
docType: api
unit: GR32_Transforms
entity: Matrix Types
kind: Type
aliases: [TFloatMatrix, TFixedMatrix, IdentityMatrix]
summary: "Defines 3x3 transformation matrix representations and identity constants."
---

## Description

The `GR32_Transforms` unit uses $3 \times 3$ matrices to represent 2D homogeneous coordinate transformations (translation, rotation, scaling, skewing, and projective mapping).

- `TFloatMatrix`: $3 \times 3$ matrix using 32-bit floating-point precision (`TFloat`).
- `TFixedMatrix`: $3 \times 3$ matrix using 16.16 fixed-point precision (`TFixed`).
- `IdentityMatrix`: $3 \times 3$ constant identity matrix ($M_{i,i} = 1$, $M_{i,j} = 0$).

## Types & Constants

| Identifier | Type / Declaration | Description |
| --- | --- | --- |
| `TFloatMatrix` | `array [0..2, 0..2] of TFloat;` | $3 \times 3$ floating-point transformation matrix. |
| `TFixedMatrix` | `array [0..2, 0..2] of TFixed;` | $3 \times 3$ fixed-point transformation matrix. |
| `IdentityMatrix` | `const IdentityMatrix: TFloatMatrix` | Constant $3 \times 3$ floating-point identity matrix. |
