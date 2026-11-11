---
layout: doc
docType: api
unit: GR32_Transforms
entity: Vector Types
kind: Type
aliases: [TVector3f, TVector3i]
summary: "Defines 3-element vector arrays used in 2D homogeneous coordinate calculations."
---

## Description

The 3-element vector types represent 2D points or direction vectors in homogeneous coordinate space $[X, Y, W]$.

- `TVector3f`: Floating-point 3-element vector (`TFloat`).
- `TVector3i`: Integer 3-element vector (`Integer`).

## Types

| Identifier | Type / Declaration | Description |
| --- | --- | --- |
| `TVector3f` | `array [0..2] of TFloat;` | 3-element floating-point vector $[X, Y, W]$. |
| `TVector3i` | `array [0..2] of Integer;` | 3-element integer vector $[X, Y, W]$. |
