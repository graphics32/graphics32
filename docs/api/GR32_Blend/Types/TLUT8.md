---
layout: doc
docType: api
unit: GR32_Blend
entity: TLUT8
aliases: [PLUT8, TLUT88]
kind: Type
declaration: |
  TLUT8 = array[Byte] of Byte;
  PLUT8 = ^TLUT8;
  TLUT88 = array[Byte] of TLUT8;
summary: "Byte lookup table structures for accelerated 8-bit arithmetic and alpha compositing calculations."
seealso:
  - "[[DivMul255Table]]"
  - "[[MulDiv255Table]]"
---

## Description

`TLUT8` represents a 256-element byte lookup table. `TLUT88` represents a $256 \times 256$ matrix of byte lookup values.

These structures form the foundation for precalculated mathematical lookup tables in Graphics32 (such as `MulDiv255Table` and `DivMul255Table`) that replace costly 8-bit division and multiplication operations in blending and compositing loops with direct table lookups.
