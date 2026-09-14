---
layout: doc
docType: api
unit: GR32_Blend
entity: MulDiv255Table
aliases: [DivMul255Table]
kind: Variable
declaration: |
  var MulDiv255Table: TLUT88;
  var DivMul255Table: TLUT88;
summary: "Precalculated 256x256 8-bit multiplication and division lookup tables for fast alpha compositing."
seealso:
  - "[[TLUT8]]"
---

## Description

`MulDiv255Table` and `DivMul255Table` are precalculated $256 \times 256$ byte lookup tables used in alpha compositing calculations:

- `MulDiv255Table[a, b] = Round(a * b / 255)`: Used for fast 8-bit multiplication and scaling without division.
- `DivMul255Table[a, b] = Round(b * 255 / a)`: Used for associative alpha merging division ($R_c = F_c / R_a$) and unpremultiplication.
