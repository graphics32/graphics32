---
layout: doc
docType: api
unit: GR32_ColorGradients
entity: TColor32LookupTable
kind: Class
summary: "Manages a fast 32-bit ARGB color lookup table buffer indexed by bit-shift power sizes."
declaration: "TColor32LookupTable = class(TPersistent)"
inheritance:
  - TObject
  - TPersistent
  - TColor32LookupTable
---

## Description

`TColor32LookupTable` provides high-performance color table lookup capabilities for gradient rendering.

The size of the table is determined by the `Order` property, where `Size = 2^Order` (for example, `Order = 8` yields 256 colors; default `Order = 9` yields 512 colors). Fast bitwise index wrapping is achieved using `Mask = Size - 1`.

[members]
