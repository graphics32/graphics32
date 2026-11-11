---
layout: doc
docType: api
unit: GR32_Blend
entity: TMultTable
aliases: [TMultEntry, PMultEntry, PMultTable]
kind: Type
declaration: |
  TMultEntry = array[0..3] of TColor32Entry;
  PMultEntry = ^TMultEntry;
  TMultTable = array[Byte] of TMultEntry;
  PMultTable = ^TMultTable;
summary: "16-byte aligned vector lookup table structures used for SIMD vectorized division by 255."
seealso:
  - "[[alpha_ptr]]"
  - "[[bias_ptr]]"
---

## Description

`TMultTable` is a 256-entry lookup table where each entry contains 4 `TColor32Entry` records.

The table is used in SIMD (SSE2/AVX) vectorized blending routines to perform fast integer division by 255:
$$\text{Value} \div 255 = (\text{Value} + 128) \gg 8$$
The memory is 16-byte aligned so vector instructions can read quadword blocks directly.
