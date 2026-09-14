---
layout: doc
docType: api
unit: GR32_Blend
entity: alpha_ptr
aliases: [bias_ptr]
kind: Variable
declaration: |
  var alpha_ptr: PMultTable;
  var bias_ptr: PMultEntry;
summary: "Pointers to 16-byte aligned vector lookup tables used for SIMD fast division by 255."
seealso:
  - "[[TMultTable]]"
---

## Description

`alpha_ptr` points to a 16-byte aligned array of 256 vector lookup entries (`TMultTable`).

`bias_ptr` points to the middle entry (`alpha_ptr[128]`) containing rounding bias values (`$80, $00, $80, $00`).

These pointers are used by SSE2 SIMD blending routines to perform parallel division of 4 packed byte values by 255:
$$\text{Value} \div 255 = ((\text{alpha\_ptr}[\text{Value}] + \text{bias\_ptr}^\wedge) \gg 8)$$
