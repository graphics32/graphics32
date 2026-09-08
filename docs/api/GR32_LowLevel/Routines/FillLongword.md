---
layout: doc
docType: api
unit: GR32_LowLevel
entity: FillLongword
kind: Procedure
declaration: "procedure FillLongword(var X; Count: Cardinal; Value: Longword);"
summary: "Fills a contiguous memory block with a specified 32-bit Longword value."
parameters:
  - name: X
    type: var
    description: "The starting destination memory location to fill."
  - name: Count
    type: Cardinal
    description: "Number of 32-bit Longword elements (4-byte blocks) to write."
  - name: Value
    type: Longword
    description: "32-bit value (e.g. TColor32 pixel value) to write into each element."
seealso:
  - "[[FillWord]]"
  - "[[MoveLongword]]"
---

## Description

`FillLongword` fills a block of memory with 32-bit values. It operates analogously to System.FillChar, but writes 32-bit `Longword` units instead of 8-byte `Byte` units.

In Graphics32, `FillLongword` is widely used for rapidly clearing or setting solid background colors across 32-bit pixel buffers (such as bitmap scanlines or entire pixel arrays).

Although implemented internally as a function delegate bound to the fastest CPU implementation (Pascal, x86/x64 assembly, or SSE2), it is used and documented as a standard procedure.
