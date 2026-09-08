---
layout: doc
docType: api
unit: GR32_LowLevel
entity: FillWord
kind: Procedure
declaration: "procedure FillWord(var X; Count: Cardinal; Value: Longword);"
summary: "Fills a contiguous memory block with a specified 16-bit Word value."
parameters:
  - name: X
    type: var
    description: "The starting destination memory location to fill."
  - name: Count
    type: Cardinal
    description: "Number of 16-bit Word elements (2-byte blocks) to write."
  - name: Value
    type: Longword
    description: "16-bit value stored in the lower word of Value to write into each element."
seealso:
  - "[[FillLongword]]"
  - "[[MoveWord]]"
---

## Description

`FillWord` fills a block of memory with 16-bit `Word` values. The lower 16 bits of `Value` are copied repeatedly into `Count` consecutive 16-bit word slots starting at memory location `X`.
