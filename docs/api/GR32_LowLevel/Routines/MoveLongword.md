---
layout: doc
docType: api
unit: GR32_LowLevel
entity: MoveLongword
kind: Procedure
declaration: "procedure MoveLongword(const Source; var Dest; Count: Integer);"
summary: "Copies a block of 32-bit values from Source memory to Dest memory."
parameters:
  - name: Source
    type: const
    description: "Reference to the source memory block."
  - name: Dest
    type: var
    description: "Reference to the destination memory block."
  - name: Count
    type: Integer
    description: "Number of 32-bit (4-byte) elements to copy."
seealso:
  - "[[MoveWord]]"
  - "[[FillLongword]]"
---

## Description

`MoveLongword` copies `Count` 32-bit items (4 bytes each) from `Source` to `Dest`. It is optimized for 32-bit aligned memory copies such as pixel buffer transfers and scanline copies.
