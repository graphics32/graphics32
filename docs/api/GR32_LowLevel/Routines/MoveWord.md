---
layout: doc
docType: api
unit: GR32_LowLevel
entity: MoveWord
kind: Procedure
declaration: "procedure MoveWord(const Source; var Dest; Count: Integer);"
summary: "Copies a block of 16-bit values from Source memory to Dest memory."
parameters:
  - name: Source
    type: const
    description: "Reference to the source memory block."
  - name: Dest
    type: var
    description: "Reference to the destination memory block."
  - name: Count
    type: Integer
    description: "Number of 16-bit (2-byte) elements to copy."
seealso:
  - "[[MoveLongword]]"
  - "[[FillWord]]"
---

## Description

`MoveWord` copies `Count` 16-bit items (2 bytes each) from `Source` memory to `Dest` memory.
