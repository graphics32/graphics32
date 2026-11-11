---
layout: doc
docType: api
unit: GR32_LowLevel
entity: Swap
aliases: [Swap32]
kind: Procedure
summary: "Exchanges two values in-place."
overloads:
  - signature: "procedure Swap(var A, B: Pointer); overload;"
    summary: "Exchanges two Pointer values."
    parameters:
      - name: A, B
        type: Pointer
        description: "Variables whose pointer values will be swapped."

  - signature: "procedure Swap(var A, B: Integer); overload;"
    summary: "Exchanges two Integer values."
    parameters:
      - name: A, B
        type: Integer
        description: "Variables whose integer values will be swapped."

  - signature: "procedure Swap(var A, B: TFixed); overload;"
    summary: "Exchanges two TFixed values."
    parameters:
      - name: A, B
        type: TFixed
        description: "Variables whose fixed-point values will be swapped."

  - signature: "procedure Swap(var A, B: TColor32); overload;"
    summary: "Exchanges two TColor32 values."
    parameters:
      - name: A, B
        type: TColor32
        description: "Variables whose color values will be swapped."

  - signature: "procedure Swap32(var A, B); overload;"
    summary: "Exchanges two untyped 32-bit values."
    parameters:
      - name: A, B
        type: var
        description: "Untyped variables (32-bit wide) to exchange."
seealso:
  - "[[TestSwap]]"
  - "[[Swap16]]"
---

## Description

`Swap` exchanges the values of variables `A` and `B` in place.
