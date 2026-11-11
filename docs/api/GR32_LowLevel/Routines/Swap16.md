---
layout: doc
docType: api
unit: GR32_LowLevel
entity: Swap16
aliases: [Swap32, Swap64]
kind: Function
summary: "Converts endianness by swapping byte order of 16-bit, 32-bit, or 64-bit integer values."
overloads:
  - signature: "function Swap16(Value: Word): Word;"
    summary: "Swaps byte order of a 16-bit Word value."
    parameters:
      - name: Value
        type: Word
        description: "Input 16-bit word."
    returns:
      - type: Word
        description: "16-bit word with swapped byte order."

  - signature: "function Swap32(Value: Cardinal): Cardinal; overload;"
    summary: "Swaps byte order of a 32-bit Cardinal value."
    parameters:
      - name: Value
        type: Cardinal
        description: "Input 32-bit cardinal."
    returns:
      - type: Cardinal
        description: "32-bit cardinal with swapped byte order."

  - signature: "function Swap64(Value: Int64): Int64;"
    summary: "Swaps byte order of a 64-bit Int64 value."
    parameters:
      - name: Value
        type: Int64
        description: "Input 64-bit integer."
    returns:
      - type: Int64
        description: "64-bit integer with swapped byte order."
seealso:
  - "[[Swap]]"
---

## Description

`Swap16`, `Swap32`, and `Swap64` convert values between little-endian and big-endian representations by reversing their byte order.
