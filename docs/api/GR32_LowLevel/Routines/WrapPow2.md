---
layout: doc
docType: api
unit: GR32_LowLevel
entity: WrapPow2
kind: Function
summary: "Fast bitwise wrap alternative for ranges where range length is a power of two."
overloads:
  - signature: "function WrapPow2(Value, Max: Integer): Integer; overload;"
    summary: "Wraps an integer value to range [0..Max] where Max+1 is a power of two."
    parameters:
      - name: Value
        type: Integer
        description: "Integer value to wrap."
      - name: Max
        type: Integer
        description: "Inclusive upper bound (where Max+1 is a power of 2, e.g. 3, 7, 15, 255)."
    returns:
      - type: Integer
        description: "Wrapped integer within range [0..Max]."

  - signature: "function WrapPow2(Value, Min, Max: Integer): Integer; overload;"
    summary: "Wraps an integer value to range [Min..Max] where Max-Min+1 is a power of two."
    parameters:
      - name: Value
        type: Integer
        description: "Integer value to wrap."
      - name: Min
        type: Integer
        description: "Inclusive lower bound."
      - name: Max
        type: Integer
        description: "Inclusive upper bound (where Max-Min+1 is a power of 2)."
    returns:
      - type: Integer
        description: "Wrapped integer within range [Min..Max]."
seealso:
  - "[[Wrap]]"
  - "[[ReflectPow2]]"
---

## Description

`WrapPow2` provides accelerated bitwise wrapping for power-of-two dimensions (e.g., textures of width/height 4, 8, 16, 32, 64, 128, 256, 512, 1024). It replaces division/modulo operations with fast bitwise AND masking (`Value and Max`).
