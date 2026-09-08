---
layout: doc
docType: api
unit: GR32_LowLevel
entity: ReflectPow2
kind: Function
summary: "Fast bitwise reflection alternative for ranges where range length is a power of two."
overloads:
  - signature: "function ReflectPow2(Value, Max: Integer): Integer; overload;"
    summary: "Reflects an integer value in range [0..Max] where Max+1 is a power of two."
    parameters:
      - name: Value
        type: Integer
        description: "Integer value to reflect."
      - name: Max
        type: Integer
        description: "Inclusive upper bound (where Max+1 is a power of 2)."
    returns:
      - type: Integer
        description: "Reflected integer coordinate in range [0..Max]."

  - signature: "function ReflectPow2(Value, Min, Max: Integer): Integer; overload;"
    summary: "Reflects an integer value in range [Min..Max] where Max-Min+1 is a power of two."
    parameters:
      - name: Value
        type: Integer
        description: "Integer value to reflect."
      - name: Min
        type: Integer
        description: "Inclusive lower bound."
      - name: Max
        type: Integer
        description: "Inclusive upper bound (where Max-Min+1 is a power of 2)."
    returns:
      - type: Integer
        description: "Reflected integer coordinate in range [Min..Max]."
seealso:
  - "[[Reflect]]"
  - "[[WrapPow2]]"
---

## Description

`ReflectPow2` provides optimized power-of-two reflection by utilizing bitwise operations instead of division and modulo instructions.
