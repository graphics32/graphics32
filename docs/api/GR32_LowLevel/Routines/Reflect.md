---
layout: doc
docType: api
unit: GR32_LowLevel
entity: Reflect
kind: Function
summary: "Constrains an integer coordinate to a range with edge-pixel reflection symmetry."
overloads:
  - signature: "function Reflect(Value, Max: Integer): Integer; overload;"
    summary: "Reflects an integer value in range [0..Max]."
    parameters:
      - name: Value
        type: Integer
        description: "Integer value to reflect."
      - name: Max
        type: Integer
        description: "Inclusive upper bound."
    returns:
      - type: Integer
        description: "Reflected integer coordinate in range [0..Max]."

  - signature: "function Reflect(Value, Min, Max: Integer): Integer; overload;"
    summary: "Reflects an integer value in range [Min..Max]."
    parameters:
      - name: Value
        type: Integer
        description: "Integer value to reflect."
      - name: Min
        type: Integer
        description: "Inclusive lower bound."
      - name: Max
        type: Integer
        description: "Inclusive upper bound."
    returns:
      - type: Integer
        description: "Reflected integer coordinate in range [Min..Max]."
seealso:
  - "[[ReflectPow2]]"
  - "[[Mirror]]"
  - "[[Wrap]]"
  - "[[Clamp]]"
---

## Description

`Reflect` constrains `Value` to range `[0..Max]` (or `[Min..Max]`) using symmetry around the outer edge of boundary pixels (cycle length $2 \times (\text{Max}+1)$).

For example, for `Max = 3`, the sequence of values generated across coordinate space is: `0, 1, 2, 3, 3, 2, 1, 0, 0, 1, 2, 3, 3, 2, 1, 0`.

![](/images/plot-reflect.svg)