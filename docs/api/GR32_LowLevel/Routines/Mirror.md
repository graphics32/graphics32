---
layout: doc
docType: api
unit: GR32_LowLevel
entity: Mirror
kind: Function
summary: "Constrains an integer coordinate to a range with center-pixel symmetry mirroring."
overloads:
  - signature: "function Mirror(Value, Max: Integer): Integer; overload;"
    summary: "Mirrors an integer value in range [0..Max]."
    parameters:
      - name: Value
        type: Integer
        description: "Integer value to mirror."
      - name: Max
        type: Integer
        description: "Inclusive upper bound."
    returns:
      - type: Integer
        description: "Mirrored integer coordinate in range [0..Max]."

  - signature: "function Mirror(Value, Min, Max: Integer): Integer; overload;"
    summary: "Mirrors an integer value in range [Min..Max]."
    parameters:
      - name: Value
        type: Integer
        description: "Integer value to mirror."
      - name: Min
        type: Integer
        description: "Inclusive lower bound."
      - name: Max
        type: Integer
        description: "Inclusive upper bound."
    returns:
      - type: Integer
        description: "Mirrored integer coordinate in range [Min..Max]."
seealso:
  - "[[Reflect]]"
  - "[[Clamp]]"
  - "[[Wrap]]"
---

## Description

`Mirror` constrains `Value` to range `[0..Max]` (or `[Min..Max]`) using center-pixel mirror symmetry (with cycle length $2 \times \text{Max}$).

For example, for `Max = 3`, the sequence of values generated across coordinate space is: `0, 1, 2, 3, 2, 1, 0, 1, 2, 3, 2, 1`.

![](/images/plot-mirror.svg)