---
layout: doc
docType: api
unit: GR32_LowLevel
entity: Clamp
kind: Function
summary: "Clamps an integer value to a specified range."
overloads:
  - signature: "function Clamp(const Value: Integer): Integer; overload;"
    summary: "Clamps an integer value to byte range [0..255]."
    parameters:
      - name: Value
        type: Integer
        description: "Integer value to clamp."
    returns:
      - type: Integer
        description: "Value clamped to range [0..255]."

  - signature: "function Clamp(Value, Max: Integer): Integer; overload;"
    summary: "Clamps an integer value to range [0..Max]."
    parameters:
      - name: Value
        type: Integer
        description: "Integer value to clamp."
      - name: Max
        type: Integer
        description: "Maximum upper boundary."
    returns:
      - type: Integer
        description: "Value clamped to range [0..Max]."

  - signature: "function Clamp(Value, Min, Max: Integer): Integer; overload;"
    summary: "Clamps an integer value to range [Min..Max]."
    parameters:
      - name: Value
        type: Integer
        description: "Integer value to clamp."
      - name: Min
        type: Integer
        description: "Minimum lower boundary."
      - name: Max
        type: Integer
        description: "Maximum upper boundary."
    returns:
      - type: Integer
        description: "Value clamped to range [Min..Max]."
seealso:
  - "[[Constrain]]"
  - "[[Wrap]]"
  - "[[Mirror]]"
  - "[[Reflect]]"
---

## Description

`Clamp` restricts an integer value to fit within a specified range.

- The single-parameter overload `Clamp(Value)` clamps to byte channel range `[0..255]`.
- The two-parameter overload `Clamp(Value, Max)` clamps to `[0..Max]`.
- The three-parameter overload `Clamp(Value, Min, Max)` clamps to `[Min..Max]`.

![](/images/plot-clamp.svg)