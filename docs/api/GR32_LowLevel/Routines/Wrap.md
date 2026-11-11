---
layout: doc
docType: api
unit: GR32_LowLevel
entity: Wrap
kind: Function
summary: "Constrains a coordinate or scalar value to a range using modular wrap-around indexing."
overloads:
  - signature: "function Wrap(Value, Max: Integer): Integer; overload;"
    summary: "Wraps an integer value to closed range [0..Max]."
    parameters:
      - name: Value
        type: Integer
        description: "Integer value to wrap."
      - name: Max
        type: Integer
        description: "Inclusive upper bound."
    returns:
      - type: Integer
        description: "Wrapped integer within range [0..Max]."

  - signature: "function Wrap(Value, Min, Max: Integer): Integer; overload;"
    summary: "Wraps an integer value to closed range [Min..Max]."
    parameters:
      - name: Value
        type: Integer
        description: "Integer value to wrap."
      - name: Min
        type: Integer
        description: "Inclusive lower bound."
      - name: Max
        type: Integer
        description: "Inclusive upper bound (Min <= Max)."
    returns:
      - type: Integer
        description: "Wrapped integer within range [Min..Max]."

  - signature: "function Wrap(Value, Max: Single): Single; overload;"
    summary: "Wraps a floating-point single value to half-open range [0..Max)."
    parameters:
      - name: Value
        type: Single
        description: "Floating-point value to wrap."
      - name: Max
        type: Single
        description: "Exclusive upper bound."
    returns:
      - type: Single
        description: "Wrapped floating-point value within range [0..Max)."
seealso:
  - "[[WrapMem]]"
  - "[[WrapPow2]]"
  - "[[Clamp]]"
  - "[[Mirror]]"
  - "[[Reflect]]"
---

## Description

`Wrap` maps an arbitrary value into a periodic range using modular arithmetic wrap-around. It is commonly used for repeating textures, tileable bitmap sampling, and cyclical coordinate lookups.

::: half
![](/images/plot-wrap.svg)
:::
::: half
![](/images/plot-wrapfloat.svg)
:::