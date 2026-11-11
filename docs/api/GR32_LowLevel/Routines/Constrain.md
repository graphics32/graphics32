---
layout: doc
docType: api
unit: GR32_LowLevel
entity: Constrain
kind: Function
summary: "Constrains a numeric value to a specified lower and upper boundary range."
overloads:
  - signature: "function Constrain(const Value, Lo, Hi: Integer): Integer; overload;"
    summary: "Constrains an Integer value to [Lo..Hi]."
    parameters:
      - name: Value
        type: Integer
        description: "The integer value to constrain."
      - name: Lo
        type: Integer
        description: "Lower boundary."
      - name: Hi
        type: Integer
        description: "Upper boundary."
    returns:
      - type: Integer
        description: "Value clamped to range [Lo..Hi]."

  - signature: "function Constrain(const Value, Lo, Hi: Single): Single; overload;"
    summary: "Constrains a Single floating-point value to [Lo..Hi]."
    parameters:
      - name: Value
        type: Single
        description: "The floating-point value to constrain."
      - name: Lo
        type: Single
        description: "Lower boundary."
      - name: Hi
        type: Single
        description: "Upper boundary."
    returns:
      - type: Single
        description: "Value clamped to range [Lo..Hi]."
seealso:
  - "[[Clamp]]"
  - "[[SwapConstrain]]"
---

## Description

`Constrain` restricts `Value` to lie within the closed range `[Lo..Hi]`. If `Value` is less than `Lo`, `Lo` is returned; if `Value` is greater than `Hi`, `Hi` is returned; otherwise `Value` is returned unchanged.
