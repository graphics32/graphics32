---
layout: doc
docType: api
unit: GR32_LowLevel
entity: SAR
aliases: [SAR_3, SAR_4, SAR_6, SAR_8, SAR_9, SAR_11, SAR_12, SAR_13, SAR_14, SAR_15, SAR_16]
kind: Function
summary: "Arithmetic shift right with sign preservation."
overloads:
  - signature: "function SAR_3(Value: Integer): Integer;"
    summary: "Arithmetically shifts Value right by 3 bits (Value div 8 with negative sign conservation)."
    parameters:
      - name: Value
        type: Integer
        description: "Integer value to shift."
    returns:
      - type: Integer
        description: "Value shifted right by 3 bits."

  - signature: "function SAR_4(Value: Integer): Integer;"
    summary: "Arithmetically shifts Value right by 4 bits."
    parameters:
      - name: Value
        type: Integer
        description: "Integer value to shift."
    returns:
      - type: Integer
        description: "Value shifted right by 4 bits."

  - signature: "function SAR_6(Value: Integer): Integer;"
    summary: "Arithmetically shifts Value right by 6 bits."
    parameters:
      - name: Value
        type: Integer
        description: "Integer value to shift."
    returns:
      - type: Integer
        description: "Value shifted right by 6 bits."

  - signature: "function SAR_8(Value: Integer): Integer;"
    summary: "Arithmetically shifts Value right by 8 bits."
    parameters:
      - name: Value
        type: Integer
        description: "Integer value to shift."
    returns:
      - type: Integer
        description: "Value shifted right by 8 bits."

  - signature: "function SAR_9(Value: Integer): Integer;"
    summary: "Arithmetically shifts Value right by 9 bits."
    parameters:
      - name: Value
        type: Integer
        description: "Integer value to shift."
    returns:
      - type: Integer
        description: "Value shifted right by 9 bits."

  - signature: "function SAR_11(Value: Integer): Integer;"
    summary: "Arithmetically shifts Value right by 11 bits."
    parameters:
      - name: Value
        type: Integer
        description: "Integer value to shift."
    returns:
      - type: Integer
        description: "Value shifted right by 11 bits."

  - signature: "function SAR_12(Value: Integer): Integer;"
    summary: "Arithmetically shifts Value right by 12 bits."
    parameters:
      - name: Value
        type: Integer
        description: "Integer value to shift."
    returns:
      - type: Integer
        description: "Value shifted right by 12 bits."

  - signature: "function SAR_13(Value: Integer): Integer;"
    summary: "Arithmetically shifts Value right by 13 bits."
    parameters:
      - name: Value
        type: Integer
        description: "Integer value to shift."
    returns:
      - type: Integer
        description: "Value shifted right by 13 bits."

  - signature: "function SAR_14(Value: Integer): Integer;"
    summary: "Arithmetically shifts Value right by 14 bits."
    parameters:
      - name: Value
        type: Integer
        description: "Integer value to shift."
    returns:
      - type: Integer
        description: "Value shifted right by 14 bits."

  - signature: "function SAR_15(Value: Integer): Integer;"
    summary: "Arithmetically shifts Value right by 15 bits."
    parameters:
      - name: Value
        type: Integer
        description: "Integer value to shift."
    returns:
      - type: Integer
        description: "Value shifted right by 15 bits."

  - signature: "function SAR_16(Value: Integer): Integer;"
    summary: "Arithmetically shifts Value right by 16 bits."
    parameters:
      - name: Value
        type: Integer
        description: "Integer value to shift."
    returns:
      - type: Integer
        description: "Value shifted right by 16 bits."
---

## Description

The `SAR_n` functions perform an arithmetic shift right by $n$ bits (`SAR_3` through `SAR_16`).

Unlike standard integer division (`x div 2^n`), which truncates towards zero and introduces asymmetry for negative numbers, `SAR_n` preserves two's-complement sign extension consistent with CPU `SAR` assembly instructions across positive and negative values.
