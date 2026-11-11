---
layout: doc
docType: api
unit: GR32_LowLevel
entity: Fast Rounding Functions
aliases: [FastFloor, FastCeil, FastTrunc, FastRound, FastFloorSingle, FastFloorDouble, FastCeilSingle, FastCeilDouble]
kind: Function
summary: "High-performance alternatives to standard RTL Round, Trunc, Floor, and Ceil operations."
overloads:
  - signature: "function FastFloor(Value: TFloat): Integer; overload;"
    summary: "Computes the greatest integer less than or equal to Value (Single precision)."
    parameters:
      - name: Value
        type: TFloat
        description: "Floating-point single value."
    returns:
      - type: Integer
        description: "Floor of Value as an Integer."

  - signature: "function FastFloor(Value: Double): Integer; overload;"
    summary: "Computes the greatest integer less than or equal to Value (Double precision)."
    parameters:
      - name: Value
        type: Double
        description: "Floating-point double value."
    returns:
      - type: Integer
        description: "Floor of Value as an Integer."

  - signature: "function FastCeil(Value: TFloat): Integer; overload;"
    summary: "Computes the smallest integer greater than or equal to Value (Single precision)."
    parameters:
      - name: Value
        type: TFloat
        description: "Floating-point single value."
    returns:
      - type: Integer
        description: "Ceil of Value as an Integer."

  - signature: "function FastCeil(Value: Double): Integer; overload;"
    summary: "Computes the smallest integer greater than or equal to Value (Double precision)."
    parameters:
      - name: Value
        type: Double
        description: "Ceil of Value as an Integer."

  - signature: "function FastTrunc(Value: TFloat): Integer;"
    summary: "Truncates floating-point Value to Integer towards zero."
    parameters:
      - name: Value
        type: TFloat
        description: "Floating-point value."
    returns:
      - type: Integer
        description: "Truncated integer."

  - signature: "function FastRound(Value: TFloat): Integer;"
    summary: "Rounds floating-point Value to the nearest Integer."
    parameters:
      - name: Value
        type: TFloat
        description: "Floating-point value."
    returns:
      - type: Integer
        description: "Nearest integer value."

  - signature: "function FastFloorSingle(Value: TFloat): Integer;"
    summary: "Fast floor operation for Single precision."
    parameters:
      - name: Value
        type: TFloat
        description: "Single-precision float value."
    returns:
      - type: Integer
        description: "Floor integer."

  - signature: "function FastFloorDouble(Value: Double): Integer;"
    summary: "Fast floor operation for Double precision."
    parameters:
      - name: Value
        type: Double
        description: "Double-precision float value."
    returns:
      - type: Integer
        description: "Floor integer."

  - signature: "function FastCeilSingle(Value: TFloat): Integer;"
    summary: "Fast ceiling operation for Single precision."
    parameters:
      - name: Value
        type: TFloat
        description: "Single-precision float value."
    returns:
      - type: Integer
        description: "Ceil integer."

  - signature: "function FastCeilDouble(Value: Double): Integer;"
    summary: "Fast ceiling operation for Double precision."
    parameters:
      - name: Value
        type: Double
        description: "Double-precision float value."
    returns:
      - type: Integer
        description: "Ceil integer."
---

## Description

The fast rounding routines (`FastFloor`, `FastCeil`, `FastTrunc`, `FastRound`, `FastFloorSingle`, `FastFloorDouble`, `FastCeilSingle`, `FastCeilDouble`) provide optimized replacements for standard Pascal Runtime Library (RTL) floating-point conversion routines.

When CPU instruction set extensions are available at runtime (such as SSE2 or SSE4.1), Graphics32 binds the rounding function delegates (`FastTrunc`, `FastRound`, `FastFloorSingle`, `FastFloorDouble`, `FastCeilSingle`, `FastCeilDouble`) to specialized hardware-accelerated SIMD implementations.
