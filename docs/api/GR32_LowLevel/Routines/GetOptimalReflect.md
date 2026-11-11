---
layout: doc
docType: api
unit: GR32_LowLevel
entity: GetOptimalReflect
kind: Function
summary: "Determines whether to return standard Reflect or bitwise optimized ReflectPow2."
overloads:
  - signature: "function GetOptimalReflect(Max: Integer): TWrapProc; overload;"
    summary: "Returns optimal TWrapProc delegate for reflection in range [0..Max]."
    parameters:
      - name: Max
        type: Integer
        description: "Upper range boundary."
    returns:
      - type: TWrapProc
        description: "Reflect procedure delegate (Reflect or ReflectPow2)."

  - signature: "function GetOptimalReflect(Min, Max: Integer): TWrapProcEx; overload;"
    summary: "Returns optimal TWrapProcEx delegate for reflection in range [Min..Max]."
    parameters:
      - name: Min
        type: Integer
        description: "Lower range boundary."
      - name: Max
        type: Integer
        description: "Upper range boundary."
    returns:
      - type: TWrapProcEx
        description: "Reflect procedure delegate (Reflect or ReflectPow2)."
seealso:
  - "[[GetOptimalWrap]]"
  - "[[GetWrapProc]]"
  - "[[GetWrapProcEx]]"
---

## Description

`GetOptimalReflect` tests whether the specified range length is a power of two and returns either [[ReflectPow2]] or [[Reflect]].
