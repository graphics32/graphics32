---
layout: doc
docType: api
unit: GR32_LowLevel
entity: GetOptimalWrap
kind: Function
summary: "Determines whether to return standard Wrap/Reflect or bitwise optimized WrapPow2/ReflectPow2."
overloads:
  - signature: "function GetOptimalWrap(Max: Integer): TWrapProc; overload;"
    summary: "Returns optimal TWrapProc delegate for range [0..Max]."
    parameters:
      - name: Max
        type: Integer
        description: "Upper range boundary."
    returns:
      - type: TWrapProc
        description: "Wrap procedure delegate (Wrap or WrapPow2)."

  - signature: "function GetOptimalWrap(Min, Max: Integer): TWrapProcEx; overload;"
    summary: "Returns optimal TWrapProcEx delegate for range [Min..Max]."
    parameters:
      - name: Min
        type: Integer
        description: "Lower range boundary."
      - name: Max
        type: Integer
        description: "Upper range boundary."
    returns:
      - type: TWrapProcEx
        description: "Wrap procedure delegate (Wrap or WrapPow2)."
seealso:
  - "[[GetOptimalReflect]]"
  - "[[GetWrapProc]]"
  - "[[GetWrapProcEx]]"
---

## Description

`GetOptimalWrap` tests whether the length of the given range (`Max + 1` or `Max - Min + 1`) is a power of two. If so, it returns the fast bitwise [[WrapPow2]] function delegate; otherwise, it returns standard [[Wrap]].
