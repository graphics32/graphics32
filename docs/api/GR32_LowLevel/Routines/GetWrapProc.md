---
layout: doc
docType: api
unit: GR32_LowLevel
entity: GetWrapProc
kind: Function
summary: "Returns the procedure delegate corresponding to a given TWrapMode and range."
overloads:
  - signature: "function GetWrapProc(WrapMode: TWrapMode): TWrapProc; overload;"
    summary: "Returns the standard TWrapProc procedure for the specified WrapMode."
    parameters:
      - name: WrapMode
        type: TWrapMode
        description: "Wrap mode (wmClamp, wmRepeat, wmMirror, wmReflect)."
    returns:
      - type: TWrapProc
        description: "Procedure pointer matching the requested WrapMode."

  - signature: "function GetWrapProc(WrapMode: TWrapMode; Max: Integer): TWrapProc; overload;"
    summary: "Returns the optimal TWrapProc procedure for the specified WrapMode and range [0..Max]."
    parameters:
      - name: WrapMode
        type: TWrapMode
        description: "Wrap mode."
      - name: Max
        type: Integer
        description: "Upper range bound."
    returns:
      - type: TWrapProc
        description: "Optimal procedure pointer."
seealso:
  - "[[GetWrapProcEx]]"
  - "[[GetOptimalWrap]]"
  - "[[GetOptimalReflect]]"
---

## Description

`GetWrapProc` inspects `WrapMode` (and optional range `Max`) and returns the appropriate single-parameter wrapping routine pointer (`Clamp`, `Wrap`/`WrapPow2`, `Mirror`, or `Reflect`/`ReflectPow2`).
