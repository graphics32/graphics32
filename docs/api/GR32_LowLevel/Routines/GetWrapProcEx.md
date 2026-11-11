---
layout: doc
docType: api
unit: GR32_LowLevel
entity: GetWrapProcEx
kind: Function
summary: "Returns the extended procedure delegate corresponding to a given TWrapMode and range."
overloads:
  - signature: "function GetWrapProcEx(WrapMode: TWrapMode): TWrapProcEx; overload;"
    summary: "Returns the standard TWrapProcEx procedure for the specified WrapMode."
    parameters:
      - name: WrapMode
        type: TWrapMode
        description: "Wrap mode (wmClamp, wmRepeat, wmMirror, wmReflect)."
    returns:
      - type: TWrapProcEx
        description: "Extended procedure pointer matching the requested WrapMode."

  - signature: "function GetWrapProcEx(WrapMode: TWrapMode; Min, Max: Integer): TWrapProcEx; overload;"
    summary: "Returns the optimal TWrapProcEx procedure for the specified WrapMode and range [Min..Max]."
    parameters:
      - name: WrapMode
        type: TWrapMode
        description: "Wrap mode."
      - name: Min
        type: Integer
        description: "Lower range bound."
      - name: Max
        type: Integer
        description: "Upper range bound."
    returns:
      - type: TWrapProcEx
        description: "Optimal extended procedure pointer."
seealso:
  - "[[GetWrapProc]]"
  - "[[GetOptimalWrap]]"
  - "[[GetOptimalReflect]]"
---

## Description

`GetWrapProcEx` inspects `WrapMode` (and optional range `[Min..Max]`) and returns the appropriate 3-parameter extended wrapping routine pointer.
