---
layout: doc
docType: api
unit: GR32_Image
parent: TMousePanOptions
entity: TMousePanOptions.MatchShiftState
kind: Method
declaration: "function MatchShiftState(AShiftState: TShiftState): Boolean;"
summary: "Checks if current keyboard shift state matches configured ShiftState requirements."
parameters:
  - name: AShiftState
    type: TShiftState
    description: "Current keyboard shift state."
returns:
  - type: Boolean
    description: "True if shift state matches configured requirements."
---

## Description

`MatchShiftState` evaluates whether the passed `TShiftState` matches the configured `ShiftState` requirements (`mssShift`, `mssAlt`, `mssCtrl`).
