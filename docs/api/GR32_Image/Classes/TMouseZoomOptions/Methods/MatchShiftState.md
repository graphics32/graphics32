---
layout: doc
docType: api
unit: GR32_Image
parent: TMouseZoomOptions
entity: TMouseZoomOptions.MatchShiftState
kind: Method
declaration: "function MatchShiftState(AShiftState: TShiftState): Boolean;"
summary: "Checks if current shift state matches zoom modifier requirements."
parameters:
  - name: AShiftState
    type: TShiftState
    description: "Current keyboard shift state."
returns:
  - type: Boolean
    description: "True if shift state matches requirements."
---

## Description

`MatchShiftState` evaluates whether the passed `TShiftState` matches the configured `ShiftState` modifier keys.
