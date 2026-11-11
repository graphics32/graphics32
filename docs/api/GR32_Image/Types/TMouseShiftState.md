---
layout: doc
docType: api
unit: GR32_Image
entity: TMouseShiftState
kind: Type
aliases: [mssShift, mssAlt, mssCtrl]
declaration: |
  TMouseShiftState = set of (mssShift, mssAlt, mssCtrl);
summary: "Set of modifier key requirements for mouse pan and zoom interactions."
---

## Description

`TMouseShiftState` specifies required keyboard modifier keys that must be held during mouse operations in [[TMousePanOptions]] and [[TMouseZoomOptions]].

| Flag | Description |
| --- | --- |
| `mssShift` | Requires the Shift key to be depressed. |
| `mssAlt` | Requires the Alt key to be depressed. |
| `mssCtrl` | Requires the Ctrl key to be depressed. |
