---
layout: doc
docType: api
unit: GR32
parent: TCustomBitmap32
entity: TCustomBitmap32.SetStipple
kind: Method
scope: Public
summary: "Configures custom color array pattern for stippled line drawing."
overloads:
  - signature: "procedure SetStipple(const NewStipple: TArrayOfColor32); overload;"
    summary: "Sets stipple pattern from dynamic array of TColor32."
    parameters:
      - name: NewStipple
        type: TArrayOfColor32
        description: "Dynamic array of colors."
  - signature: "procedure SetStipple(const NewStipple: array of TColor32); overload;"
    summary: "Sets stipple pattern from open array of TColor32."
    parameters:
      - name: NewStipple
        type: array of TColor32
        description: "Open array of colors."
---

## Description

`SetStipple` defines the color pattern used when rendering stippled lines and frames.
