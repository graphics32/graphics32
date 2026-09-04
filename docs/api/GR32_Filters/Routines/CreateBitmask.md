---
layout: doc
docType: api
unit: GR32_Filters
entity: CreateBitmask
kind: Function
declaration: "function CreateBitmask(Components: TColor32Components): TColor32;"
summary: "Generates a TColor32 bitmask corresponding to specified ARGB color channels."
parameters:
  - name: Components
    type: TColor32Components
    description: "Set of ARGB channels to include in mask."
returns:
  - type: TColor32
    description: "A 32-bit `TColor32` bitmask where specified ARGB channel bytes are set to `$FF` and unselected channels are `$00`."
---

## Description

`CreateBitmask` constructs a 32-bit DWORD mask where bit channels corresponding to `Components` are set to `$FF` and unselected channels are `$00`.

## Example

```pascal
Mask := CreateBitmask([ccRed, ccBlue]);
```
