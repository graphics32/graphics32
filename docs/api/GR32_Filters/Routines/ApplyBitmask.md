---
layout: doc
docType: api
unit: GR32_Filters
entity: ApplyBitmask
kind: Function
summary: "Performs bitwise logical operations between bitmap pixels and a bitmask."
overloads:
  - signature: "procedure ApplyBitmask(Dst: TCustomBitmap32; DstX, DstY: Integer; Src: TCustomBitmap32; SrcRect: TRect; Bitmask: TColor32; LogicalOperator: TLogicalOperator); overload;"
    summary: "Performs bitwise logical operation between Src sub-rectangle and Dst."
    parameters:
      - name: Dst
        type: TCustomBitmap32
        description: "Destination bitmap."
      - name: DstX, DstY
        type: Integer
        description: "Destination coordinates."
      - name: Src
        type: TCustomBitmap32
        description: "Source bitmap."
      - name: SrcRect
        type: TRect
        description: "Source sub-rectangle."
      - name: Bitmask
        type: TColor32
        description: "32-bit bitmask."
      - name: LogicalOperator
        type: TLogicalOperator
        description: "Logical operator (loXOR, loAND, loOR)."

  - signature: "procedure ApplyBitmask(ABitmap: TCustomBitmap32; ARect: TRect; Bitmask: TColor32; LogicalOperator: TLogicalOperator); overload;"
    summary: "Performs in-place bitwise logical operation on a rectangular region of ABitmap."
    parameters:
      - name: ABitmap
        type: TCustomBitmap32
        description: "Bitmap to process in-place."
      - name: ARect
        type: TRect
        description: "Sub-rectangle region."
      - name: Bitmask
        type: TColor32
        description: "32-bit bitmask."
      - name: LogicalOperator
        type: TLogicalOperator
        description: "Logical operator."
---

## Description

`ApplyBitmask` applies bitwise `and`, `or`, or `xor` logical operations using optimized x86/x64 assembly routines.

## Example

```pascal
ApplyBitmask(Bitmap, Bitmap.BoundsRect, $00FFFFFF, loAND); // Clear alpha channel
```
