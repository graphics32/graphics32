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

`ApplyBitmask` applies bitwise `and`, `or`, or `xor` logical operations on a bitmap fragment using optimized x86/x64 assembly routines.

The operation is carried out in the following manner: *\<Source Pixel> \<Logical Operation> \<Bitmask>*. The logical operation is defined by the provided Logical Operator.

Each byte in the bitmask will be used as component corresponding operand. The following combinations illustrates different applications:

| Description | Parameter Values | Source | Result |
| --- | --- | --- | --- |
| Zero out Red and Blue components | `LogicalOperator = loAND`<br>`Bitmask = $FF00FF00` | ![](/images/applybitmask-none.png) | ![](/images/applybitmask-green.png) |
| Invertion of Blue component | `LogicalOperator = loXOR`<br>`Bitmask = $000000FF` | ![](/images/applybitmask-none.png) | ![](/images/applybitmask-blue.png) |
| Full power to Red component | `LogicalOperator = loOR`<br>`Bitmask = $00FF0000` | ![](/images/applybitmask-none.png) | ![](/images/applybitmask-red.png) |
| Weird filter | `LogicalOperator = loOR`<br>`Bitmask = $BABEC0DE` | ![](/images/applybitmask-none.png) | ![](/images/applybitmask-custom.png) |

The auxiliary function [[CreateBitmask]] can be used to create bitmasks.

## Example

```pascal
ApplyBitmask(Bitmap, Bitmap.BoundsRect, $00FFFFFF, loAND); // Clear alpha channel
```
