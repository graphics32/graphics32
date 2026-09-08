---
layout: doc
docType: api
unit: GR32_LowLevel
entity: TestClip
kind: Function
summary: "Orders two range coordinates and clips them to a specified interval."
overloads:
  - signature: "function TestClip(var A, B: Integer; const Size: Integer): Boolean; overload;"
    summary: "Orders A and B, then clips them to interval [0..Size-1]."
    parameters:
      - name: A, B
        type: Integer
        description: "Variables to order and clip to range [0..Size-1]."
      - name: Size
        type: Integer
        description: "Upper size constraint (defines interval [0..Size-1])."
    returns:
      - type: Boolean
        description: "Returns True if the clipped range overlaps with [0..Size-1]; False otherwise."

  - signature: "function TestClip(var A, B: Integer; const Start, Stop: Integer): Boolean; overload;"
    summary: "Orders A and B, then clips them to interval [Start..Stop]."
    parameters:
      - name: A, B
        type: Integer
        description: "Variables to order and clip to range [Start..Stop]."
      - name: Start
        type: Integer
        description: "Start boundary of interval."
      - name: Stop
        type: Integer
        description: "Stop boundary of interval."
    returns:
      - type: Boolean
        description: "Returns True if the clipped range overlaps with [Start..Stop]; False otherwise."
seealso:
  - "[[TestSwap]]"
  - "[[Clamp]]"
---

## Description

`TestClip` first ensures `A <= B` (swapping them if necessary using [[TestSwap]]), and then clips both `A` and `B` to fit within the specified bounds (`[0..Size-1]` or `[Start..Stop]`).

It returns `True` if the resulting interval `[A..B]` has common points with the target bounding range, or `False` if the range lies entirely outside.
