---
layout: doc
docType: api
unit: GR32_LowLevel
entity: TestSwap
kind: Procedure
summary: "Exchanges two values if the second value is strictly smaller than the first."
overloads:
  - signature: "procedure TestSwap(var A, B: Integer); overload;"
    summary: "Ensures A <= B for Integer values by swapping if B < A."
    parameters:
      - name: A, B
        type: Integer
        description: "Integer variables to evaluate and conditionally swap."

  - signature: "procedure TestSwap(var A, B: TFixed); overload;"
    summary: "Ensures A <= B for TFixed values by swapping if B < A."
    parameters:
      - name: A, B
        type: TFixed
        description: "Fixed-point variables to evaluate and conditionally swap."
seealso:
  - "[[Swap]]"
  - "[[TestClip]]"
---

## Description

`TestSwap` checks whether `B < A`. If `B` is smaller than `A`, the two variables are swapped so that upon return, `A` is guaranteed to be less than or equal to `B`.
