---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: InSignedRange
kind: Function
summary: "Checks if a scalar coordinate lies within a specified signed range."
overloads:
  - signature: "function InSignedRange(const X, X1, X2: TFloat): Boolean; overload;"
    summary: "Checks if float X lies between X1 and X2 regardless of ordering."
    parameters:
      - name: X, X1, X2
        type: TFloat
        description: "Test coordinate X and range bounds X1, X2."

    returns:
      - type: Boolean
        description: "Returns `True` if successful or intersecting; otherwise `False`."
  - signature: "function InSignedRange(const X, X1, X2: TFixed): Boolean; overload;"
    summary: "Checks if fixed-point X lies between X1 and X2 regardless of ordering."
    parameters:
      - name: X, X1, X2
        type: TFixed
        description: "Test coordinate X and range bounds X1, X2."

    returns:
      - type: Boolean
        description: "Returns `True` if successful or intersecting; otherwise `False`."
---

## Description

`InSignedRange` returns `True` if `X` is between `X1` and `X2` (inclusive), accommodating both ascending (`X1 <= X2`) and descending (`X1 > X2`) bounds.
