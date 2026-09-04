---
layout: doc
docType: api
unit: GR32_Geometry
entity: Average
kind: Function
summary: "Calculates the midpoint / average position between two points."
overloads:
  - signature: "function Average(const V1, V2: TFloatPoint): TFloatPoint; overload;"
    summary: "Calculates the midpoint between two floating-point vectors."
    parameters:
      - name: V1
        type: TFloatPoint
        description: "First point."
      - name: V2
        type: TFloatPoint
        description: "Second point."
    returns:
      - type: TFloatPoint
        description: "The calculated [[TFloatPoint]] midpoint between V1 and V2."
  - signature: "function Average(const V1, V2: TFixedPoint): TFixedPoint; overload;"
    summary: "Calculates the midpoint between two fixed-point vectors."
    parameters:
      - name: V1
        type: TFixedPoint
        description: "First point."
      - name: V2
        type: TFixedPoint
        description: "Second point."
    returns:
      - type: TFixedPoint
        description: "The calculated [[TFixedPoint]] midpoint between V1 and V2."
  - signature: "function Average(const V1, V2: TPoint): TPoint; overload;"
    summary: "Calculates the midpoint between two integer points."
    parameters:
      - name: V1
        type: TPoint
        description: "First point."
      - name: V2
        type: TPoint
        description: "Second point."

    returns:
      - type: TPoint
        description: "The calculated [[TPoint]] midpoint between V1 and V2."
---

## Description

`Average` computes the geometric midpoint between two coordinates $V1$ and $V2$. Each component ($X$ and $Y$) of the resulting point is calculated as $(V1 + V2) / 2$.

## Example

```pascal
var
  P1, P2, Mid: TFloatPoint;
begin
  P1 := FloatPoint(10.0, 20.0);
  P2 := FloatPoint(30.0, 40.0);
  Mid := Average(P1, P2); // Result: (20.0, 30.0)
end;
```
