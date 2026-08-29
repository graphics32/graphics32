---
layout: doc
docType: api
unit: GR32_Geometry
entity: SegmentIntersect
kind: Function
summary: "Calculates the intersection point between two 2D line segments P1-P2 and P3-P4."
overloads:
  - signature: "function SegmentIntersect(const P1, P2, P3, P4: TFloatPoint; out IntersectPoint: TFloatPoint): Boolean; overload;"
    summary: "Calculates intersection point between floating-point line segments P1-P2 and P3-P4."
    parameters:
      - name: P1, P2
        type: TFloatPoint
        description: "Endpoints of first line segment."
      - name: P3, P4
        type: TFloatPoint
        description: "Endpoints of second line segment."
      - name: IntersectPoint
        type: TFloatPoint
        description: "Output point receiving intersection coordinates."
  - signature: "function SegmentIntersect(const P1, P2, P3, P4: TFixedPoint; out IntersectPoint: TFixedPoint): Boolean; overload;"
    summary: "Calculates intersection point between fixed-point line segments P1-P2 and P3-P4."
    parameters:
      - name: P1, P2
        type: TFixedPoint
        description: "Endpoints of first line segment."
      - name: P3, P4
        type: TFixedPoint
        description: "Endpoints of second line segment."
      - name: IntersectPoint
        type: TFixedPoint
        description: "Output point receiving intersection coordinates."
---

## Description

`SegmentIntersect` determines whether line segment $P1 \rightarrow P2$ intersects line segment $P3 \rightarrow P4$.

If an intersection occurs on both segments, the function populates `IntersectPoint` with the coordinates of the intersection and returns `True`. If the segments are parallel or do not overlap, the function returns `False`.
