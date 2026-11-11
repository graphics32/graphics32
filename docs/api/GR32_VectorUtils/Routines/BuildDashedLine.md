---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: BuildDashedLine
kind: Function
summary: "Splits input polylines into dashed line segment arrays."
overloads:
  - signature: "function BuildDashedLine(const Points: TArrayOfFloatPoint; const DashArray: TArrayOfFloat; DashOffset: TFloat = 0; Closed: Boolean = False): TArrayOfArrayOfFloatPoint; overload;"
    summary: "Splits a floating-point polyline into dashed segments according to DashArray lengths."
    parameters:
      - name: Points
        type: TArrayOfFloatPoint
        description: "Polyline vertices."
      - name: DashArray
        type: TArrayOfFloat
        description: "Array of alternating dash and gap lengths in pixels."
      - name: DashOffset
        type: TFloat
        description: "Initial offset into the dash pattern."
      - name: Closed
        type: Boolean
        description: "True if polyline is a closed loop."

    returns:
      - type: TArrayOfArrayOfFloatPoint
        description: "A [[TArrayOfArrayOfFloatPoint]] array containing generated polygon coordinates."
  - signature: "function BuildDashedLine(const Points: TArrayOfFixedPoint; const DashArray: TArrayOfFixed; DashOffset: TFixed = 0; Closed: Boolean = False): TArrayOfArrayOfFixedPoint; overload;"
    summary: "Splits a fixed-point polyline into dashed segments according to DashArray lengths."
    parameters:
      - name: Points
        type: TArrayOfFixedPoint
        description: "Fixed-point polyline vertices."
      - name: DashArray
        type: TArrayOfFixed
        description: "Array of dash/gap lengths in fixed-point format."
      - name: DashOffset
        type: TFixed
        description: "Initial dash offset."
      - name: Closed
        type: Boolean
        description: "True if polyline is a closed loop."

    returns:
      - type: TArrayOfArrayOfFixedPoint
        description: "A [[TArrayOfArrayOfFixedPoint]] array containing generated polygon coordinates."
---

## Description

`BuildDashedLine` breaks input polyline paths into multi-contour arrays containing dashed line segments.
