---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: BuildArc
kind: Function
summary: "Generates an arc curve contour."
overloads:
  - signature: "function BuildArc(const P: TFloatPoint; StartAngle, EndAngle, Radius: TFloat; Steps: Integer): TArrayOfFloatPoint; overload;"
    summary: "Generates a floating-point arc contour centered at P with specified angular span and steps."
    parameters:
      - name: P
        type: TFloatPoint
        description: "Center point of the arc."
      - name: StartAngle, EndAngle
        type: TFloat
        description: "Start and end angles in radians."
      - name: Radius
        type: TFloat
        description: "Arc radius."
      - name: Steps
        type: Integer
        description: "Number of linear steps approximating the arc."

  - signature: "function BuildArc(const P: TFloatPoint; StartAngle, EndAngle, Radius: TFloat): TArrayOfFloatPoint; overload;"
    summary: "Generates a floating-point arc contour with automatically calculated step count."
    parameters:
      - name: P
        type: TFloatPoint
        description: "Center point."
      - name: StartAngle, EndAngle
        type: TFloat
        description: "Start and end angles in radians."
      - name: Radius
        type: TFloat
        description: "Arc radius."

  - signature: "function BuildArc(const P: TFixedPoint; StartAngle, EndAngle, Radius: TFloat; Steps: Integer): TArrayOfFixedPoint; overload;"
    summary: "Generates a fixed-point arc contour centered at P with specified steps."
    parameters:
      - name: P
        type: TFixedPoint
        description: "Fixed-point center coordinates."
      - name: StartAngle, EndAngle
        type: TFloat
        description: "Start and end angles in radians."
      - name: Radius
        type: TFloat
        description: "Arc radius."
      - name: Steps
        type: Integer
        description: "Number of linear steps."

  - signature: "function BuildArc(const P: TFixedPoint; StartAngle, EndAngle, Radius: TFloat): TArrayOfFixedPoint; overload;"
    summary: "Generates a fixed-point arc contour with automatically calculated step count."
    parameters:
      - name: P
        type: TFixedPoint
        description: "Fixed-point center coordinates."
      - name: StartAngle, EndAngle
        type: TFloat
        description: "Start and end angles in radians."
      - name: Radius
        type: TFloat
        description: "Arc radius."
---

## Description

`BuildArc` constructs a polyline curve array approximating a circular arc centered at `P` extending from `StartAngle` to `EndAngle`.
