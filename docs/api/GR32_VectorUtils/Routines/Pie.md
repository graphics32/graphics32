---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: Pie
kind: Function
summary: "Generates pie/wedge polygon approximations."
overloads:
  - signature: "function Pie(const P: TFloatPoint; const Radius: TFloat; const Angle, Offset: TFloat; Steps: Integer): TArrayOfFloatPoint; overload;"
    summary: "Generates a pie wedge centered at P with specified radius, angle, offset, and steps."
    parameters:
      - name: P
        type: TFloatPoint
        description: "Center coordinates."
      - name: Radius
        type: TFloat
        description: "Wedge radius."
      - name: Angle, Offset
        type: TFloat
        description: "Wedge arc angle and starting offset angle in radians."
      - name: Steps
        type: Integer
        description: "Number of linear steps."

  - signature: "function Pie(const P: TFloatPoint; const Radius: TFloat; const Angle: TFloat; const Offset: TFloat = 0): TArrayOfFloatPoint; overload;"
    summary: "Generates a pie wedge centered at P with automatically computed steps."
    parameters:
      - name: P
        type: TFloatPoint
        description: "Center coordinates."
      - name: Radius
        type: TFloat
        description: "Wedge radius."
      - name: Angle, Offset
        type: TFloat
        description: "Wedge arc angle and starting offset angle in radians."

  - signature: "function Pie(const P: TFloatPoint; const Radius: TFloat; const Angle: TFloat; Steps: Integer): TArrayOfFloatPoint; overload;"
    summary: "Generates a pie wedge centered at P."
    parameters:
      - name: P
        type: TFloatPoint
        description: "Center coordinates."
      - name: Radius
        type: TFloat
        description: "Wedge radius."
      - name: Angle
        type: TFloat
        description: "Wedge arc angle."
      - name: Steps
        type: Integer
        description: "Number of linear steps."

  - signature: "function Pie(const X, Y, Radius: TFloat; const Angle, Offset: TFloat; Steps: Integer): TArrayOfFloatPoint; overload;"
    summary: "Generates a pie wedge centered at (X, Y)."
    parameters:
      - name: X, Y
        type: TFloat
        description: "Center coordinates."
      - name: Radius
        type: TFloat
        description: "Wedge radius."
      - name: Angle, Offset
        type: TFloat
        description: "Wedge arc angle and starting offset angle in radians."
      - name: Steps
        type: Integer
        description: "Number of linear steps."

  - signature: "function Pie(const X, Y, Radius: TFloat; const Angle: TFloat; const Offset: TFloat = 0): TArrayOfFloatPoint; overload;"
    summary: "Generates a pie wedge centered at (X, Y)."
    parameters:
      - name: X, Y
        type: TFloat
        description: "Center coordinates."
      - name: Radius
        type: TFloat
        description: "Wedge radius."
      - name: Angle, Offset
        type: TFloat
        description: "Wedge arc angle and starting offset angle in radians."

  - signature: "function Pie(const X, Y, Radius: TFloat; const Angle: TFloat; Steps: Integer): TArrayOfFloatPoint; overload;"
    summary: "Generates a pie wedge centered at (X, Y)."
    parameters:
      - name: X, Y
        type: TFloat
        description: "Center coordinates."
      - name: Radius
        type: TFloat
        description: "Wedge radius."
      - name: Angle
        type: TFloat
        description: "Wedge arc angle in radians."
      - name: Steps
        type: Integer
        description: "Number of linear steps."
---

## Description

`Pie` constructs closed polygon vertex arrays approximating pie/wedge sectors with a center vertex and circular arc.
