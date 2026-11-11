---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: Ellipse
kind: Function
summary: "Generates elliptical polygon approximations."
overloads:
  - signature: "function Ellipse(const P, R: TFloatPoint; Steps: Integer): TArrayOfFloatPoint; overload;"
    summary: "Generates an ellipse centered at P with radii Rx, Ry in R."
    parameters:
      - name: P
        type: TFloatPoint
        description: "Center coordinates."
      - name: R
        type: TFloatPoint
        description: "Horizontal and vertical radii."
      - name: Steps
        type: Integer
        description: "Number of linear steps."

    returns:
      - type: TArrayOfFloatPoint
        description: "A [[TArrayOfFloatPoint]] array containing generated polygon coordinates."
  - signature: "function Ellipse(const P, R: TFloatPoint): TArrayOfFloatPoint; overload;"
    summary: "Generates an ellipse centered at P with automatically computed steps."
    parameters:
      - name: P
        type: TFloatPoint
        description: "Center coordinates."
      - name: R
        type: TFloatPoint
        description: "Radii."

    returns:
      - type: TArrayOfFloatPoint
        description: "A [[TArrayOfFloatPoint]] array containing generated polygon coordinates."
  - signature: "function Ellipse(const X, Y, Rx, Ry: TFloat; Steps: Integer): TArrayOfFloatPoint; overload;"
    summary: "Generates an ellipse centered at (X, Y) with radii Rx, Ry."
    parameters:
      - name: X, Y
        type: TFloat
        description: "Center coordinates."
      - name: Rx, Ry
        type: TFloat
        description: "Horizontal and vertical radii."
      - name: Steps
        type: Integer
        description: "Number of linear steps."

    returns:
      - type: TArrayOfFloatPoint
        description: "A [[TArrayOfFloatPoint]] array containing generated polygon coordinates."
  - signature: "function Ellipse(const X, Y, Rx, Ry: TFloat): TArrayOfFloatPoint; overload;"
    summary: "Generates an ellipse centered at (X, Y) with radii Rx, Ry."
    parameters:
      - name: X, Y
        type: TFloat
        description: "Center coordinates."
      - name: Rx, Ry
        type: TFloat
        description: "Radii."

    returns:
      - type: TArrayOfFloatPoint
        description: "A [[TArrayOfFloatPoint]] array containing generated polygon coordinates."
  - signature: "function Ellipse(const R: TRect): TArrayOfFloatPoint; overload;"
    summary: "Generates an ellipse inscribed within TRect R."
    parameters:
      - name: R
        type: TRect
        description: "Bounding rectangle."

    returns:
      - type: TArrayOfFloatPoint
        description: "A [[TArrayOfFloatPoint]] array containing generated polygon coordinates."
  - signature: "function Ellipse(const R: TRect; Steps: Integer): TArrayOfFloatPoint; overload;"
    summary: "Generates an ellipse inscribed within TRect R with specified steps."
    parameters:
      - name: R
        type: TRect
        description: "Bounding rectangle."
      - name: Steps
        type: Integer
        description: "Number of linear steps."

    returns:
      - type: TArrayOfFloatPoint
        description: "A [[TArrayOfFloatPoint]] array containing generated polygon coordinates."
  - signature: "function Ellipse(const R: TFloatRect): TArrayOfFloatPoint; overload;"
    summary: "Generates an ellipse inscribed within TFloatRect R."
    parameters:
      - name: R
        type: TFloatRect
        description: "Bounding rectangle."

    returns:
      - type: TArrayOfFloatPoint
        description: "A [[TArrayOfFloatPoint]] array containing generated polygon coordinates."
  - signature: "function Ellipse(const R: TFloatRect; Steps: Integer): TArrayOfFloatPoint; overload;"
    summary: "Generates an ellipse inscribed within TFloatRect R with specified steps."
    parameters:
      - name: R
        type: TFloatRect
        description: "Bounding rectangle."
      - name: Steps
        type: Integer
        description: "Number of linear steps."

    returns:
      - type: TArrayOfFloatPoint
        description: "A [[TArrayOfFloatPoint]] array containing generated polygon coordinates."
---

## Description

`Ellipse` constructs closed polygon vertex arrays approximating elliptical curves.
