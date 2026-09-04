---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: Circle
kind: Function
summary: "Generates circular polygon approximations."
overloads:
  - signature: "function Circle(const P: TFloatPoint; const Radius: TFloat; Steps: Integer): TArrayOfFloatPoint; overload;"
    summary: "Generates a circular polygon centered at P with specified radius and steps."
    parameters:
      - name: P
        type: TFloatPoint
        description: "Center coordinates."
      - name: Radius
        type: TFloat
        description: "Circle radius."
      - name: Steps
        type: Integer
        description: "Number of linear steps."

    returns:
      - type: TArrayOfFloatPoint
        description: "A [[TArrayOfFloatPoint]] array containing generated polygon coordinates."
  - signature: "function Circle(const P: TFloatPoint; const Radius: TFloat): TArrayOfFloatPoint; overload;"
    summary: "Generates a circular polygon centered at P with automatically computed step count."
    parameters:
      - name: P
        type: TFloatPoint
        description: "Center coordinates."
      - name: Radius
        type: TFloat
        description: "Circle radius."

    returns:
      - type: TArrayOfFloatPoint
        description: "A [[TArrayOfFloatPoint]] array containing generated polygon coordinates."
  - signature: "function Circle(const X, Y, Radius: TFloat; Steps: Integer): TArrayOfFloatPoint; overload;"
    summary: "Generates a circular polygon centered at (X, Y) with specified steps."
    parameters:
      - name: X, Y
        type: TFloat
        description: "Center X, Y coordinates."
      - name: Radius
        type: TFloat
        description: "Circle radius."
      - name: Steps
        type: Integer
        description: "Number of linear steps."

    returns:
      - type: TArrayOfFloatPoint
        description: "A [[TArrayOfFloatPoint]] array containing generated polygon coordinates."
  - signature: "function Circle(const X, Y, Radius: TFloat): TArrayOfFloatPoint; overload;"
    summary: "Generates a circular polygon centered at (X, Y)."
    parameters:
      - name: X, Y
        type: TFloat
        description: "Center coordinates."
      - name: Radius
        type: TFloat
        description: "Circle radius."

    returns:
      - type: TArrayOfFloatPoint
        description: "A [[TArrayOfFloatPoint]] array containing generated polygon coordinates."
  - signature: "function Circle(const R: TRect): TArrayOfFloatPoint; overload;"
    summary: "Generates a circular polygon inscribed within integer TRect R."
    parameters:
      - name: R
        type: TRect
        description: "Bounding rectangle."

    returns:
      - type: TArrayOfFloatPoint
        description: "A [[TArrayOfFloatPoint]] array containing generated polygon coordinates."
  - signature: "function Circle(const R: TRect; Steps: Integer): TArrayOfFloatPoint; overload;"
    summary: "Generates a circular polygon inscribed within integer TRect R with specified steps."
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
  - signature: "function Circle(const R: TFloatRect): TArrayOfFloatPoint; overload;"
    summary: "Generates a circular polygon inscribed within TFloatRect R."
    parameters:
      - name: R
        type: TFloatRect
        description: "Bounding rectangle."

    returns:
      - type: TArrayOfFloatPoint
        description: "A [[TArrayOfFloatPoint]] array containing generated polygon coordinates."
  - signature: "function Circle(const R: TFloatRect; Steps: Integer): TArrayOfFloatPoint; overload;"
    summary: "Generates a circular polygon inscribed within TFloatRect R with specified steps."
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

`Circle` generates closed polygon vertex arrays approximating circular curves.
