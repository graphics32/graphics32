---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: Star
kind: Function
summary: "Generates star polygon contours."
overloads:
  - signature: "function Star(const P: TFloatPoint; const InnerRadius, OuterRadius: TFloat; Vertices: Integer = 5; Rotation: TFloat = 0): TArrayOfFloatPoint; overload;"
    summary: "Generates a star centered at P with specified inner/outer radii, vertex count, and rotation."
    parameters:
      - name: P
        type: TFloatPoint
        description: "Center coordinates."
      - name: InnerRadius, OuterRadius
        type: TFloat
        description: "Inner and outer star radii."
      - name: Vertices
        type: Integer
        description: "Number of star points/vertices."
      - name: Rotation
        type: TFloat
        description: "Initial rotation angle in radians."

  - signature: "function Star(const X, Y, InnerRadius, OuterRadius: TFloat; Vertices: Integer = 5; Rotation: TFloat = 0): TArrayOfFloatPoint; overload;"
    summary: "Generates a star centered at (X, Y)."
    parameters:
      - name: X, Y
        type: TFloat
        description: "Center coordinates."
      - name: InnerRadius, OuterRadius
        type: TFloat
        description: "Inner and outer radii."
      - name: Vertices
        type: Integer
        description: "Number of star points."
      - name: Rotation
        type: TFloat
        description: "Initial rotation angle in radians."

  - signature: "function Star(const P: TFloatPoint; const Radius: TFloat; Vertices: Integer = 5; Rotation: TFloat = 0): TArrayOfFloatPoint; overload;"
    summary: "Generates a regular star centered at P."
    parameters:
      - name: P
        type: TFloatPoint
        description: "Center coordinates."
      - name: Radius
        type: TFloat
        description: "Star radius."
      - name: Vertices
        type: Integer
        description: "Number of points."
      - name: Rotation
        type: TFloat
        description: "Rotation angle in radians."

  - signature: "function Star(const X, Y, Radius: TFloat; Vertices: Integer = 5; Rotation: TFloat = 0): TArrayOfFloatPoint; overload;"
    summary: "Generates a regular star centered at (X, Y)."
    parameters:
      - name: X, Y
        type: TFloat
        description: "Center coordinates."
      - name: Radius
        type: TFloat
        description: "Star radius."
      - name: Vertices
        type: Integer
        description: "Number of points."
      - name: Rotation
        type: TFloat
        description: "Rotation angle in radians."
---

## Description

`Star` constructs star-shaped polygon contours.
