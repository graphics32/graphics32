---
layout: doc
docType: api
unit: GR32_VectorUtils
entity: ClipLine
kind: Function
summary: "Clips a 2D line segment to a bounding rectangle."
overloads:
  - signature: "function ClipLine(var X1, Y1, X2, Y2: Integer; MinX, MinY, MaxX, MaxY: Integer): Boolean; overload;"
    summary: "Clips an integer line segment to min/max integer bounds."
    parameters:
      - name: X1, Y1, X2, Y2
        type: Integer
        description: "Line segment endpoints (modified in-place)."
      - name: MinX, MinY, MaxX, MaxY
        type: Integer
        description: "Bounding clip box."

  - signature: "function ClipLine(var X1, Y1, X2, Y2: TFloat; MinX, MinY, MaxX, MaxY: TFloat): Boolean; overload;"
    summary: "Clips a floating-point line segment to min/max float bounds."
    parameters:
      - name: X1, Y1, X2, Y2
        type: TFloat
        description: "Line segment endpoints."
      - name: MinX, MinY, MaxX, MaxY
        type: TFloat
        description: "Bounding clip box."

  - signature: "function ClipLine(var X1, Y1, X2, Y2: TFixed; MinX, MinY, MaxX, MaxY: TFixed): Boolean; overload;"
    summary: "Clips a fixed-point line segment to min/max fixed bounds."
    parameters:
      - name: X1, Y1, X2, Y2
        type: TFixed
        description: "Line segment endpoints."
      - name: MinX, MinY, MaxX, MaxY
        type: TFixed
        description: "Bounding clip box."

  - signature: "function ClipLine(var P1, P2: TPoint; const ClipRect: TRect): Boolean; overload;"
    summary: "Clips integer points P1, P2 to TRect ClipRect."
    parameters:
      - name: P1, P2
        type: TPoint
        description: "Line endpoints."
      - name: ClipRect
        type: TRect
        description: "Clipping rectangle."

  - signature: "function ClipLine(var P1, P2: TFloatPoint; const ClipRect: TFloatRect): Boolean; overload;"
    summary: "Clips float points P1, P2 to TFloatRect ClipRect."
    parameters:
      - name: P1, P2
        type: TFloatPoint
        description: "Line endpoints."
      - name: ClipRect
        type: TFloatRect
        description: "Clipping rectangle."

  - signature: "function ClipLine(var P1, P2: TFixedPoint; const ClipRect: TFixedRect): Boolean; overload;"
    summary: "Clips fixed points P1, P2 to TFixedRect ClipRect."
    parameters:
      - name: P1, P2
        type: TFixedPoint
        description: "Line endpoints."
      - name: ClipRect
        type: TFixedRect
        description: "Clipping rectangle."
---

## Description

`ClipLine` clips a line segment against a 2D bounding rectangle using Cohen-Sutherland line clipping. Returns `True` if any part of the line remains inside the clipping bounds.
