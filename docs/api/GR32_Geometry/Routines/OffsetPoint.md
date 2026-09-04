---
layout: doc
docType: api
unit: GR32_Geometry
entity: OffsetPoint
kind: Function
summary: "Translates a point by X and Y coordinate offsets."
overloads:
  - signature: "function OffsetPoint(const Pt: TFloatPoint; DeltaX, DeltaY: TFloat): TFloatPoint; overload;"
    summary: "Translates floating-point Pt by DeltaX and DeltaY."
    parameters:
      - name: Pt
        type: TFloatPoint
        description: "Point to translate."
      - name: DeltaX, DeltaY
        type: TFloat
        description: "Horizontal and vertical displacement."
    returns:
      - type: TFloatPoint
        description: "The offset [[TFloatPoint]] point shifted by specified displacement."
  - signature: "function OffsetPoint(const Pt, Delta: TFloatPoint): TFloatPoint; overload;"
    summary: "Translates floating-point Pt by delta point offset."
    parameters:
      - name: Pt
        type: TFloatPoint
        description: "Point to translate."
      - name: Delta
        type: TFloatPoint
        description: "Displacement vector."
    returns:
      - type: TFloatPoint
        description: "The offset [[TFloatPoint]] point shifted by specified displacement."
  - signature: "function OffsetPoint(const Pt: TFixedPoint; DeltaX, DeltaY: TFixed): TFixedPoint; overload;"
    summary: "Translates fixed-point Pt by fixed-point DeltaX and DeltaY offsets."
    parameters:
      - name: Pt
        type: TFixedPoint
        description: "Point to translate."
      - name: DeltaX, DeltaY
        type: TFixed
        description: "Fixed-point displacement."
    returns:
      - type: TFixedPoint
        description: "The offset [[TFixedPoint]] point shifted by specified displacement."
  - signature: "function OffsetPoint(const Pt: TFixedPoint; DeltaX, DeltaY: TFloat): TFixedPoint; overload;"
    summary: "Translates fixed-point Pt by floating-point DeltaX and DeltaY offsets."
    parameters:
      - name: Pt
        type: TFixedPoint
        description: "Point to translate."
      - name: DeltaX, DeltaY
        type: TFloat
        description: "Floating-point displacement."
    returns:
      - type: TFixedPoint
        description: "The offset [[TFixedPoint]] point shifted by specified displacement."
  - signature: "function OffsetPoint(const Pt: TFixedPoint; const Delta: TFixedPoint): TFixedPoint; overload;"
    summary: "Translates fixed-point Pt by fixed-point Delta vector."
    parameters:
      - name: Pt
        type: TFixedPoint
        description: "Point to translate."
      - name: Delta
        type: TFixedPoint
        description: "Fixed-point displacement vector."
    returns:
      - type: TFixedPoint
        description: "The offset [[TFixedPoint]] point shifted by specified displacement."
  - signature: "function OffsetPoint(const Pt: TFixedPoint; const Delta: TFloatPoint): TFixedPoint; overload;"
    summary: "Translates fixed-point Pt by floating-point Delta vector."
    parameters:
      - name: Pt
        type: TFixedPoint
        description: "Point to translate."
      - name: Delta
        type: TFloatPoint
        description: "Floating-point displacement vector."
    returns:
      - type: TFixedPoint
        description: "The offset [[TFixedPoint]] point shifted by specified displacement."
  - signature: "function OffsetPoint(const Pt: TPoint; DeltaX, DeltaY: Integer): TPoint; overload;"
    summary: "Translates integer Pt by integer DeltaX and DeltaY offsets."
    parameters:
      - name: Pt
        type: TPoint
        description: "Point to translate."
      - name: DeltaX, DeltaY
        type: Integer
        description: "Integer displacement."
    returns:
      - type: TPoint
        description: "The offset [[TPoint]] point shifted by specified displacement."
  - signature: "function OffsetPoint(const Pt, Delta: TPoint): TPoint; overload;"
    summary: "Translates integer Pt by integer Delta point offset."
    parameters:
      - name: Pt
        type: TPoint
        description: "Point to translate."
      - name: Delta
        type: TPoint
        description: "Integer displacement vector."

    returns:
      - type: TPoint
        description: "The offset [[TPoint]] point shifted by specified displacement."
---

## Description

`OffsetPoint` returns a new point translated by adding delta offset coordinates to `Pt`.
