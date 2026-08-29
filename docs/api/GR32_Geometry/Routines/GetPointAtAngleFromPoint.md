---
layout: doc
docType: api
unit: GR32_Geometry
entity: GetPointAtAngleFromPoint
kind: Function
summary: "Calculates a target point located at a specified distance and angle from an origin point."
overloads:
  - signature: "function GetPointAtAngleFromPoint(const Pt: TFloatPoint; const Dist, Radians: Single): TFloatPoint; overload;"
    summary: "Calculates target floating-point coordinate at specified distance and angle in radians."
    parameters:
      - name: Pt
        type: TFloatPoint
        description: "Origin point."
      - name: Dist
        type: Single
        description: "Distance from origin point."
      - name: Radians
        type: Single
        description: "Angle in radians."
  - signature: "function GetPointAtAngleFromPoint(const Pt: TFixedPoint; const Dist, Radians: Single): TFixedPoint; overload;"
    summary: "Calculates target fixed-point coordinate at specified distance and angle in radians."
    parameters:
      - name: Pt
        type: TFixedPoint
        description: "Origin point."
      - name: Dist
        type: Single
        description: "Distance from origin point."
      - name: Radians
        type: Single
        description: "Angle in radians."
---

## Description

`GetPointAtAngleFromPoint` computes a new coordinate at polar offset (`Dist`, `Radians`) from base point `Pt`.

In 2D computer graphics where the Y-axis increases downwards:
- $X = \text{Dist} \cdot \cos(\text{Radians}) + \text{Pt.X}$
- $Y = -\text{Dist} \cdot \sin(\text{Radians}) + \text{Pt.Y}$
