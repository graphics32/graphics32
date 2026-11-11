---
layout: doc
docType: api
unit: GR32_ColorGradients
parent: TCustomArbitrarySparsePointGradientPolygonFiller
entity: TCustomArbitrarySparsePointGradientPolygonFiller.Add
kind: Method
summary: "Adds a scatter vertex point to the polygon filler."
overloads:
  - signature: "procedure Add(const Point: TFloatPoint; Color: TColor32); overload; virtual;"
    summary: "Adds point position and color."
    parameters:
      - name: Point
        type: TFloatPoint
        description: "Vertex position."
      - name: Color
        type: TColor32
        description: "Vertex color."
  - signature: "procedure Add(const ColorPoint: TColor32FloatPoint); overload; virtual;"
    summary: "Adds TColor32FloatPoint record."
    parameters:
      - name: ColorPoint
        type: TColor32FloatPoint
        description: "Point/color record."
---

## Description

Appends a new scatter point vertex to the polygon filler.
