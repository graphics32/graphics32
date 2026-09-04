---
layout: doc
docType: api
unit: GR32_Transforms
parent: TTransformation
entity: TTransformation.ReverseTransform
kind: Method
summary: "Performs inverse coordinate mapping from destination to source space."
overloads:
  - signature: "function ReverseTransform(const P: TPoint): TPoint; overload; virtual;"
    summary: "Maps destination integer point back to source integer point."
    parameters:
      - name: P
        type: TPoint
        description: "Destination point in integer coordinates."
    returns:
      - type: TPoint
        description: "The inverse-transformed source point in integer coordinates."
  - signature: "function ReverseTransform(const P: TFixedPoint): TFixedPoint; overload; virtual;"
    summary: "Maps destination fixed-point coordinate back to source fixed-point coordinate."
    parameters:
      - name: P
        type: TFixedPoint
        description: "Destination point in fixed-point coordinates."
    returns:
      - type: TFixedPoint
        description: "The inverse-transformed source point in fixed-point coordinates."
  - signature: "function ReverseTransform(const P: TFloatPoint): TFloatPoint; overload; virtual;"
    summary: "Maps destination floating-point coordinate back to source floating-point coordinate."
    parameters:
      - name: P
        type: TFloatPoint
        description: "Destination point in floating-point coordinates."
    returns:
      - type: TFloatPoint
        description: "The inverse-transformed source point in floating-point coordinates."
---

## Description

`ReverseTransform` maps destination coordinate `P` back to source space. Inverse transformation is primarily for bitmap resampling.
