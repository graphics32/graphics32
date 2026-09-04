---
layout: doc
docType: api
unit: GR32_Transforms
parent: TTransformation
entity: TTransformation.Transform
kind: Method
summary: "Performs forward coordinate mapping from source to destination space."
overloads:
  - signature: "function Transform(const P: TPoint): TPoint; overload; virtual;"
    summary: "Maps source integer point to destination integer point."
    parameters:
      - name: P
        type: TPoint
        description: "Source point in integer coordinates."
    returns:
      - type: TPoint
        description: "The transformed destination point in integer coordinates."
  - signature: "function Transform(const P: TFixedPoint): TFixedPoint; overload; virtual;"
    summary: "Maps source fixed-point coordinate to destination fixed-point coordinate."
    parameters:
      - name: P
        type: TFixedPoint
        description: "Source point in fixed-point coordinates."
    returns:
      - type: TFixedPoint
        description: "The transformed destination point in fixed-point coordinates."
  - signature: "function Transform(const P: TFloatPoint): TFloatPoint; overload; virtual;"
    summary: "Maps source floating-point coordinate to destination floating-point coordinate."
    parameters:
      - name: P
        type: TFloatPoint
        description: "Source point in floating-point coordinates."
    returns:
      - type: TFloatPoint
        description: "The transformed destination point in floating-point coordinates."
---

## Description

`Transform` maps source coordinate `P` *forward* into destination space. Forward transformation is primarily for vector path distortion.

::: warning
Note that not all transformation classes support forward transformation.

If the transformation does not support forward transformation, calling `Transform` will raise an ETransformNotImplemented exception.
:::
