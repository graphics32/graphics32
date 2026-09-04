---
layout: doc
docType: api
unit: GR32_Transforms
parent: TTransformation
entity: TTransformation.GetTransformedBounds
kind: Method
summary: "Calculates the bounding rectangle of transformed coordinates."
overloads:
  - signature: "function GetTransformedBounds: TFloatRect; overload;"
    summary: "Calculates transformed bounding box using internal SrcRect."
    returns:
      - type: TFloatRect
        description: "The calculated bounding rectangle of transformed coordinates for internal `SrcRect`."
  - signature: "function GetTransformedBounds(const ASrcRect: TFloatRect): TFloatRect; overload; virtual;"
    summary: "Calculates transformed bounding box for a specified source rectangle."
    parameters:
      - name: ASrcRect
        type: TFloatRect
        description: "Source rectangle."
    returns:
      - type: TFloatRect
        description: "The calculated bounding rectangle of transformed coordinates for `ASrcRect`."
---

## Description

`GetTransformedBounds` calculates the bounding rectangle resulting from transforming source rectangle coordinates.

![GetTransformedBounds](/images/TTransformation.GetTransformedBounds.png)
