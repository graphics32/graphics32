---
layout: doc
docType: api
unit: GR32_Transforms
parent: TAffineTransformation
entity: TAffineTransformation.Clear
kind: Method
summary: "Resets the transformation matrix to identity or a specified base matrix."
overloads:
  - signature: "procedure Clear; overload;"
    summary: "Resets transformation matrix to identity matrix."
  - signature: "procedure Clear(const BaseMatrix: TFloatMatrix); overload;"
    summary: "Resets transformation matrix to BaseMatrix."
    parameters:
      - name: BaseMatrix
        type: TFloatMatrix
        description: "Base matrix to set."
---

## Description

`Clear` resets the current transformation matrix state to [[IdentityMatrix|identity]] or a base matrix.
