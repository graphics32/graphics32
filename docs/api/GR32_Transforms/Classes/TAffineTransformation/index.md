---
layout: doc
docType: api
unit: GR32_Transforms
entity: TAffineTransformation
kind: Class
declaration: "TAffineTransformation = class(T3x3Transformation)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TTransformation
  - T3x3Transformation
  - TAffineTransformation
summary: "Provides 2D affine transformation operations (rotation, scaling, translation, skewing, and matrix stacks)."
---

## Description

`TAffineTransformation` handles 2D affine linear mappings.

The transformation is defined by 3x3 homogeneous matrix of single-precision floats, [[TFloatMatrix]]:

$$ \begin{bmatrix} x_{\text{dst}} \\ y_{\text{dst}} \\ \text{not used} \end{bmatrix} = \begin{bmatrix} M_{0,0} & M_{1,0} & M_{2,0} \\ M_{0,1} & M_{1,1} & M_{2,1} \\ M_{0,2} & M_{1,2} & M_{2,2} \end{bmatrix} \cdot \begin{bmatrix} x_{\text{src}} \\ y_{\text{src}} \\ 1 \end{bmatrix}; $$

Only the first two rows are used for coordinate transformation at the final stage.

Affine transformations preserve straight lines and parallel lines while performing arbitrary combinations of translation, rotation, scaling, and skewing.

It includes matrix stack push/pop capabilities ([[Push]], [[Pop]]) for nested affine state operations.

[members]
