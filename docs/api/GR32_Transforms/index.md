---
layout: doc
docType: api
unit: GR32_Transforms
entity: GR32_Transforms
kind: Unit
summary: "Provides affine matrix transformations, inverse mapping, non-linear deformations, and geometric projection for bitmaps and vector paths."
---

## Description

The `GR32_Transforms` unit provides a comprehensive framework for spatial coordinate transformations, bitmap resampling transformations, vector path warp operations, and geometric projections in Graphics32.

It includes:
- Base class hierarchy ([[TTransformation]], [[TNestedTransformation]], [[T3x3Transformation]]).
- Linear 2D affine operations ([[TAffineTransformation]]) for rotation, scaling, translation, and skewing.
- Projective quadrilateral mapping ([[TProjectiveTransformation]] and [[TProjectiveTransformationEx]]).
- Non-linear spatial warps and distortions (twirl, bloat, disturbance, fish-eye, polar, radial distortion, vector remap, path warp, and sphere projection).
- Low-level $3 \times 3$ floating-point and fixed-point matrix algebra routines.

[members]
