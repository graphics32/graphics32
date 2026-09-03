---
layout: doc
docType: api
unit: GR32_Paths
entity: TCustomCanvas
kind: Class
abstract: true
declaration: "TCustomCanvas = class abstract(TFlattenedPath)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TThreadPersistent
  - TCustomPath
  - TFlattenedPath
  - TCustomCanvas
summary: "Abstract canvas class managing path construction and coordinate transformation for vector path rendering."
---

## Description

`TCustomCanvas` is an abstract base class that combines path creation and flattening capabilities from [[TFlattenedPath]] with coordinate transformation support ([[TTransformation]]).

Key capabilities of `TCustomCanvas` include:
- **Coordinate Transformation**: Applies a [[TTransformation]] instance (such as affine scaling, rotation, translation, or projective transforms) to path vertices during drawing operations.
- **Abstract Path Drawing Contract**: Defines the protected abstract `DrawPath` method, implemented by subclasses (such as [[TCanvas32]]) to rasterize flattened path contours onto rendering targets.

[members]
