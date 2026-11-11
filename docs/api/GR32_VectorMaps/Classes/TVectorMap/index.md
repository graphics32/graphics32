---
layout: doc
docType: api
unit: GR32_VectorMaps
entity: TVectorMap
kind: Class
declaration: "TVectorMap = class(TCustomMap)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TThreadPersistent
  - TCustomMap
  - TVectorMap
summary: "2D displacement map class that stores coordinate offset vectors for spatial image transformations and warping."
---

## Description

`TVectorMap` is a 2D map container that stores coordinate displacement vectors ([[TFixedVector]] / [[TFloatVector]]) for each coordinate in a rectangular grid. It is primarily used in Graphics32 to perform non-linear spatial image warping, resampler distortions, and mesh deformations (such as in [[TRemapTransformation]]).

### Purpose

In spatial transformations, a vector map represents an offset field $(\Delta X, \Delta Y)$ for every pixel in a destination surface. When applying a vector map transformation to an image, the sampler inspects the displacement vector at $(X, Y)$ and samples the source image from $(X + \Delta X, Y + \Delta Y)$.

### Typical Population Methods

`TVectorMap` instances are typically populated in one of the following ways:
1. **Rasterizing a Transformation**: Using `RasterizeTransformation` (from `GR32_Transforms`) to evaluate a [[TTransformation]] object across a destination rectangle and record generated displacement vectors.
2. **Procedural / Mathematical Vector Generation**: Writing displacement vectors directly via indexed pixel accessor properties (`FixedVector`, `FloatVector`, `FixedVectorX`, `FloatVectorF`).
3. **Loading Mesh Files**: Importing Adobe Photoshop Liquify mesh files (`.msh`) using [[TVectorMap.LoadFromFile]].
4. **Vector Map Merging**: Combining or layering existing vector map displacement fields using [[TVectorMap.Merge]].

[members]
