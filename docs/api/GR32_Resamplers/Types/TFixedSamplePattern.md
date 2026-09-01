---
layout: doc
docType: api
unit: GR32_Resamplers
entity: TFixedSamplePattern
kind: Type
aliases: [TFloatSamplePattern]
summary: "Two-dimensional array of point offsets defining sample pattern grids for pattern-based sampling."
declaration: |
  TFloatSamplePattern = array of array of TArrayOfFloatPoint;
  TFixedSamplePattern = array of array of TArrayOfFixedPoint;
---

## Description

`TFixedSamplePattern` and `TFloatSamplePattern` define 2D tiled sample offset grids used by [[TPatternSampler]] and initialized via routines such as [[CreateJitteredPattern]].
