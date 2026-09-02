---
layout: doc
docType: api
unit: GR32_Rasterizers
entity: TTesseralRasterizer
kind: Class
declaration: "TTesseralRasterizer = class(TRasterizer)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TThreadPersistent
  - TRasterizer
  - TTesseralRasterizer
summary: "Recursive divide-and-conquer rasterizer that hierarchically subdivides rectangles into smaller blocks."
---

## Description

`TTesseralRasterizer` implements a recursive divide-and-conquer sampling order. It splits the destination rectangle along its longest axis (vertically or horizontally), samples cross-section dividing lines, fires incremental area update notifications, and recursively subdivides child sub-rectangles until single-pixel blocks remain.

This hierarchical tesseleration reveals structural outlines and major features across the target image early in the rendering process.

[members]
