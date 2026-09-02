---
layout: doc
docType: api
unit: GR32_Rasterizers
entity: TContourRasterizer
kind: Class
declaration: "TContourRasterizer = class(TRasterizer)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TThreadPersistent
  - TRasterizer
  - TContourRasterizer
summary: "Rasterizer that tracks and follows paths of similar color intensity across the sample surface."
---

## Description

`TContourRasterizer` evaluates samples by following paths of minimal color intensity difference (contours) across adjacent pixels.

Starting at an initial unvisited coordinate, it evaluates neighboring forward, left, and right pixels, navigating along paths where color change is minimized. Once a contour branch dead-ends or encounters previously visited pixels, it scans for the next unvisited coordinate and repeats until the destination rectangle is fully sampled.

[members]
