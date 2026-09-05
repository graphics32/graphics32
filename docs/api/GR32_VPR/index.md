---
layout: doc
docType: api
unit: GR32_VPR
entity: GR32_VPR
kind: Unit
summary: "Vectorial Polygon Rasterizer engine providing low-level analytical anti-aliased coverage rendering."
---

## Description

The `GR32_VPR` unit implements the core **Vectorial Polygon Rasterizer (VPR)** algorithm for Graphics32. Designed by Mattias Andersson, VPR is an analytical coverage-based polygon rasterization engine that computes exact sub-pixel area coverage for vector polygon edges without the performance bottlenecks of traditional scanline edge-sorting algorithms.

### Use Case & Architecture

`GR32_VPR` provides low-level, high-performance rasterization functions (`RenderPolygon` and `RenderPolyPolygon`) that convert vector polygon contours (`TArrayOfFloatPoint` or `TArrayOfArrayOfFloatPoint`) into horizontal coverage spans (`TValueSpan`).

This unit is primarily used as the underlying rasterization engine for:
- [[TPolygonRenderer32VPR]] and related vector polygon renderers in the [[GR32_Polygons]] unit.
- Custom rasterization pipelines requiring direct access to analytical anti-aliasing coverage values per scanline.

For a detailed technical explanation of the pipeline, mathematical integration of line segments, and cumulative prefix sum architecture, see the [Vectorial Polygon Rasterizer](/guide/vpr) guide.

[members]
