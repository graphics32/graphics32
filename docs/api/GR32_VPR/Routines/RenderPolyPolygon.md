---
layout: doc
docType: api
unit: GR32_VPR
entity: RenderPolyPolygon
kind: Function
summary: "Rasterizes multiple floating-point vector polygon contours into analytical coverage spans."
overloads:
  - signature: "procedure RenderPolyPolygon(const Points: TArrayOfArrayOfFloatPoint; const ClipRect: TFloatRect; const RenderProc: TRenderSpanProc; Data: Pointer = nil); overload;"
    summary: "Rasterizes multi-contour polygons using a procedural callback and optional user context data."
    parameters:
      - name: Points
        type: TArrayOfArrayOfFloatPoint
        description: "Array of polygon contours, where each contour is an array of 2D floating-point vertices."
      - name: ClipRect
        type: TFloatRect
        description: "Bounding rectangle to clip polygon contours against prior to rasterization."
      - name: RenderProc
        type: TRenderSpanProc
        description: "Procedural callback invoked for each rasterized scanline span."
      - name: Data
        type: Pointer
        description: "Optional user context pointer passed directly to RenderProc."

  - signature: "procedure RenderPolyPolygon(const Points: TArrayOfArrayOfFloatPoint; const ClipRect: TFloatRect; const RenderProc: TRenderSpanEvent); overload;"
    summary: "Rasterizes multi-contour polygons using an object method callback event."
    parameters:
      - name: Points
        type: TArrayOfArrayOfFloatPoint
        description: "Array of polygon contours, where each contour is an array of 2D floating-point vertices."
      - name: ClipRect
        type: TFloatRect
        description: "Bounding rectangle to clip polygon contours against prior to rasterization."
      - name: RenderProc
        type: TRenderSpanEvent
        description: "Object method event callback invoked for each rasterized scanline span."
seealso:
  - "[[RenderPolygon]]"
  - "[[TValueSpan]]"
  - "[[TRenderSpanProc]]"
---

## Description

`RenderPolyPolygon` clips and rasterizes multiple 2D vector polygon contours specified by `Points` against `ClipRect`. It is designed for complex polygons, multi-ring shapes, or shapes containing holes.

During execution, VPR:
1. Clips each contour in `Points` against `ClipRect`.
2. Computes global scanline segment allocations across all contours.
3. Subdivides segment edges into scanline-bounded fragments ($Y \in [0, 1]$).
4. Calculates horizontal 1D boundary crossings and cumulative prefix sums.
5. Performs analytical trapezoidal integration per pixel column.
6. Invokes the `RenderProc` callback for each generated scanline span [`TValueSpan`](/api/GR32_VPR/Types/TValueSpan).

For more information on the rendering pipeline, see the [Vectorial Polygon Rasterizer Guide](/guide/vpr).
