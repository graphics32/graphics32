---
layout: doc
docType: api
unit: GR32_VPR
entity: RenderPolygon
kind: Function
summary: "Rasterizes a single floating-point vector polygon into analytical coverage spans."
overloads:
  - signature: "procedure RenderPolygon(const Points: TArrayOfFloatPoint; const ClipRect: TFloatRect; const RenderProc: TRenderSpanProc; Data: Pointer = nil); overload;"
    summary: "Rasterizes a single polygon using a procedural callback and optional user context data."
    parameters:
      - name: Points
        type: TArrayOfFloatPoint
        description: "Array of 2D floating-point vertices defining the polygon outline."
      - name: ClipRect
        type: TFloatRect
        description: "Bounding rectangle to clip the polygon against prior to rasterization."
      - name: RenderProc
        type: TRenderSpanProc
        description: "Procedural callback invoked for each rasterized scanline span."
      - name: Data
        type: Pointer
        description: "Optional user context pointer passed directly to RenderProc."

  - signature: "procedure RenderPolygon(const Points: TArrayOfFloatPoint; const ClipRect: TFloatRect; const RenderProc: TRenderSpanEvent); overload;"
    summary: "Rasterizes a single polygon using an object method callback event."
    parameters:
      - name: Points
        type: TArrayOfFloatPoint
        description: "Array of 2D floating-point vertices defining the polygon outline."
      - name: ClipRect
        type: TFloatRect
        description: "Bounding rectangle to clip the polygon against prior to rasterization."
      - name: RenderProc
        type: TRenderSpanEvent
        description: "Object method event callback invoked for each rasterized scanline span."
seealso:
  - "[[RenderPolyPolygon]]"
  - "[[TValueSpan]]"
  - "[[TRenderSpanProc]]"
---

## Description

`RenderPolygon` clips and rasterizes a single 2D vector polygon contour specified by `Points` against `ClipRect`.

During execution, VPR:
1. Clips `Points` against `ClipRect`.
2. Subdivides segment edges into scanline-bounded fragments ($Y \in [0, 1]$).
3. Calculates horizontal 1D boundary crossings and cumulative prefix sums.
4. Performs analytical trapezoidal integration per pixel column.
5. Invokes the `RenderProc` callback for each generated scanline span [[TValueSpan]].

For more information on the rendering pipeline, see the [Vectorial Polygon Rasterizer](/guide/vpr) guide.
