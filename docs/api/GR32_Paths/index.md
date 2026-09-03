---
layout: doc
docType: api
unit: GR32_Paths
entity: GR32_Paths
kind: Unit
summary: "Provides vector path construction, curve flattening algorithms, and vector canvas drawing engine for Graphics32."
---

## Description

The `GR32_Paths` unit defines the core 2D vector graphics path building and canvas rendering framework in Graphics32. It provides a flexible hierarchy of classes for creating, flattening, and rasterizing complex vector shapes, curves, and text outlines onto bitmap pixel buffers.

Key components in `GR32_Paths` include:

- **Path Construction Base Class**: [[TCustomPath]] provides high-level vector drawing directives including absolute and relative line movements, cubic Bezier curves (`CurveTo`), quadratic/conic Bezier curves (`ConicTo`), arcs, rectangles, rounded rectangles, ellipses, circles, polylines, and polygons.
- **Curve Flattening Engine**: [[TFlattenedPath]] automatically tessellates mathematical curves into discrete polygonal vertex contours (`TArrayOfArrayOfFloatPoint`), governed by configurable error tolerances (`CBezierTolerance`, `QBezierTolerance`).
- **Canvas Abstraction**: [[TCustomCanvas]] extends path flattening with coordinate transformation capabilities ([[TTransformation]]).
- **2D Vector Canvas**: [[TCanvas32]] integrates path building with bitmap rendering targets ([[TBitmap32]]), customizable stroke and fill brushes ([[TBrushCollection]]), anti-aliased polygon rasterizers ([[TPolygonRenderer32]]), and vector font text outline rendering (`RenderText`, `MeasureText`).

[members]
