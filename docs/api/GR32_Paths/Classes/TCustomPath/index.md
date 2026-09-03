---
layout: doc
docType: api
unit: GR32_Paths
entity: TCustomPath
kind: Class
abstract: true
declaration: "TCustomPath = class abstract(TThreadPersistent)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TThreadPersistent
  - TCustomPath
summary: "Abstract base class for vector path builders, managing coordinate states and vector path directives."
---

## Description

`TCustomPath` is the abstract base class for vector path construction in Graphics32. It establishes a vector drawing interface inspired by SVG path commands, offering absolute and relative directives for straight lines, cubic Bezier curves, quadratic (conic) Bezier curves, circular arcs, rectangles, rounded rectangles, ellipses, circles, polylines, and polygons.

Key features of `TCustomPath` include:
- **Path Position Management**: Maintains the active drawing coordinate in [[CurrentPoint]] updated by movement, line, and curve commands.
- **Absolute and Relative Commands**: Provides paired absolute (e.g. `LineTo`, `CurveTo`, `ConicTo`) and relative commands (e.g. `LineToRelative`, `CurveToRelative`, `ConicToRelative`).
- **Smooth Curve Continuation**: Automatically reflects control points when chaining smooth cubic (`CurveTo(C2, P)`) or quadratic (`ConicTo(P)`) Bezier segments.
- **Geometric Primitives**: Provides convenient methods for adding shapes such as [[Rectangle]], [[RoundRect]], [[Ellipse]], [[Circle]], [[Polygon]], and [[PolyPolygon]].

Derived classes (such as [[TFlattenedPath]] and [[TCanvas32]]) override internal point generation routines (`AddPoint`, `EndPath`) to convert high-level path commands into flattened polygonal vertex lists or render them directly onto bitmap targets.

[members]
