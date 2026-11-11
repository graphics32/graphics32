---
layout: doc
docType: api
unit: GR32_Paths
entity: QBezierTolerance
kind: Variable
declaration: "var QBezierTolerance: TFloat = DefaultBezierTolerance;"
summary: "Global pixel tolerance threshold for flattening quadratic (conic) Bezier curves into line segments."
---

## Description

`QBezierTolerance` controls the flatness error tolerance (in pixels) applied when subdividing quadratic (conic) Bezier curves (`ConicTo`) into line segments in [[TCustomPath]].

It defaults to [[DefaultBezierTolerance]] (`0.25`). Lowering this value produces finer curve approximations during vector path tessellation.
