---
layout: doc
docType: api
unit: GR32_Paths
entity: CBezierTolerance
kind: Variable
declaration: "var CBezierTolerance: TFloat = DefaultBezierTolerance;"
summary: "Global pixel tolerance threshold for flattening cubic Bezier curves into line segments."
---

## Description

`CBezierTolerance` controls the flatness error tolerance (in pixels) applied when subdividing cubic Bezier curves (`CurveTo`) into line segments in [[TCustomPath]].

It defaults to [[DefaultBezierTolerance]] (`0.25`). Lowering this value increases curve smoothness for high-resolution output or zoom levels, while increasing it improves flattening performance.
