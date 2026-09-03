---
layout: doc
docType: api
unit: GR32_Paths
entity: DefaultBezierTolerance
kind: Constant
declaration: "const DefaultBezierTolerance = 0.25;"
summary: "Default maximum allowable distance error in pixels when flattening Bezier curves into line segments."
---

## Description

`DefaultBezierTolerance` defines the default error tolerance (in pixels) used when tessellating cubic and quadratic Bezier curves into straight line segments in [[TCustomPath.CurveTo]] and [[TCustomPath.ConicTo]].

A smaller tolerance value yields higher curve fidelity with more generated line segments, while a larger tolerance yields faster rendering with fewer segments.
