---
layout: doc
docType: api
unit: GR32_Polygons
parent: TPolygonRenderer32
entity: TPolygonRenderer32.Filler
kind: Property
declaration: "property Filler: TCustomPolygonFiller read FFiller write SetFiller;"
summary: "Optional custom span filler used to generate per-pixel scanline colors."
---

## Description

`Filler` references an optional `TCustomPolygonFiller` instance (such as a gradient filler, bitmap pattern filler, or sampler filler). When set, the renderer delegates span pixel generation to the filler instead of using solid `Color`.
