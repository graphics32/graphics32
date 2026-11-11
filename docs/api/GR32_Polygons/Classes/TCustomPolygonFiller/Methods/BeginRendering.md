---
layout: doc
docType: api
unit: GR32_Polygons
parent: TCustomPolygonFiller
entity: TCustomPolygonFiller.BeginRendering
kind: Method
declaration: "procedure BeginRendering; virtual;"
summary: "Prepares resources before scanline rendering begins."
---

## Description

`BeginRendering` is called by polygon renderers immediately before rasterization of scanline spans begins. Subclasses override this method to prepare internal buffers or validate patterns.
