---
layout: doc
docType: api
unit: GR32_Polygons
parent: TClearPolygonFiller
entity: TClearPolygonFiller.Create
kind: Constructor
declaration: "constructor Create(Color: TColor32 = $00808080); reintroduce; virtual;"
summary: "Initializes a new TClearPolygonFiller with a specified solid color."
parameters:
  - name: Color
    type: TColor32
    description: "Solid 32-bit ARGB fill color (defaults to $00808080)."
---

## Description

`Create` instantiates a `TClearPolygonFiller` and sets its target fill `Color`.
