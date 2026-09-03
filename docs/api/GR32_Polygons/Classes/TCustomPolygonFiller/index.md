---
layout: doc
docType: api
unit: GR32_Polygons
entity: TCustomPolygonFiller
kind: Class
abstract: true
declaration: "TCustomPolygonFiller = class abstract"
summary: "Abstract base class for custom polygon scanline fillers."
---

## Description

`TCustomPolygonFiller` is the abstract base class for custom scanline span fillers.<br>
Derived classes implement custom pixel generation logic for polygon rendering, such as bitmap patterns ([[TBitmapPolygonFiller]]), gradient patterns, custom shaders, or arbitrary samplers ([[TSamplerFiller]]).

---

[members]
