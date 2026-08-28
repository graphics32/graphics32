---
layout: doc
docType: api
unit: GR32_Polygons
parent: TPolygonRenderer32
entity: TPolygonRenderer32.Create
kind: Constructor
declaration: "constructor Create(Bitmap: TCustomBitmap32; Fillmode: TPolyFillMode = pfWinding); reintroduce; overload;"
summary: "Initializes a new instance of TPolygonRenderer32 bound to a destination bitmap."
parameters:
  - name: Bitmap
    type: TCustomBitmap32
    description: "Destination bitmap for polygon rasterization."
  - name: Fillmode
    type: TPolyFillMode
    description: "Initial fill mode rule (defaults to pfWinding)."
---

## Description

`Create` instantiates a `TPolygonRenderer32` object and assigns the specified target `Bitmap` and `Fillmode`.
