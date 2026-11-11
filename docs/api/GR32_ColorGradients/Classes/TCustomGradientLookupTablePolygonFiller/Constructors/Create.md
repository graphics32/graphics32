---
layout: doc
docType: api
unit: GR32_ColorGradients
parent: TCustomGradientLookupTablePolygonFiller
entity: TCustomGradientLookupTablePolygonFiller.Create
kind: Constructor
summary: "Initializes a TCustomGradientLookupTablePolygonFiller instance."
overloads:
  - signature: "constructor Create; reintroduce; overload;"
    summary: "Creates filler with default lookup table."
  - signature: "constructor Create(LookupTable: TColor32LookupTable); overload; virtual;"
    summary: "Creates filler with custom lookup table."
    parameters:
      - name: LookupTable
        type: TColor32LookupTable
        description: "Lookup table instance."
---

## Description

Constructs lookup table polygon filler and initializes `UseLookUpTable = True`.
