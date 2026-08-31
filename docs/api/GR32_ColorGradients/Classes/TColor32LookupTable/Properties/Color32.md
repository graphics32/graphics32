---
layout: doc
docType: api
unit: GR32_ColorGradients
parent: TColor32LookupTable
entity: TColor32LookupTable.Color32
kind: Property
declaration: "property Color32[Index: Integer]: TColor32 read GetColor32 write SetColor32;"
summary: "Provides indexed array access to individual TColor32 values in the table."
---

## Description

Default indexed array property providing direct read and write access to individual 32-bit ARGB color entries in the lookup buffer. Indices range from `0` to `Size - 1`.
