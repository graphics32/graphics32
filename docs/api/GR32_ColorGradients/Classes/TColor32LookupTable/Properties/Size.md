---
layout: doc
docType: api
unit: GR32_ColorGradients
parent: TColor32LookupTable
entity: TColor32LookupTable.Size
kind: Property
declaration: "property Size: Cardinal read FSize;"
summary: "Total number of TColor32 entries in the lookup table."
---

## Description

Read-only property returning the total number of entries in the color lookup table, calculated as $2^{	ext{Order}}$. For example, when `Order = 9`, `Size` returns `512`.
