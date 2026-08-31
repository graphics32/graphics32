---
layout: doc
docType: api
unit: GR32_ColorGradients
parent: TColor32LookupTable
entity: TColor32LookupTable.Order
kind: Property
declaration: "property Order: Byte read FOrder write SetOrder;"
summary: "Bit power exponent defining lookup table entry capacity (Size = 2^Order)."
---

## Description

`Order` controls the entry capacity of the color lookup table.

When `Order` is modified:
1. The internal buffer is reallocated to $2^{	ext{Order}}$ entries.
2. `Size` is updated to $2^{	ext{Order}}$.
3. `Mask` is updated to $	ext{Size} - 1$.
4. `OnOrderChanged` event is triggered to notify samplers or fillers to refresh pre-calculated lookup tables.

Typical values range from `8` (256 entries) to `10` (1024 entries).
