---
layout: doc
docType: api
unit: GR32_ColorGradients
parent: TColor32LookupTable
entity: TColor32LookupTable.Create
kind: Constructor
declaration: "constructor Create(Order: Byte = 9); virtual;"
summary: "Initializes a new TColor32LookupTable instance with a specified bit-power size."
parameters:
  - name: Order
    type: Byte
    description: "Bit-shift power exponent determining the number of lookup table entries (Size = 2^Order). Default is 9 (512 entries)."
---

## Description

`Create` instantiates `TColor32LookupTable` and allocates a contiguous memory buffer of `Size = 2^Order` [[TColor32]] entries. It initializes `Mask = Size - 1` for fast bitwise index modulo operations and sets the `Color32Ptr` pointer to the start of the table buffer.

Default `Order = 9` allocates a 512-entry color array, balancing color sampling accuracy and cache efficiency.
