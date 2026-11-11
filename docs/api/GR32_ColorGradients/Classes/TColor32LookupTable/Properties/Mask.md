---
layout: doc
docType: api
unit: GR32_ColorGradients
parent: TColor32LookupTable
entity: TColor32LookupTable.Mask
kind: Property
declaration: "property Mask: Cardinal read FMask;"
summary: "Bitmask for fast modulo wrapping of lookup indices."
---

## Description

Read-only property returning $	ext{Size} - 1$. Because lookup table sizes are strictly powers of two ($2^{	ext{Order}}$), scanline rendering routines use `Mask` to perform fast modulo index wrapping (`Index and Mask`) without executing expensive integer division (`mod`) operations.
