---
layout: doc
docType: api
unit: GR32_ColorGradients
parent: TColor32LookupTable
entity: TColor32LookupTable.Color32Ptr
kind: Property
declaration: "property Color32Ptr: PColor32Array read FGradientLUT;"
summary: "Direct pointer to the underlying array of TColor32 entries."
---

## Description

Returns a direct pointer (`PColor32Array`) to the contiguous array of [[TColor32]] values. High-performance scanline fillers and assembly/SSE routines use `Color32Ptr` for direct pointer arithmetic and unrolled memory loops without property access overhead.
