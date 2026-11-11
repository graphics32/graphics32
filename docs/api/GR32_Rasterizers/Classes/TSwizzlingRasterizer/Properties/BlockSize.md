---
layout: doc
docType: api
unit: GR32_Rasterizers
parent: TSwizzlingRasterizer
entity: TSwizzlingRasterizer.BlockSize
kind: Property
declaration: "property BlockSize: Integer read FBlockSize write SetBlockSize default 3;"
summary: "Power-of-two exponent specifying the block size used for batch invalidation during swizzled sampling."
---

## Description

`BlockSize` defines the power-of-two exponent determining the block size ($2^{\text{BlockSize}}$ pixels) used by `TSwizzlingRasterizer` to group area change notifications (`Dst.Changed`).

Default value is `3` (block size of $2^3 = 8$ pixels).
