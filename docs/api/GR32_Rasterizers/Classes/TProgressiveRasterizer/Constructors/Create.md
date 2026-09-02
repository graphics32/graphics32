---
layout: doc
docType: api
unit: GR32_Rasterizers
parent: TProgressiveRasterizer
entity: TProgressiveRasterizer.Create
kind: Constructor
declaration: "constructor Create; override;"
summary: "Creates and initializes a TProgressiveRasterizer instance with default steps and row update settings."
---

## Description

`Create` initializes a new `TProgressiveRasterizer` instance with [[Steps]] set to `4` (initial $16 \times 16$ block size) and [[UpdateRows]] set to `True`.
