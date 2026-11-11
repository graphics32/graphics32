---
layout: doc
docType: api
unit: GR32_Rasterizers
entity: NumberOfProcessors
kind: Variable
declaration: "var NumberOfProcessors: Integer = 1;"
summary: "Global variable storing the number of detected logical CPU processor cores available for parallel rasterization."
---

## Description

`NumberOfProcessors` holds the number of CPU core units detected during unit initialization.

It is used by multi-threaded rasterizers (such as [[TThreadRegularRasterizer]], [[TTaskRegularRasterizer]], and [[TParallelRegularRasterizer]]) to partition scanlines across available hardware threads.
