---
layout: doc
docType: api
unit: GR32_Rasterizers
entity: DefaultRasterizerClass
kind: Variable
declaration: "var DefaultRasterizerClass: TRasterizerClass = TRegularRasterizer;"
summary: "Global variable specifying the default TRasterizer class used for standard sampling operations."
---

## Description

`DefaultRasterizerClass` holds the default [[TRasterizerClass]] metaclass used by high-level rendering operations in Graphics32.

By default, `DefaultRasterizerClass` is initialized to [[TRegularRasterizer]] on single-core systems, or to [[TMultithreadedRegularRasterizer]] when multi-core execution is detected at unit initialization.
