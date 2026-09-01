---
layout: doc
docType: api
unit: GR32_Resamplers
parent: TAdaptiveSuperSampler
entity: TAdaptiveSuperSampler.Level
kind: Property
scope: Published
declaration: "property Level: Integer read FLevel write SetLevel;"
summary: "Specifies the maximum recursion depth level for adaptive quadtree subdivision."
---

## Description

`Level` controls the maximum subdivision depth level for adaptive quadtree sampling. Higher levels improve anti-aliasing quality in high-contrast edge areas at the expense of additional processing time.
