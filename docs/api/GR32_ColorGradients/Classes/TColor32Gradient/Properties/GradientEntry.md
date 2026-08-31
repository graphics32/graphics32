---
layout: doc
docType: api
unit: GR32_ColorGradients
parent: TColor32Gradient
entity: TColor32Gradient.GradientEntry
kind: Property
declaration: "property GradientEntry[Index: Integer]: TColor32GradientStop read GetGradientEntry;"
summary: "Provides indexed read access to individual TColor32GradientStop records."
---

## Description

Indexed array property providing read access to color stop records ordered by ascending offset. Indices range from `0` to `GradientCount - 1`.
