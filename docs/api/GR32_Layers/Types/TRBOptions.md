---
layout: doc
docType: api
unit: GR32_Layers
entity: TRBOptions
kind: Type
declaration: "TRBOptions = set of (roProportional, roConstrained, roQuantized);"
summary: "Behavioral options for rectangular rubberband layer resizing."
aliases: [roProportional, roConstrained, roQuantized]
---

## Description

`TRBOptions` configures constraint and aspect-ratio behavior during rubberband layer resizing.

## Values

| Value | Description |
| --- | --- |
| `roProportional` | Maintains original aspect ratio when resizing. |
| `roConstrained` | Triggers [[TRubberbandLayer.OnConstrain|OnConstrain]] event during resizing to enforce custom position bounds. |
| `roQuantized` | Snaps handle positions and layer dimensions to discrete [[Quantized]] grid steps. |
