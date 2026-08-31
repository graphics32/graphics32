---
layout: doc
docType: api
unit: GR32_ColorGradients
parent: TVoronoiSampler
entity: TVoronoiSampler.MetricFunc
kind: Property
declaration: "property MetricFunc: TVoronoiMetricFunc read FMetricFunc write SetMetricFunc;"
summary: "Custom procedural distance metric function delegate when Metric = vmCustom."
---

## Description

Procedural delegate (`TVoronoiMetricFunc`) evaluated to compute distance between sampling coordinate $(X, Y)$ and site point $P_i$ when `Metric = vmCustom`.
