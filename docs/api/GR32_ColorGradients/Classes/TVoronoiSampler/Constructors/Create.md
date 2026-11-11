---
layout: doc
docType: api
unit: GR32_ColorGradients
parent: TVoronoiSampler
entity: TVoronoiSampler.Create
kind: Constructor
declaration: "constructor Create(Metric: TVoronoiMetric = vmEuclidean); virtual;"
summary: "Initializes a TVoronoiSampler instance with a distance metric."
parameters:
  - name: Metric
    type: TVoronoiMetric
    description: "Distance calculation metric (vmEuclidean, vmManhattan, vmCustom)."
---

## Description

Constructs a Voronoi diagram sampler initialized with distance metric `Metric`.
