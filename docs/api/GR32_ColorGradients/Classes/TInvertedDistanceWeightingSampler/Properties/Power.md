---
layout: doc
docType: api
unit: GR32_ColorGradients
parent: TInvertedDistanceWeightingSampler
entity: TInvertedDistanceWeightingSampler.Power
kind: Property
declaration: "property Power: TFloat read FPower write FPower;"
summary: "Power exponent parameter p used in Shepard inverse distance weighting formula."
---

## Description

Specifies power exponent $p$ in $w_i = \frac{1}{d(P, P_i)^p}$. Values $p > 2$ create sharper localized color pools around control points, while values $p < 1$ produce softer global color diffusion.
