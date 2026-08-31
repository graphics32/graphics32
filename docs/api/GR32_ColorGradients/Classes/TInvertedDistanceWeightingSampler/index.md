---
layout: doc
docType: api
unit: GR32_ColorGradients
entity: TInvertedDistanceWeightingSampler
kind: Class
summary: "Shepard's Inverse Distance Weighting sampler."
declaration: "TInvertedDistanceWeightingSampler = class(TCustomArbitrarySparsePointGradientSampler)"
inheritance:
  - TObject
  - TPersistent
  - TNotifiablePersistent
  - TCustomSampler
  - TCustomSparsePointGradientSampler
  - TCustomArbitrarySparsePointGradientSampler
  - TInvertedDistanceWeightingSampler
---

## Description

`TInvertedDistanceWeightingSampler` samples smooth multivariate color gradients across scattered points using Shepard's Inverse Distance Weighting (IDW).

<!-- TODO: more description -->

| Power=1 | Power=2 | Power=4 | Power=8 | 
| --- | --- | --- | --- |
| ![](/images/gradient-sampler-inverted-distance-weighting-power1.png)| ![](/images/gradient-sampler-inverted-distance-weighting-power2.png)| ![](/images/gradient-sampler-inverted-distance-weighting-power4.png)| ![](/images/gradient-sampler-inverted-distance-weighting-power8.png) |

## Mathematics & Algorithm

Given $N$ scattered control points $P_i$ with colors $C_i$, the interpolated color $C(P)$ at point $P = (X, Y)$ is computed using Shepard's method with power parameter $p$ (`Power`):

$$w_i(P) = \frac{1}{d(P, P_i)^p} = \frac{1}{\|(X, Y) - (X_i, Y_i)\|^p}$$

$$C(P) = \frac{\sum_{i=1}^N w_i(P) C_i}{\sum_{i=1}^N w_i(P)}$$

If $P = P_i$, then $C(P) = C_i$.

## References
- [Inverse Distance Weighting - Wikipedia](https://en.wikipedia.org/wiki/Inverse_distance_weighting)

[members]
