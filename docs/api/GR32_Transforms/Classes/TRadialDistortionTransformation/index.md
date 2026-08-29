---
layout: doc
docType: api
unit: GR32_Transforms
entity: TRadialDistortionTransformation
kind: Class
declaration: "TRadialDistortionTransformation = class(TTransformation)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TTransformation
  - TRadialDistortionTransformation
summary: "Models optical lens radial distortions (barrel and pincushion) using polynomial coefficients."
---

## Description

`TRadialDistortionTransformation` models optical lens radial distortions (such as barrel or pincushion distortion) using polynomial radial coefficients (`Coefficient1`, `Coefficient2`) and scaling (`Scale`).

The transformation uses the formula of the [Brown-Conrady model](https://en.wikipedia.org/wiki/Distortion_(optics)#Software_correction):

$$p_{\text{dst}} = p_{\text{center}} + (p_{\text{src}} - p_{\text{center}}) \cdot (K_1 \cdot r^2 + K_2 \cdot r^4)$$
$$r = |p_{\text{src}} - p_{\text{center}}|$$
Where:

- **$p_{\text{center}}$** is the focal point of the distortion, normally in the middle of the source image (in the current implementation it is fixed to the middle).
- **$p_{\text{src}}$** is a point of the source image.
- **$p_{\text{dst}}$** is the transformed point.

The two coefficients [[Coefficient1|$K_1$]] and [[Coefficient2|$K_2$]] depend on the zoom and physical characteristics of the optical lenses used. They are usually determined such that straight lines in the scene seen through the lenses, which appear as bent lines because of the distortion, map to straight lines again after applying this transformation.

This transformation implements both the forward and backward transform. The forward transform is given by the formula above. The backward transform requires inversion of a 4th degree polynomial, which is a nonlinear function. Because of this a discrete map is used. The number of elements of this map can be set using MapElements.

[members]
