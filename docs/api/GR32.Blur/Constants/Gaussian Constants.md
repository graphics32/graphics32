---
layout: doc
docType: api
unit: GR32.Blur
entity: Gaussian Constants
aliases: [GaussianRadiusToSigma, GaussianSigmaToRadius]
kind: Constant
declaration: |
  const GaussianRadiusToSigma = 0.300386630413846;
  const GaussianSigmaToRadius = 1 / GaussianRadiusToSigma;
summary: "Conversion constants between Gaussian standard deviation (Sigma) and effective pixel blur radius."
seealso:
  - "[[Blur32]]"
  - "[[GammaBlur32]]"
---

## Description

The `GaussianRadiusToSigma` and `GaussianSigmaToRadius` constants define the mathematical conversion ratio between the Gaussian kernel standard deviation $\sigma$ (Sigma) and the effective pixel radius $R$.

Because a theoretical Gaussian curve extends infinitely, Graphics32 clips the tail where kernel weights fall below one pixel precision threshold.

### Conversion Formulas

* **Radius to Sigma**:
  $$\sigma = R \cdot \text{GaussianRadiusToSigma}$$

* **Sigma to Radius**:
  $$R = \sigma \cdot \text{GaussianSigmaToRadius}$$

### Summary Table

| Constant | Value | Description |
| --- | --- | --- |
| `GaussianRadiusToSigma` | $0.300386630413846$ | Multiplier to convert pixel blur radius $R$ to Gaussian standard deviation $\sigma$. |
| `GaussianSigmaToRadius` | $3.32904308528...$ | Multiplier to convert Gaussian standard deviation $\sigma$ to pixel blur radius $R$. |
