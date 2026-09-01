---
layout: doc
docType: api
unit: GR32_Resamplers
entity: THermiteKernel
kind: Class
declaration: "THermiteKernel = class(TCustomKernel)"
inheritance:
  - TObject
  - TPersistent
  - TCustomKernel
  - THermiteKernel
summary: "Hermite cubic curve filter kernel."
---

## Description

`THermiteKernel` implements a 1D cubic Hermite spline interpolation filter with configurable `Bias` and `Tension` parameters, confined to radius $R = 1.0$..

<img src="/images/kernel-window-hermite.svg" alt="Hermite Kernel Window" style="width:100%; max-width:600px; margin:1rem 0;" />

### Mathematics

Calculates Hermite basis polynomials over range $[-2, 2]$ using
$$
m_0 = \frac{(1-\text{Tension})(1+\text{Bias})}{2}
$$

and 

$$
m_1 = \frac{(1-\text{Tension})(1-\text{Bias})}{2}
$$

### References

- [Cubic Hermite spline (Wikipedia)](https://en.wikipedia.org/wiki/Cubic_Hermite_spline)


[members]
