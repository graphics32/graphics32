---
layout: doc
docType: api
unit: GR32_Resamplers
entity: TAlbrechtKernel
kind: Class
declaration: "TAlbrechtKernel = class(TWindowedSincKernel)"
inheritance:
  - TObject
  - TPersistent
  - TCustomKernel
  - TWindowedKernel
  - TWindowedSincKernel
  - TAlbrechtKernel
summary: "A Sinc kernel constrained by Albrecht window functions."
---

## Description

`TAlbrechtKernel` applies Albrecht cosine-sum windows to the ideal Sinc filter.

<img src="/images/kernel-window-albrecht.svg" alt="Albrecht Kernel Window" style="width:100%; max-width:600px; margin:1rem 0;" />

### Mathematics

The window function uses a cosine sum with precalculated minimum-sidelobe coefficients:

$$
w(x) = \sum_{k=0}^{N-1} a_k \cos\left(\frac{k \pi x}{W}\right)
$$

for $|x| \le W$.

### References

- Hans-Helge Albrecht, *"A family of cosine-sum windows for high resolution measurements"*, IEEE ICASSP, 2001.

[members]
