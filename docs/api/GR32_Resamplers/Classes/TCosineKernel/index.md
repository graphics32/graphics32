---
layout: doc
docType: api
unit: GR32_Resamplers
entity: TCosineKernel
kind: Class
summary: "Spatial filter kernel implementing smooth cosine-shaped interpolation."
declaration: "TCosineKernel = class(TCustomKernel)"
inheritance:
  - TObject
  - TPersistent
  - TCustomKernel
  - TCosineKernel
---

## Description

`TCosineKernel` implements a smooth cosine-shaped interpolation curve.

<img src="/images/kernel-window-cosine.svg" alt="Cosine Kernel Window" style="width:100%; max-width:600px; margin:1rem 0;" />

### Mathematics

The cosine filter function is defined as:

$$
f(x) = \begin{cases}
\frac{\cos(\pi x) + 1}{2}, & \text{if } |x| < 1 \\
0, & \text{otherwise}
\end{cases}
$$

The effective radius is $W = 1.0$.

[members]
