---
layout: doc
docType: api
unit: GR32_Resamplers
entity: TSplineKernel
kind: Class
declaration: "TSplineKernel = class(TCustomKernel)"
inheritance:
  - TObject
  - TPersistent
  - TCustomKernel
  - TSplineKernel
summary: "B-Spline cubic filter kernel (B=1, C=0)."
---

## Description

`TSplineKernel` implements a smooth cubic $B$-spline filter.

<img src="/images/kernel-window-spline.svg" alt="Spline Kernel Window" style="width:100%; max-width:600px; margin:1rem 0;" />

### Mathematics

The cubic $B$-spline function is defined piecewise over $|x| < 2$:

$$
f(x) = \begin{cases}
\frac{1}{2} |x|^3 - |x|^2 + \frac{2}{3}, & \text{if } |x| < 1 \\[6pt]
\frac{1}{6} (2 - |x|)^3, & \text{if } 1 \le |x| < 2 \\[6pt]
0, & \text{otherwise}
\end{cases}
$$

The effective radius is $W = 2.0$.

### References

- [B-spline (Wikipedia)](https://en.wikipedia.org/wiki/B-spline)

[members]
