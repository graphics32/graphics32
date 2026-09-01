---
layout: doc
docType: api
unit: GR32_Resamplers
entity: TLinearKernel
kind: Class
declaration: "TLinearKernel = class(TCustomKernel)"
inheritance:
  - TObject
  - TPersistent
  - TCustomKernel
  - TLinearKernel
summary: "Linear (tent / triangle) filter kernel."
---

## Description

`TLinearKernel` implements a linear interpolation filter. Also known as triangle filter, tent function, roof function, Chateau function, or Bartlett window.

<img src="/images/kernel-window-linear.svg" alt="Linear Kernel Window" style="width:100%; max-width:600px; margin:1rem 0;" />

### Mathematics

The linear triangle filter is defined as:

$$
f(x) = \begin{cases}
1 - |x|, & \text{if } |x| < 1 \\
0, & \text{otherwise}
\end{cases}
$$

The effective radius is $W = 1.0$.

### References

- [Triangular function (Wikipedia)](https://en.wikipedia.org/wiki/Triangular_function)
- [Bilinear interpolation (Wikipedia)](https://en.wikipedia.org/wiki/Bilinear_interpolation)

[members]
