---
layout: doc
docType: api
unit: GR32_Resamplers
entity: TCubicKernel
kind: Class
declaration: "TCubicKernel = class(TCustomKernel)"
inheritance:
  - TObject
  - TPersistent
  - TCustomKernel
  - TCubicKernel
summary: "A reconstruction filter described by a cubic polynomial."
---

## Description

`TCubicKernel` implements Keys' cubic convolution interpolation polynomial with configurable sharpening parameter `Coeff` (default $a = -0.5$ for Catmull-Rom spline).

<img src="/images/kernel-window-cubic.svg" alt="Cubic Kernel Window" style="width:100%; max-width:600px; margin:1rem 0;" />

Cubic filtering is commonly used for high-quality resampling.

### Mathematics

With parameter $a = \text{Coeff}$:

$$
f(x) = \begin{cases}
(a + 2)|x|^3 - (a + 3)|x|^2 + 1, & \text{if } |x| \le 1 \\[6pt]
a |x|^3 - 5a |x|^2 + 8a |x| - 4a, & \text{if } 1 < |x| < 2 \\[6pt]
0, & \text{otherwise}
\end{cases}
$$

### References

- Robert G. Keys, *"Cubic convolution interpolation for digital image processing"*, IEEE Transactions on Acoustics, Speech, and Signal Processing, 1981.
- [Bicubic interpolation (Wikipedia)](https://en.wikipedia.org/wiki/Bicubic_interpolation)

[members]
