---
layout: doc
docType: api
unit: GR32_Resamplers
entity: TGaussianKernel
kind: Class
declaration: "TGaussianKernel = class(TWindowedKernel)"
inheritance:
  - TObject
  - TPersistent
  - TCustomKernel
  - TWindowedKernel
  - TGaussianKernel
summary: "A kernel constrained by a Gaussian window function."
---

## Description

`TGaussianKernel` implements a Gaussian spatial reconstruction kernel.

<img src="/images/kernel-window-gaussian.svg" alt="Gaussian Kernel Window" style="width:100%; max-width:600px; margin:1rem 0;" />

### Mathematics

The normalized Gaussian filter is defined as:

$$
f(x) = \frac{1}{\sigma \sqrt{2\pi}} \exp\left(-\frac{x^2}{2\sigma^2}\right)
$$

### References

- [Gaussian function (Wikipedia)](https://en.wikipedia.org/wiki/Gaussian_function)
- [Gaussian blur (Wikipedia)](https://en.wikipedia.org/wiki/Gaussian_blur)

[members]
