---
layout: doc
docType: api
unit: GR32_Resamplers
entity: THannKernel
kind: Class
declaration: "THannKernel = class(TWindowedSincKernel)"
inheritance:
  - TObject
  - TPersistent
  - TCustomKernel
  - TWindowedKernel
  - TWindowedSincKernel
  - THannKernel
summary: "A Sinc kernel constrained by a Hann window function."
---

## Description

`THannKernel` implements a sinc filter modulated by a Hann (raised cosine) window function.

<img src="/images/kernel-window-hann.svg" alt="Hann Kernel Window" style="width:100%; max-width:600px; margin:1rem 0;" />

### Mathematics

The Hann window function $w(x)$ for window radius $W$ is defined as:

$$
w(x) = 0.5 + 0.5 \cos\left(\frac{\pi x}{W}\right)
$$

The overall filter function is $f(x) = \text{sinc}(x) \cdot w(x)$ for $|x| \le W$.

### References

- Ralph Beebe Blackman & John Wilder Tukey, "The measurement of power spectra from the point of view of communications engineering — Part I.", The Bell System Technical Journal. 37 (1), 1958.
- [Hann function (Wikipedia)](https://en.wikipedia.org/wiki/Hann_function)

[members]
