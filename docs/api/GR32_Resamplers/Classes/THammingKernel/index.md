---
layout: doc
docType: api
unit: GR32_Resamplers
entity: THammingKernel
kind: Class
declaration: "THammingKernel = class(TWindowedSincKernel)"
inheritance:
  - TObject
  - TPersistent
  - TCustomKernel
  - TWindowedKernel
  - TWindowedSincKernel
  - THammingKernel
summary: "A Sinc kernel constrained by a Hamming window function."
---

## Description

`THammingKernel` implements a sinc filter modulated by a Hamming window function.

<img src="/images/kernel-window-hamming.svg" alt="Hamming Kernel Window" style="width:100%; max-width:600px; margin:1rem 0;" />

### Mathematics

$$
w(x) = 0.54 + 0.46 \cos\left(\frac{\pi x}{W}\right)
$$

The overall filter function is $f(x) = \text{sinc}(x) \cdot w(x)$ for $|x| \le N$.

### References

- Richard W. Hamming, *"Digital Filters"*, Prentice-Hall, 1977.
- [Window function - Hamming window (Wikipedia)](https://en.wikipedia.org/wiki/Window_function#Hamming_window)

[members]
