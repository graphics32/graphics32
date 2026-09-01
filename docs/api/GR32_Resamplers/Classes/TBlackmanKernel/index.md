---
layout: doc
docType: api
unit: GR32_Resamplers
entity: TBlackmanKernel
kind: Class
declaration: "TBlackmanKernel = class(TWindowedSincKernel)"
inheritance:
  - TObject
  - TPersistent
  - TCustomKernel
  - TWindowedKernel
  - TWindowedSincKernel
  - TBlackmanKernel
summary: "A Sinc kernel constrained by a Blackman window function."
---

## Description

`TBlackmanKernel` multiplies the Sinc function by a Blackman window.

<img src="/images/kernel-window-blackman.svg" alt="Blackman Kernel Window" style="width:100%; max-width:600px; margin:1rem 0;" />

### Mathematics

$$
w(x) = 0.34 + 0.5 \cos\left(\frac{\pi x}{W}\right) + 0.16 \cos^2\left(\frac{\pi x}{W}\right)
$$

### References

- Ralph Beebe Blackman & John Wilder Tukey, *"Particular Pairs of Windows"*, Dover, 1959.
- [Blackman window (Wikipedia)](https://en.wikipedia.org/wiki/Window_function#Blackman_window)
[members]
