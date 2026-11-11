---
layout: doc
docType: api
unit: GR32_Resamplers
entity: TLanczosKernel
kind: Class
declaration: "TLanczosKernel = class(TWindowedSincKernel)"
inheritance:
  - TObject
  - TPersistent
  - TCustomKernel
  - TWindowedKernel
  - TWindowedSincKernel
  - TLanczosKernel
summary: "A Sinc kernel constrained by a Lanczos window function."
---

## Description

`TLanczosKernel` implements a Sinc filter multiplied by a Sinc windowing lobe (commonly Lanczos3 with $W = 3.0$).

<img src="/images/kernel-window-lanczos.svg" alt="Lanczos Kernel Window" style="width:100%; max-width:600px; margin:1rem 0;" />

### Mathematics

$$
f(x) = \begin{cases}
\text{sinc}(x) \text{sinc}\left(\frac{x}{W}\right), & \text{if } |x| < W \\
0, & \text{otherwise}
\end{cases}
$$

### References

- Claude E. Duchon, *"Lanczos Filtering in One and Two Dimensions"*, Journal of Applied Meteorology, 1979.
- Turkowski, K. (1990). *Filters for Common Resampling Tasks*. Graphics Gems.
- [Lanczos resampling (Wikipedia)](https://en.wikipedia.org/wiki/Lanczos_resampling)

[members]
