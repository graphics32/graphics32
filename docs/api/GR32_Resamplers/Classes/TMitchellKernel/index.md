---
layout: doc
docType: api
unit: GR32_Resamplers
entity: TMitchellKernel
kind: Class
declaration: "TMitchellKernel = class(TCustomKernel)"
inheritance:
  - TObject
  - TPersistent
  - TCustomKernel
  - TMitchellKernel
summary: "Mitchell-Netravali cubic filter kernel (B=1/3, C=1/3)."
---

## Description

`TMitchellKernel` implements the Mitchell-Netravali cubic filter optimized for visual quality with parameters $B = \frac{1}{3}$ and $C = \frac{1}{3}$.
It strikes an optimal visual balance between blurring, ringing, and aliasing artifacts.

<img src="/images/kernel-window-mitchell.svg" alt="Mitchell Kernel Window" style="width:100%; max-width:600px; margin:1rem 0;" />

### Mathematics

With $B=\frac{1}{3}$ and $C=\frac{1}{3}$, the Mitchell-Netravali piecewise cubic function $f(x)$ is defined as:

$$
f(x) = \begin{cases}
\frac{1}{18} (21 |x|^3 - 36 |x|^2 + 16), & \text{if } |x| < 1 \\[6pt]
\frac{1}{18} (-7 |x|^3 + 36 |x|^2 - 60 |x| + 32), & \text{if } 1 \le |x| < 2 \\[6pt]
0, & \text{otherwise}
\end{cases}
$$

::: info
Many other variants of this filter, with various other values for B&C, exist.
Often people come up with some variation of B&C and then put their own name on the filter. For example Robidoux (B:0.3782, C:0.3109), etc.
:::

### References

- Don P. Mitchell & Arun N. Netravali, *"Reconstruction Filters in Computer Graphics"*, ACM SIGGRAPH Computer Graphics, Volume 22, Number 4, August 1988.

[members]
