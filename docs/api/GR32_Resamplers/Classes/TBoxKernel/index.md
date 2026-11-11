---
layout: doc
docType: api
unit: GR32_Resamplers
entity: TBoxKernel
kind: Class
declaration: "TBoxKernel = class(TCustomKernel)"
inheritance:
  - TObject
  - TPersistent
  - TCustomKernel
  - TBoxKernel
summary: "Box (nearest-neighbor) filter kernel."
---

## Description

`TBoxKernel` implements a nearest-neighbor box filter (also known as a top-hat or rectangular window).

<img src="/images/kernel-window-box.svg" alt="Box Kernel Window" style="width:100%; max-width:600px; margin:1rem 0;" />

### Mathematics

The box filter function is defined mathematically as:

$$
f(x) = \begin{cases}
1, & \text{if } |x| \le 0.5 \\
0, & \text{otherwise}
\end{cases}
$$

The kernel width radius is $W = 0.5$ (reported as $1.0$ full support).

### References

- [Nearest-neighbor interpolation (Wikipedia)](https://en.wikipedia.org/wiki/Nearest-neighbor_interpolation)
- [Box filter (Wikipedia)](https://en.wikipedia.org/wiki/Box_filter)

[members]
