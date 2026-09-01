---
layout: doc
docType: api
unit: GR32_Resamplers
entity: TWindowedKernel
kind: Class
abstract: true
declaration: "TWindowedKernel = class(TCustomKernel)"
inheritance:
  - TObject
  - TPersistent
  - TPlainInterfacedPersistent
  - TNotifiablePersistent
  - TCustomKernel
  - TWindowedKernel
summary: "Abstract base class for windowed filter kernels."
---

## Description

`TWindowedKernel` serves as the base class for windowed filter kernels. It constrains an arbitrary filter response inside a window function $w(x)$ over radius $W$:

$$
f(x) = \begin{cases}
w(x), & \text{if } |x| < W \\
0, & \text{otherwise}
\end{cases}
$$

::: info
Derived classes must override the [[Window]] method in order to implement a custom window function.
:::

[members]
