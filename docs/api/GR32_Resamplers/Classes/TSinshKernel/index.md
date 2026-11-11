---
layout: doc
docType: api
unit: GR32_Resamplers
entity: TSinshKernel
kind: Class
declaration: "TSinshKernel = class(TCustomKernel)"
inheritance:
  - TObject
  - TPersistent
  - TCustomKernel
  - TSinshKernel
summary: "Sinsh (hyperbolic sine windowed sinc) filter kernel."
---

## Description

`TSinshKernel` implements a filter combining sine and hyperbolic sine functions for sharp, controlled detail preservation.

<img src="/images/kernel-window-sinsh.svg" alt="Sinsh Kernel Window" style="width:100%; max-width:600px; margin:1rem 0;" />

### Mathematics

Defined as:

$$
f(x) = \begin{cases}
1, & \text{if } x = 0 \\
c \frac{\sin(\pi x)}{\sinh(\pi c x)}, & \text{otherwise}
\end{cases}
$$

where $c = \text{Coeff}$.

[members]
