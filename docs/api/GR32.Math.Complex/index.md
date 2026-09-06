---
layout: doc
docType: api
unit: GR32.Math.Complex
entity: GR32.Math.Complex
kind: Unit
summary: "High-performance complex number arithmetic, polar coordinate conversions, transcendental mathematical functions, and parsing routines."
seealso:
  - "[[TComplex]]"
  - "[Complex Number — Wikipedia](https://en.wikipedia.org/wiki/Complex_number)"
  - "[Euler's Formula — Wikipedia](https://en.wikipedia.org/wiki/Euler%27s_formula)"
  - "[Complex Logarithm — Wikipedia](https://en.wikipedia.org/wiki/Complex_logarithm)"
  - "[Mandelbrot Set — Wikipedia](https://en.wikipedia.org/wiki/Mandelbrot_set)"
---

## Description

The `GR32.Math.Complex` unit provides a 64-bit double-precision complex number engine built around the [[TComplex]] record structure. It features comprehensive operator overloads, polar coordinate transformations, transcendental mathematical functions (trigonometric, inverse trigonometric, hyperbolic, inverse hyperbolic, logarithmic, and exponential), zero-defuzzing mechanisms, and localized string parsing and formatting routines.

---

## Use Cases

Complex arithmetic plays a fundamental role across graphics algorithms, computer vision, mathematical visualization, and scientific computation:

- **Fractal Generation & Rendering**: Computing iterative complex mappings such as the Mandelbrot set ($z_{n+1} = z_n^2 + c$) and Julia sets ($z_{n+1} = z_n^2 + c$) in procedural texture generators and visual canvas filters.
- **2D Conformal Transformations & Mappings**: Performing conformal geometry mappings, Möbius transformations, twirl and distortion filters, and complex planar warping where geometric points are represented as complex numbers $z = x + i y$.
- **Signal Processing & Frequency Domain Analysis**: Executing Fast Fourier Transforms (FFT), phase-angle filtering, and spatial frequency domain image processing algorithms.
- **Vector Rotations & Polar Geometry**: Rotating 2D vectors and interpolating spatial directions using polar representation $z = r e^{i \theta}$ without trigonometric drift.

---

## Mathematical Background

### Complex Numbers & Representation
A complex number $z \in \mathbb{C}$ is composed of a real part $x \in \mathbb{R}$ and an imaginary part $y \in \mathbb{R}$:

$$z = x + i y$$

where $i$ is the imaginary unit defined by $i^2 = -1$. In Cartesian coordinates, $x = \text{Re}(z)$ and $y = \text{Im}(z)$.

### Polar Representation & Euler's Formula
By Euler's formula ($e^{i \theta} = \cos \theta + i \sin \theta$), any complex number can be expressed in polar coordinates:

$$z = r e^{i \theta} = r (\cos \theta + i \sin \theta)$$

where:
- **Modulus (Magnitude)**: $r = |z| = \sqrt{x^2 + y^2}$
- **Phase (Argument)**: $\theta = \arg(z) = \text{atan2}(y, x) \in (-\pi, \pi]$

### Complex Arithmetic Operations
Given $z_1 = x_1 + i y_1$ and $z_2 = x_2 + i y_2$:

- **Addition**: $z_1 + z_2 = (x_1 + x_2) + i (y_1 + y_2)$
- **Subtraction**: $z_1 - z_2 = (x_1 - x_2) + i (y_1 - y_2)$
- **Multiplication**: $z_1 \cdot z_2 = (x_1 x_2 - y_1 y_2) + i (x_1 y_2 + x_2 y_1)$
- **Division**: $\frac{z_1}{z_2} = \frac{(x_1 x_2 + y_1 y_2) + i (y_1 x_2 - x_1 y_2)}{x_2^2 + y_2^2}$
- **Complex Conjugate**: $\overline{z} = x - i y$
- **Multiplicative Inverse**: $z^{-1} = \frac{\overline{z}}{|z|^2} = \frac{x - i y}{x^2 + y^2}$

### Exponential, Logarithmic & Power Functions
- **Exponential**: $e^z = e^{x + iy} = e^x (\cos y + i \sin y)$
- **Natural Logarithm**: $\ln z = \ln |z| + i \arg(z)$
- **Complex Power**: $z^w = e^{w \ln z}$

---

## Precision & Zero-Defuzzing

Due to IEEE 754 floating-point rounding inaccuracies, calculations that should mathematically yield zero (such as $\sin(\pi)$ or $e^{i \pi} + 1$) may produce residual values near $10^{-16}$.

The `TComplex` structure provides built-in zero-defuzzing via the `DefuzzAtZero` class variable. When enabled, `Defuzz` automatically snaps real or imaginary components within floating-point zero tolerance (`Math.IsZero`) back to exact `0.0`, preventing floating-point noise from accumulating during iterative complex calculations.

---

[members]
