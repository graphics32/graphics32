---
layout: doc
docType: api
unit: GR32.Math.Complex
parent: TComplex
entity: TComplex Mathematical Functions
kind: Method
summary: "Comprehensive suite of mathematical, polar, trigonometric, hyperbolic, exponential, logarithmic, and string parsing functions for TComplex."
aliases: [From, Parse, ToString, IsZero, IsInfinite, IsComplexInfinite, IsNaN, Modulus, Phase, Abs, AbsSqr, Angle, Sign, Conjugate, Inverse, Frac, Int, Compare, Equals, Exp, Ln, Log10, Log2, LogN, Sqr, Sqrt, Power, Cos, Sin, Tan, Cot, Csc, Sec, ArcCos, ArcSin, ArcTan, ArcCot, ArcCsc, ArcSec, CosH, SinH, TanH, CotH, CscH, SecH, ArcCosH, ArcSinH, ArcTanH, ArcCotH, ArcCscH, ArcSecH]
---

## Description

The `TComplex` record structure provides a complete collection of static methods and instance functions for complex mathematical operations, polar transformations, transcendental function evaluation, and culture-aware string serialization.

---

## Construction, Parsing & Formatting

| Function | Signature | Description |
| --- | --- | --- |
| `From` | `class function From(const AReal: Double): TComplex; overload; static;` | Constructs a complex number $a + 0i$ from a real scalar value. |
| `From` | `class function From(const AReal, AImaginary: Double): TComplex; overload; static;` | Constructs a complex number $a + bi$ from real and imaginary parts. |
| `FromPolar` | `class function FromPolar(const AModulus, APhase: Double): TComplex; static;` | Constructs a complex number from polar coordinates $r e^{i \theta} = r \cos \theta + i r \sin \theta$. |
| `Parse` | `class function Parse(const AText: string): TComplex; overload; static;` | Parses a complex number string using default format settings. |
| `Parse` | `class function Parse(const AText: string; const AFormatSettings: TFormatSettings): TComplex; overload; static;` | Parses a complex number string using specified culture format settings. |
| `ToString` | `function ToString: string; overload;` | Formats the complex number into a string using default format settings. |
| `ToString` | `function ToString(const AFormatSettings: TFormatSettings): string; overload;` | Formats the complex number into a string using custom culture format settings. |
| `IsZero` | `function IsZero: Boolean;` | Returns `True` if both real and imaginary components are zero. |
| `IsInfinite` | `function IsInfinite: Boolean;` | Returns `True` if either real or imaginary component is infinite. |
| `IsComplexInfinite` | `function IsComplexInfinite: Boolean;` | Returns `True` if both real and imaginary components are `NaN` (complex infinity sentinel). |
| `IsNaN` | `function IsNaN: Boolean;` | Returns `True` if exactly one component is `NaN`. |

---

## Basic & Polar Coordinate Functions

| Function | Signature | Description |
| --- | --- | --- |
| `Modulus` | `function Modulus: Double;` | Computes squared magnitude $|z|^2 = x^2 + y^2$. |
| `Phase` | `function Phase: Double;` | Computes polar angle $\theta = \arg(z) = \text{atan2}(y, x) \in (-\pi, \pi]$. |
| `Abs` | `class function Abs(const AValue: TComplex): Double; static;` | Computes complex magnitude $|z| = \sqrt{x^2 + y^2}$. |
| `AbsSqr` | `class function AbsSqr(const AValue: TComplex): Double; static;` | Alias for `Modulus` ($|z|^2$). |
| `Angle` | `class function Angle(const AValue: TComplex): Double; static;` | Alias for `Phase` ($\arg(z)$). |
| `Sign` | `class function Sign(const AValue: TComplex): TComplex; static;` | Computes normalized unit complex number $\frac{z}{|z|}$ along the same phase angle. |
| `Conjugate` | `class function Conjugate(const AValue: TComplex): TComplex; static;` | Computes complex conjugate $\overline{z} = x - iy$. |
| `Inverse` | `class function Inverse(const AValue: TComplex): TComplex; static;` | Computes multiplicative inverse $z^{-1} = \frac{\overline{z}}{|z|^2}$. Raises `EZeroDivide` if $z = 0$. |
| `Frac` | `class function Frac(const AValue: TComplex): Double; static;` | Returns fractional part of the real component (asserts `Imaginary = 0`). |
| `Int` | `class function Int(const AValue: TComplex): Double; static;` | Returns integer part of the real component (asserts `Imaginary = 0`). |
| `Compare` | `class function Compare(const Left, Right: TComplex): Integer; static;` | Returns `0` if equal, `-1` otherwise. |
| `Equals` | `class function Equals(const Left, Right: TComplex): Boolean; static;` | Evaluates equality `Left = Right`. |

---

## Power, Exponential & Logarithmic Functions

| Function | Signature | Description |
| --- | --- | --- |
| `Exp` | `class function Exp(const AValue: TComplex): TComplex; static;` | Evaluates complex exponential $e^z = e^x (\cos y + i \sin y)$. |
| `Ln` | `class function Ln(const AValue: TComplex): TComplex; static;` | Evaluates principal natural logarithm $\ln z = \ln |z| + i \arg(z)$. |
| `Log10` | `class function Log10(const AValue: TComplex): TComplex; static;` | Evaluates base-10 complex logarithm $\log_{10} z = \frac{\ln z}{\ln 10}$. |
| `Log2` | `class function Log2(const AValue: TComplex): TComplex; static;` | Evaluates base-2 complex logarithm $\log_2 z = \frac{\ln z}{\ln 2}$. |
| `LogN` | `class function LogN(const AValue: TComplex; const X: Double): TComplex; static;` | Evaluates base-$X$ complex logarithm $\log_X z = \frac{\ln z}{\ln X}$. |
| `Sqr` | `class function Sqr(const AValue: TComplex): TComplex; static;` | Computes square $z^2 = (x^2 - y^2) + 2i x y$. |
| `Sqrt` | `class function Sqrt(const AValue: TComplex): TComplex; static;` | Computes principal complex square root $\sqrt{z}$. |
| `Power` | `class function Power(const AValue, APower: TComplex): TComplex; static;` | Computes complex power $z^w = e^{w \ln z}$. |

---

## Trigonometric & Inverse Trigonometric Functions

| Function | Signature | Description |
| --- | --- | --- |
| `Cos` | `class function Cos(const AValue: TComplex): TComplex; static;` | Complex cosine: $\cos(x + iy) = \cos x \cosh y - i \sin x \sinh y$. |
| `Sin` | `class function Sin(const AValue: TComplex): TComplex; static;` | Complex sine: $\sin(x + iy) = \sin x \cosh y + i \cos x \sinh y$. |
| `Tan` | `class function Tan(const AValue: TComplex): TComplex; static;` | Complex tangent: $\frac{\sin z}{\cos z}$. |
| `Cot` | `class function Cot(const AValue: TComplex): TComplex; static;` | Complex cotangent: $\frac{\cos z}{\sin z}$. |
| `Csc` | `class function Csc(const AValue: TComplex): TComplex; static;` | Complex cosecant: $\frac{1}{\sin z}$. |
| `Sec` | `class function Sec(const AValue: TComplex): TComplex; static;` | Complex secant: $\frac{1}{\cos z}$. |
| `ArcCos` | `class function ArcCos(const AValue: TComplex): TComplex; static;` | Inverse complex cosine. |
| `ArcSin` | `class function ArcSin(const AValue: TComplex): TComplex; static;` | Inverse complex sine. |
| `ArcTan` | `class function ArcTan(const AValue: TComplex): TComplex; static;` | Inverse complex tangent. |
| `ArcCot` | `class function ArcCot(const AValue: TComplex): TComplex; static;` | Inverse complex cotangent. |
| `ArcCsc` | `class function ArcCsc(const AValue: TComplex): TComplex; static;` | Inverse complex cosecant. |
| `ArcSec` | `class function ArcSec(const AValue: TComplex): TComplex; static;` | Inverse complex secant. |

---

## Hyperbolic & Inverse Hyperbolic Functions

| Function | Signature | Description |
| --- | --- | --- |
| `CosH` | `class function CosH(const AValue: TComplex): TComplex; static;` | Complex hyperbolic cosine: $\cosh(x + iy) = \cosh x \cos y + i \sinh x \sin y$. |
| `SinH` | `class function SinH(const AValue: TComplex): TComplex; static;` | Complex hyperbolic sine: $\sinh(x + iy) = \sinh x \cos y + i \cosh x \sin y$. |
| `TanH` | `class function TanH(const AValue: TComplex): TComplex; static;` | Complex hyperbolic tangent: $\frac{\sinh z}{\cosh z}$. |
| `CotH` | `class function CotH(const AValue: TComplex): TComplex; static;` | Complex hyperbolic cotangent: $\frac{\cosh z}{\sinh z}$. |
| `CscH` | `class function CscH(const AValue: TComplex): TComplex; static;` | Complex hyperbolic cosecant: $\frac{1}{\sinh z}$. |
| `SecH` | `class function SecH(const AValue: TComplex): TComplex; static;` | Complex hyperbolic secant: $\frac{1}{\cosh z}$. |
| `ArcCosH` | `class function ArcCosH(const AValue: TComplex): TComplex; static;` | Inverse complex hyperbolic cosine. |
| `ArcSinH` | `class function ArcSinH(const AValue: TComplex): TComplex; static;` | Inverse complex hyperbolic sine. |
| `ArcTanH` | `class function ArcTanH(const AValue: TComplex): TComplex; static;` | Inverse complex hyperbolic tangent. |
| `ArcCotH` | `class function ArcCotH(const AValue: TComplex): TComplex; static;` | Inverse complex hyperbolic cotangent. |
| `ArcCscH` | `class function ArcCscH(const AValue: TComplex): TComplex; static;` | Inverse complex hyperbolic cosecant. |
| `ArcSecH` | `class function ArcSecH(const AValue: TComplex): TComplex; static;` | Inverse complex hyperbolic secant. |
