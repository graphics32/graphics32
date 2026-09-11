---
layout: doc
docType: api
unit: GR32_Gamma
entity: GR32_Gamma
kind: Unit
summary: "Provides color gamma encoding, gamma decoding, precomputed 256-byte lookup tables, sRGB transfer function support, and gamma change notifications."
---

## Description

The `GR32_Gamma` unit manages color gamma transformations for Graphics32. It provides global 256-entry lookup tables (`GAMMA_ENCODING_TABLE` and `GAMMA_DECODING_TABLE`), routines for setting power-law or standard sRGB gamma curves, functions for applying gamma correction to colors, pixel arrays, and bitmaps, as well as a delegate registration mechanism for gamma change notifications.

---

## What Problem Gamma Correction Solves

Gamma correction addresses two fundamental phenomena in digital imaging: non-linear human visual perception and non-linear display hardware response.

### 1. Human Perception vs. Physical Light Intensity
Human vision does not perceive light intensity linearly. Our eyes are significantly more sensitive to subtle differences in dark shadow regions than in bright highlight regions (governed by the Weber-Fechner law and Stevens' power law).

If digital images stored pixel intensities using linear physical light energy (where a value of $128$ emits exactly half the physical photons of $255$), 8-bit quantization ($256$ levels per color channel) would allocate far too many values to bright highlights—where the human eye cannot distinguish fine variations—and far too few values to dark shadows. This causes noticeable posterization and shadow banding artifacts.

To optimize 8-bit color depth, images are **gamma-encoded** (compressed) into a perceptual space where 8-bit quantization steps are distributed more evenly according to human visual perception.

### 2. Display Hardware Response
Traditional cathode-ray tube (CRT) monitors possessed a physical voltage-to-luminance transfer function following a power law with exponent $\gamma \approx 2.2$:

$$L = V^\gamma$$

Modern LCD, OLED, and LED displays deliberately emulate this non-linear response curve for backward compatibility with digital image formats and human visual expectations.

### 3. Linear Light Calculations in Graphics
While gamma-encoded values are ideal for storage and display, **optical calculations must be computed in linear light space**. Performing linear mathematical operations—such as alpha blending, antialiasing coverage weighting, resampling, and spatial filtering—directly on gamma-encoded pixel values leads to physical errors:

* **Dark Antialiasing Bands**: Antialiased line and text edges rendered without gamma awareness appear prematurely dark or thin because intermediate alpha blends underestimate linear light luminance.
* **Inaccurate Color Blending**: Blending equal parts pure Red ($255, 0, 0$) and pure Green ($0, 255, 0$) directly in gamma space yields a dull olive-brown ($128, 128, 0$) instead of the physically correct bright yellow ($181, 181, 0$).
* **Resampling Artifacts**: Scaling images down directly in gamma space produces dark fringe artifacts around high-contrast edges.

By converting pixel values into linear light space before optical operations (or applying gamma bias compensation tables during antialiasing), Graphics32 ensures physically accurate rendering results.

---

## Linear Gamma vs. sRGB Gamma

`GR32_Gamma` supports two principal mathematical forms of gamma transformation: **Power-Law Gamma** and **sRGB Transfer Functions**.

```
                        Gamma Encoding Curves (Linear -> Gamma)
   1.0 +------------------------------------------------------------------+
       |                                                 . ' sRGB         |
       |                                            . '                   |
   0.8 |                                       . '     ------------------ |
       |                                  . '          Power Law (g=2.2)  |
   0.6 |                             . '                                  |
       |                        . '                                       |
   0.4 |                   . '                                            |
       |              . '                                                 |
   0.2 |         . '                                                      |
       |    . ' (linear slope)                                            |
   0.0 +------------------------------------------------------------------+
       0.0        0.2        0.4        0.6        0.8        1.0
                               Linear Light Input
```

### 1. Power-Law Gamma
Power-law gamma transformations use a pure exponential curve governed by a single gamma exponent $\gamma$:

* **Gamma Encoding (Linear Light $\to$ Gamma Space)**:
  $$V_{\text{encoded}} = V_{\text{linear}}^{1/\gamma}$$

* **Gamma Decoding (Gamma Space $\to$ Linear Light)**:
  $$V_{\text{linear}} = V_{\text{encoded}}^\gamma$$

In Graphics32, calling [[SetGamma]] computes 256-byte lookup tables based on a specified $\gamma$ parameter. The library uses a default gamma exponent `DEFAULT_GAMMA` ($1.6$), which is historically optimized as a antialiasing bias compensation factor for line and text rendering in Graphics32.

### 2. sRGB Gamma (IEC 61966-2-1)
The **sRGB** color space is the standard color profile for the Internet and digital devices. Unlike a simple power law, sRGB uses a **piecewise transfer function**:

1. A **linear segment** near zero ($V \le 0.0031308$) to avoid infinite slope and noise amplification in extreme dark shadows.
2. A **power-law segment** with exponent $2.4$ (and offset $1.055$) for $V > 0.0031308$.

#### sRGB Encoding Formula (Linear Light $L \to$ sRGB $V$)
$$V = \begin{cases} 12.92 \cdot L & \text{if } L \le 0.0031308 \\ 1.055 \cdot L^{1/2.4} - 0.055 & \text{if } L > 0.0031308 \end{cases}$$

#### sRGB Decoding Formula (sRGB $V \to$ Linear Light $L$)
$$L = \begin{cases} \frac{V}{12.92} & \text{if } V \le 0.004045 \\ \left(\frac{V + 0.055}{1.055}\right)^{2.4} & \text{if } V > 0.004045 \end{cases}$$

Calling [[Set_sRGB]] populates `GAMMA_ENCODING_TABLE` and `GAMMA_DECODING_TABLE` with exact 8-bit sRGB conversions and sets `GAMMA_IS_SRGB` to `True`.

---

## Fast 8-Bit Lookup Tables

Evaluating power or fractional exponent functions for every pixel in a large bitmap is computationally expensive. Because standard color channels in Graphics32 (`TColor32`) use 8-bit precision per channel ($0 \dots 255$), `GR32_Gamma` precomputes all gamma values into 256-entry lookup tables ([[TGammaTable8Bit]]).

Applying gamma correction via [[ApplyGamma]], [[ApplyInvGamma]], or [[ApplyCustomGamma]] performs direct $O(1)$ table lookups for Red, Green, and Blue channels, allowing high-performance batch image transformations.

[members]
