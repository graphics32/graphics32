---
layout: doc
docType: api
unit: GR32
entity: Intensity
kind: Function
declaration: "function Intensity(Color32: TColor32): Integer;"
summary: "Calculates the grayscale perceived luminance / intensity (0..255) of a TColor32 color."
parameters:
  - name: Color32
    type: TColor32
    description: "Source 32-bit ARGB color."
returns:
  - type: Integer
    description: "The calculated grayscale intensity value in range [0..255]."
---

## Description

`Intensity` calculates the perceived grayscale luminance intensity of a [[TColor32]] value, returning a byte value from `0` to `255`.

## Algorithm

The function uses integer fixed-point arithmetic based on the standard [Rec. 601 (ITU-R BT.601)](https://en.wikipedia.org/wiki/Rec._601) luma formula ($Y = 0.299R + 0.587G + 0.114B$):

$$\text{Intensity} = \frac{61R + 174G + 21B}{256}$$

The integer weights ($61/256 \approx 0.238$, $174/256 \approx 0.680$, $21/256 \approx 0.082$) approximate the human visual response to red, green, and blue light wavelengths while allowing fast bit-shift division (`shr 8`).
