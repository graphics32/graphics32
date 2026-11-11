---
layout: doc
docType: api
unit: GR32
entity: Gray32
kind: Function
declaration: "function Gray32(Intensity: Byte; Alpha: Byte = $FF): TColor32;"
summary: "Constructs a 32-bit ARGB grayscale color with equal Red, Green, and Blue intensity and specified alpha transparency."
parameters:
  - name: Intensity
    type: Byte
    description: "Grayscale intensity value (0..255)."
  - name: Alpha
    type: Byte
    description: "Alpha channel transparency value (0..255, defaults to 255 for opaque)."
returns:
  - type: TColor32
    description: "Constructed 32-bit ARGB grayscale color."
seealso:
  - "[[TColor32]]"
  - "[[Color32]]"
---

## Description

`Gray32` constructs a 32-bit [[TColor32]] ARGB color with matching Red, Green, and Blue components ($R = G = B = \text{Intensity}$) and alpha transparency set to `Alpha` (defaulting to fully opaque, `$FF`).
