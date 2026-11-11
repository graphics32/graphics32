---
layout: doc
docType: api
unit: GR32_ColorGradients
entity: TColor32Gradient
kind: Class
summary: "Manages multi-stop color gradients with linear color stop interpolation and stream serialization."
declaration: "TColor32Gradient = class(TInterfacedPersistent, IStreamPersist)"
inheritance:
  - TObject
  - TPersistent
  - TInterfacedPersistent
  - TColor32Gradient
---

## Description

`TColor32Gradient` maintains an ordered collection of [[TColor32GradientStop]] entries along a normalized offset domain $[0.0, 1.0]$.

It calculates exact interpolated colors at arbitrary offsets (`GetColorAt`) and fills lookup tables (`FillColorLookUpTable`) for fast scanline rendering.

[members]
